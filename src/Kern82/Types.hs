module Kern82.Types
  ( elaborateTypes, TypeTable, typeTable, structMembers, copyable, copyFailure, addressable ) where

import Control.Monad (foldM, when, unless)
import qualified Data.Map.Strict as M
import Kern82.AST
import Kern82.Generics
import Kern82.Modules (cloneRoutine)
import Kern82.ConstEval (ordinalConstant, integerConstant)

type TypeTable = M.Map TypeId TypeDecl
-- A value name masks a type name, just as it masks a routine name.
data NameEntry = TypeEntry Type | ValueEntry (Maybe Integer) | GenericEntry deriving Eq
type TypeScope = [M.Map String NameEntry]

typeTable :: Program -> TypeTable
typeTable (Program tops) = M.fromList [(typeDeclId d,d) | TypeTop d <- tops]

structMembers :: TypeTable -> Type -> [MemberDecl]
structMembers table ty = case representation ty of
  StructT ident -> case M.lookup ident table of Just (StructDef _ ms) -> ms; _ -> []
  _ -> []

copyable :: TypeTable -> Type -> Bool
copyable table ty = case representation ty of
  ArrayT _ _ -> False
  KetteT _ -> False
  StructT ident -> case M.lookup ident table of
    Just (StructDef _ ms) -> all fieldCopyable ms
    _ -> False
  TypeName _ -> False
  TextT -> False
  Unit -> False
  _ -> True
  where
    fieldCopyable (Method _ _) = False
    fieldCopyable (Field _ d) = copyable table (bindType (declBinding d))

-- Report the immediate cause; recursive copyability still checks every field.
copyFailure :: TypeTable -> Type -> String
copyFailure table ty = case representation ty of
  StructT ident ->
    let prefix = "STRUCT " ++ typeIdName ident ++ " is not copyable"
        members = structMembers table ty
    in if any isMethod members then prefix ++ " because it declares member routines"
       else case [declBinding d | Field _ d <- members, not (copyable table (bindType (declBinding d)))] of
         b:_ -> prefix ++ ": field '" ++ bindName b ++ "' has non-copyable type " ++ typeName (bindType b)
         [] -> prefix
  _ -> "type " ++ typeName ty ++ " is not copyable"
  where
    isMethod (Method _ _) = True
    isMethod _ = False

addressable :: Expr -> Bool
addressable e = case exprNode e of
  Bound _ -> True
  Receiver _ -> True
  Dereference _ -> True
  FieldAccess base _ -> addressable base
  ArrayAccess base _ -> addressable base
  ReferenceResult _ _ -> True
  _ -> False

lookupType :: TypeScope -> String -> Maybe Type
lookupType [] _ = Nothing
lookupType (s:ss) name = case M.lookup name s of
  Just (TypeEntry ty) -> Just ty
  Just (ValueEntry _) -> Nothing
  Just GenericEntry -> Nothing
  Nothing -> lookupType ss name

resolveType :: TypeScope -> Pos -> Type -> Result Type
resolveType scopes p ty = case ty of
  TypeName name -> case lookupType scopes name of
    Just t -> pure t
    Nothing -> failure "semantic" p ("unknown or not yet defined type: " ++ name)
  ArrayT t n -> ArrayT <$> resolveType scopes p t <*> pure n
  PointerT t -> PointerT <$> resolveType scopes p t
  ReferenceT t -> ReferenceT <$> resolveType scopes p t
  ConstT t -> ConstT <$> resolveType scopes p t
  FunctionPointerT result parameters -> FunctionPointerT
    <$> resolveType scopes p result <*> mapM (resolveType scopes p) parameters
  _ -> pure ty

introduce :: TypeScope -> Pos -> String -> Maybe Type -> Result TypeScope
introduce (s:ss) p name value = do
  let oldType = case M.lookup name s of Just (TypeEntry _) -> True; _ -> False
  when (M.member name s && (value /= Nothing || oldType)) $
    failure "semantic" p ("duplicate type or value name in this scope: " ++ name)
  pure (M.insert name (maybe (ValueEntry Nothing) TypeEntry value) s:ss)
introduce [] p _ _ = failure "semantic" p "internal empty type scope"

ordinalExpression :: TypeScope -> Expr -> Result Expr
ordinalExpression env e = do
  let go = ordinalExpression env
      lookupValue [] _ = Nothing
      lookupValue (s:ss) name = case M.lookup name s of
        Just (ValueEntry value) -> value
        Just (TypeEntry _) -> Nothing
        Just GenericEntry -> Nothing
        Nothing -> lookupValue ss name
  node <- case exprNode e of
    Variable name -> case lookupValue env name of
      Just n -> pure (IntLit n)
      Nothing -> failure "semantic" (exprPos e) "ENUM value must be an integer constant expression"
    Unary op x -> Unary op <$> go x
    Binary op x y -> Binary op <$> go x <*> go y
    Cast ty x -> Cast ty <$> go x
    other -> pure other
  pure e {exprNode=node}

introduceDeclaration :: TypeScope -> Decl -> Result TypeScope
introduceDeclaration env d = do
  let b = declBinding d
  next <- introduce env (bindPos b) (bindName b) Nothing
  let constant = case d of
        Decl _ (Just e) | bindConst b && bindType b `elem` [Ganz,Lang] && literalOperations e ->
          case ordinalExpression env e >>= integerConstant (bindType b) of Right n -> Just n; Left _ -> Nothing
        _ -> Nothing
  case next of
    scope:outer -> pure (M.insert (bindName b) (ValueEntry constant) scope:outer)
    [] -> pure next
  where
    literalOperations e = case exprNode e of
      IntLit _ -> True
      Variable _ -> True
      Unary _ x -> literalOperations x
      Binary _ x y -> literalOperations x && literalOperations y
      _ -> False

elaborateTypes :: Program -> Result Program
elaborateTypes (Program tops) = do
  validateGenericNames tops
  (items,_) <- topList [M.empty] M.empty M.empty tops
  let result = Program items
      table = typeTable result
  mapM_ (acyclic table []) [StructT i | TypeTop (StructDef i _) <- items]
  pure result
  where
    topList env _ _ [] = pure ([],env)
    topList env templates cache (top:rest) = do
      (items,next,newTemplates,newCache) <- case top of
        Generic parameters r source -> do
          let entry = (parameters,r,source,env)
              newEnv = case env of
                scope:outer -> M.insert (routineName r) GenericEntry scope:outer
                [] -> env
          pure ([],newEnv,M.insert (routineName r) entry templates,cache)
        Instantiate p name declared template arguments -> do
          (parameters,definition,source,captured) <- maybe
            (failure "semantic" p ("unknown or not yet defined generic: " ++ template)) pure
            (M.lookup template templates)
          concrete <- mapM (resolveType env p) arguments
          result <- resolveType env p declared
          checkConstraints p template parameters concrete
          let key = (routineName definition,routinePos definition,concrete)
              origin = GenericOrigin template concrete source
              parameterTypes = M.fromList [(parameter,TypeEntry ty)
                | (GenericParam _ parameter _,ty) <- zip parameters concrete]
          (generated,defs,updated) <- case M.lookup key cache of
            Just cached -> pure ((aliasRoutine p name cached) {routineOrigin=Just origin},[],cache)
            Nothing -> do
              cloned <- cloneRoutine name definition
              let contextual = cloned {routineOrigin=Just origin}
              (expanded,extra) <- inRoutineContext contextual (routine (parameterTypes:captured) contextual)
              let contextualExtra item = case item of
                    Function memberRoutine -> Function memberRoutine {routineOrigin=Just origin}
                    other -> other
              pure (expanded,map contextualExtra extra,M.insert key expanded cache)
          unless (routineResult generated == result) $
            failure "type" p ("NEU result type mismatch: declared " ++ typeName result ++
              ", specialization returns " ++ typeName (routineResult generated))
          e <- introduce env p name Nothing
          pure (defs ++ [Function generated],e,templates,updated)
        TypeTop d -> do
          (defs,values,e) <- define env d
          pure (defs ++ map Global values,e,templates,cache)
        Global d -> do
          d' <- declaration env d
          e <- introduceDeclaration env d'
          pure ([Global d'],e,templates,cache)
        Function r -> do
          e <- introduce env (routinePos r) (routineName r) Nothing
          (r',defs) <- routine e r
          pure (defs ++ [Function r'],e,templates,cache)
      (more,end) <- topList next newTemplates newCache rest
      pure (items ++ more,end)

    define env def = do
      let ident@(TypeId p name) = typeDeclId def
      case def of
        AliasDef _ base -> do
          base' <- resolveType env p base
          when (containsReference base') $
            failure "semantic" p "REFERENZ cannot be declared through TYP"
          let ty = NominalT ident base'
          next <- introduce env p name (Just ty)
          pure ([TypeTop (AliasDef ident base')],[],next)
        EnumDef _ members -> do
          when (null members) $ failure "semantic" p "ENUM requires at least one member"
          next <- introduce env p name (Just (EnumT ident))
          (_,values,end) <- foldM (enumerator ident) (0,[],next) members
          pure ([TypeTop def],values,end)
        StructDef _ members -> do
          next <- introduce env p name (Just (StructT ident))
          -- Every member is visible throughout the body of every method.
          _ <- foldM uniqueMember M.empty members
          let memberNames = M.fromList [(case m of
                Field _ d -> bindName (declBinding d)
                Method _ r -> routineName r, ValueEntry Nothing) | m <- members]
          resolved <- mapM (\m -> member (case m of Method _ _ -> memberNames:next; _ -> next) ident m) members
          let ms = map fst resolved
              extra = concatMap snd resolved
          pure (TypeTop (StructDef ident ms):extra,[],next)

    enumerator ident (previous,values,env) (Enumerator p name explicit) = do
      value <- maybe (pure previous) (\e -> ordinalExpression env e >>= ordinalConstant) explicit
      when (value < -2147483648 || value > 2147483647) $
        failure "semantic" p "ENUM ordinal is outside signed 32-bit range"
      next <- introduce env p name Nothing
      -- Unlike ordinary declarations, repeated enumerator names are forbidden here.
      when (any ((== name) . bindName . declBinding) values || localValueExists env name) $
        failure "semantic" p ("duplicate enumerator or value name: " ++ name)
      let ty = EnumT ident
          d = Decl (Binding p name ty True) (Just (expression p (EnumLit ty value)))
      pure (value+1,values ++ [d],next)
    localValueExists (s:_) name = M.member name s
    localValueExists [] _ = False

    uniqueMember names m = do
      let (p,name,isMethod,access) = case m of
            Field a d -> let b = declBinding d in (bindPos b,bindName b,False,a)
            Method a r -> (routinePos r,routineName r,True,a)
      case M.lookup name names of
        Just (old,oldAccess) | not (old && isMethod) -> failure "semantic" p ("duplicate STRUCT member: " ++ name)
                            | oldAccess /= access -> failure "semantic" p "member prototype access level mismatch"
        _ -> pure ()
      pure (M.insert name (isMethod,access) names)
    member env ident (Method access r) = do
      (r',defs) <- routine env r {routineName = methodName ident (routineName r),routineOwner = Just ident}
      pure (Method access r' {routineBody=Nothing},defs ++ [Function r'])
    member env _ (Field access d) = do
      d' <- declaration env d
      when (containsStoredReference (bindType (declBinding d'))) $
        failure "semantic" (bindPos (declBinding d')) "REFERENZ is not supported as a STRUCT field or array element"
      case d' of
        Decl b ini -> do
          when (ini /= Nothing) $ failure "semantic" (bindPos b) "STRUCT field initializers are not supported"
          when (bindConst b) $ failure "semantic" (bindPos b) "KONST STRUCT fields are not supported"
        KetteDecl b _ ini -> when (ini /= Nothing || bindConst b) $
          failure "semantic" (bindPos b) "STRUCT KETTE fields use empty default initialization"
        ArrayDecl b _ -> when (bindConst b) $
          failure "semantic" (bindPos b) "KONST STRUCT fields are not supported"
      pure (Field access d',[])

    declaration env d = do
      let b = declBinding d
      ty <- resolveType env (bindPos b) (bindType b)
      let b' = b {bindType=ty}
      case d of
        Decl _ ini -> Decl b' <$> traverse (ex env) ini
        ArrayDecl _ count -> pure (ArrayDecl b' count)
        KetteDecl _ count ini -> pure (KetteDecl b' count ini)
    routine env r = do
      result <- resolveType env (routinePos r) (routineResult r)
      params <- mapM (\b -> do ty <- resolveType env (bindPos b) (bindType b); pure b {bindType=ty}) (routineParams r)
      inner <- foldM (\e b -> introduce e (bindPos b) (bindName b) Nothing) (M.empty:env) params
      (body,defs) <- case routineBody r of
        Nothing -> pure (Nothing,[])
        Just (Stmt p (Block ss)) -> do
          (ss',_,extra) <- statements inner ss
          pure (Just (Stmt p (Block ss')),extra)
        Just s -> do (s',extra) <- statement inner s; pure (Just s',extra)
      pure (r {routineResult=result,routineParams=params,routineBody=body},defs)

    statements env [] = pure ([],env,[])
    statements env (Stmt p (TypeStatement d):ss) = do
      (defs,values,next) <- define env d
      (rest,end,extra) <- statements next ss
      pure (map (Stmt p . Declaration) values ++ rest,end,defs ++ extra)
    statements env (Stmt p (Declaration d):ss) = do
      d' <- declaration env d
      next <- introduceDeclaration env d'
      (rest,end,extra) <- statements next ss
      pure (Stmt p (Declaration d'):rest,end,extra)
    statements env (s:ss) = do
      (s',defs) <- statement env s
      (rest,end,extra) <- statements env ss
      pure (s':rest,end,defs ++ extra)

    statement env (Stmt p node) = do
      let simple n = pure (Stmt p n,[])
      case node of
        Block ss -> do (ss',_,defs) <- statements (M.empty:env) ss; pure (Stmt p (Block ss'),defs)
        Expression e -> ex env e >>= simple . Expression
        Return e -> traverse (ex env) e >>= simple . Return
        If c a b -> do
          c' <- ex env c; (a',da) <- statement env a
          (b',db) <- case b of Nothing -> pure (Nothing,[]); Just s -> do (s',ds) <- statement env s; pure (Just s',ds)
          pure (Stmt p (If c' a' b'),da ++ db)
        While c b -> do c' <- ex env c; (b',defs) <- statement env b; pure (Stmt p (While c' b'),defs)
        For ini c step b -> do
          (initial,inner,di) <- statements (M.empty:env) (maybe [] (:[]) ini)
          c' <- traverse (ex inner) c; step' <- traverse (ex inner) step
          (b',db) <- statement inner b
          pure (Stmt p (For (case initial of [] -> Nothing; x:_ -> Just x) c' step' b'),di ++ db)
        Switch value arms -> do
          value' <- ex env value
          result <- mapM (\arm -> do
            label <- traverse (ex env) (armValue arm)
            (body,_,defs) <- statements (M.empty:env) (armBody arm)
            pure (arm {armValue=label,armBody=body},defs)) arms
          pure (Stmt p (Switch value' (map fst result)),concatMap snd result)
        _ -> simple node

    ex env e = do
      let go = ex env
          generic [] _ = False
          generic (scope:outer) name = case M.lookup name scope of
            Just GenericEntry -> True
            Just _ -> False
            Nothing -> generic outer name
      case exprNode e of
        Call name _ | generic env name -> failure "semantic" (exprPos e)
          ("generic routine requires explicit NEU instantiation: " ++ name)
        AddressOf value | Variable name <- exprNode value, generic env name ->
          failure "semantic" (exprPos e) ("cannot take ADDRESSE of generic template: " ++ name)
        Variable name | generic env name -> failure "semantic" (exprPos e)
          ("generic template is not a value: " ++ name)
        _ -> pure ()
      node <- case exprNode e of
        Call name args -> case lookupType env name of
          Just ty -> case args of
            [a] -> Cast ty <$> go a
            _ -> failure "semantic" (exprPos e) "type conversion requires one argument"
          Nothing -> Call name <$> mapM go args
        Cast ty x -> Cast <$> resolveType env (exprPos e) ty <*> go x
        AddressOf x -> AddressOf <$> go x
        Dereference x -> Dereference <$> go x
        ReferenceBind x -> ReferenceBind <$> go x
        Allocate x -> Allocate <$> go x
        Free x -> Free <$> go x
        IndirectCall target args -> IndirectCall <$> go target <*> mapM go args
        ReferenceResult referent x -> ReferenceResult referent <$> go x
        Member x name -> Member <$> go x <*> pure name
        MemberCall x name args -> MemberCall <$> go x <*> pure name <*> mapM go args
        Unary op x -> Unary op <$> go x
        Binary op x y -> Binary op <$> go x <*> go y
        Assign x y -> Assign <$> go x <*> go y
        Increment op pre x -> Increment op pre <$> go x
        ArrayAccess x y -> ArrayAccess <$> go x <*> go y
        other -> pure other
      pure e {exprNode=node}

acyclic :: TypeTable -> [TypeId] -> Type -> Result ()
acyclic table seen ty = case representation ty of
  ArrayT element _ -> acyclic table seen element
  StructT ident@(TypeId p _) -> do
    when (ident `elem` seen) $ failure "semantic" p "recursive-by-value STRUCT has infinite size"
    mapM_ (acyclic table (ident:seen) . bindType . declBinding) [d | Field _ d <- structMembers table ty]
  _ -> pure ()

containsStoredReference :: Type -> Bool
containsStoredReference ty = case ty of
  ReferenceT _ -> True
  PointerT t -> containsStoredReference t
  ArrayT t _ -> containsStoredReference t
  ConstT t -> containsStoredReference t
  NominalT _ t -> containsStoredReference t
  FunctionPointerT _ _ -> False
  _ -> False
