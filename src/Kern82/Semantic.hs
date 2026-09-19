module Kern82.Semantic (analyze, Signatures(..)) where

import Control.Monad (foldM, unless, when)
import Data.Bits ((.&.), (.|.), xor, complement, shiftL, shiftR)
import qualified Data.Map.Strict as M
import Kern82.AST
import Kern82.Types (TypeTable, typeTable, structMembers)

data Signatures = Signatures
  { routines :: M.Map String Routine, definitions :: TypeTable, currentOwner :: Maybe TypeId }
type Scope = M.Map String Binding
type Env = [Scope]

analyze :: Program -> Result (Program, Signatures)
analyze (Program originalTops) = do
  tops <- mapM normalizeTop originalTops
  (globals, routineMap) <- foldM collect (M.empty,M.empty) tops
  let sigs = Signatures routineMap (typeTable (Program tops)) Nothing
  mapM_ requireDefinition (M.elems routineMap)
  case M.lookup "main" routineMap of
    Just r | routineResult r == Unit && null (routineParams r) -> pure ()
    Just r -> failure "semantic" (routinePos r) "entry point must be PROZEDUR main()"
    Nothing -> failure "semantic" (Pos 1 1) "missing PROZEDUR main() entry point"
  resolved <- mapM (resolveTop globals sigs) tops
  pure (Program resolved,sigs)
  where
    requireDefinition r = case routineBody r of
      Nothing -> failure "semantic" (routinePos r) ("routine has no definition: " ++ routineName r)
      Just _ -> pure ()
    collect (vars,sigs) top = case top of
      Generic _ r _ -> failure "semantic" (routinePos r) "unexpanded generic template"
      Instantiate p _ _ _ _ -> failure "semantic" p "unexpanded generic instantiation"
      TypeTop _ -> pure (vars,sigs)
      Global d -> do
        let b = declBinding d
        when (M.member (bindName b) vars || M.member (bindName b) sigs) $
          failure "semantic" (bindPos b) ("duplicate global name: " ++ bindName b)
        pure (M.insert (bindName b) b vars,sigs)
      Function r -> do
        when (M.member (routineName r) vars) $
          failure "semantic" (routinePos r) "routine name conflicts with a global variable"
        _ <- foldM insertBinding M.empty (routineParams r)
        case M.lookup (routineName r) sigs of
          Nothing -> pure (vars,M.insert (routineName r) r sigs)
          Just old -> do
            unless (signature old == signature r) $
              failure "semantic" (routinePos r) ("prototype mismatch for " ++ routineName r)
            case (routineBody old,routineBody r) of
              (Just _,Just _) -> failure "semantic" (routinePos r) "duplicate routine definition"
              (_,Just _) -> pure (vars,M.insert (routineName r) r sigs)
              _ -> pure (vars,sigs)
    signature r = (routineResult r,map bindType (routineParams r))

normalizeTop :: Top -> Result Top
normalizeTop (Global d) = Global <$> normalizeDecl d
normalizeTop (TypeTop (StructDef ident members)) = TypeTop . StructDef ident <$> mapM member members
  where
    member (Field access d) = Field access <$> normalizeDecl d
    member m = pure m
normalizeTop top = pure top

normalizeDecl :: Decl -> Result Decl
normalizeDecl d@(Decl _ _) = pure d
normalizeDecl (ArrayDecl b sizeExprs) = do
  counts <- mapM evalArrayLength sizeExprs
  let element = case bindType b of
        ArrayT t _ -> t
        _ -> error "internal: array declaration without array type"
  pure (ArrayDecl b {bindType = foldr (flip ArrayT) element counts} sizeExprs)
normalizeDecl (KetteDecl b sizeExpr ini) = do
  count <- case evalArrayLength sizeExpr of
    Left (Diagnostic phase p msg) -> failure phase p ("KETTE capacity: " ++ msg)
    Left issue -> Left issue
    Right n -> pure n
  when (bindConst b && ini == Nothing) $
    failure "semantic" (bindPos b) "KONST KETTE requires an explicit string literal initializer"
  case ini of
    Nothing -> pure ()
    Just e -> case exprNode e of
      StringLit bytes -> when (toInteger (length bytes) > count) $
        failure "semantic" (exprPos e) "KETTE initializer exceeds capacity"
      _ -> failure "semantic" (exprPos e) "KETTE initializer must be a string literal"
  pure (KetteDecl b {bindType = KetteT count} sizeExpr ini)

evalArrayLength :: Expr -> Result Integer
evalArrayLength expressionNode = do
  value <- evaluate expressionNode
  if value <= 0 then failure "semantic" (exprPos expressionNode) "array length must be positive"
  else if value > 2147483647 then failure "semantic" (exprPos expressionNode) "array length exceeds implementation limit"
  else pure value
  where
    invalid e = failure "semantic" (exprPos e) "array length must be an integer constant expression"
    evaluate e = case exprNode e of
      IntLit n -> pure n
      Unary "+" x -> evaluate x
      Unary "-" x -> negate <$> evaluate x
      Unary "~" x -> complement <$> evaluate x
      Binary op x y -> do
        a <- evaluate x
        b <- evaluate y
        case op of
          "+" -> pure (a+b); "-" -> pure (a-b); "*" -> pure (a*b)
          "/" -> if b == 0 then failure "semantic" (exprPos e) "division by zero in array length" else pure (a `quot` b)
          "%" -> if b == 0 then failure "semantic" (exprPos e) "division by zero in array length" else pure (a `rem` b)
          "&" -> pure (a .&. b); "|" -> pure (a .|. b); "^" -> pure (a `xor` b)
          "<<" -> shift e shiftL a b
          ">>" -> shift e shiftR a b
          _ -> invalid e
      _ -> invalid e
    shift e operation a b
      | b < 0 = failure "semantic" (exprPos e) "invalid shift in array length"
      | b > 63 = failure "semantic" (exprPos e) "array length constant shift exceeds implementation limit"
      | otherwise = pure (operation a (fromInteger b))

insertBinding :: Scope -> Binding -> Result Scope
insertBinding scope b
  | M.member (bindName b) scope = failure "semantic" (bindPos b) ("duplicate name in this scope: " ++ bindName b)
  | otherwise = Right (M.insert (bindName b) b scope)
lookupVar :: Env -> String -> Maybe Binding
lookupVar [] _ = Nothing
lookupVar (s:ss) n = case M.lookup n s of Just b -> Just b; Nothing -> lookupVar ss n

resolveTop :: Scope -> Signatures -> Top -> Result Top
resolveTop globals sigs top = case top of
  Generic _ r _ -> failure "semantic" (routinePos r) "unexpanded generic template"
  Instantiate p _ _ _ _ -> failure "semantic" p "unexpanded generic instantiation"
  TypeTop _ -> pure top
  Global (Decl b ini) -> Global . Decl b <$> traverse (resolveExpr [globals] sigs) ini
  Global d@(ArrayDecl _ _) -> pure (Global d)
  Global d@(KetteDecl _ _ _) -> pure (Global d)
  Function r -> inRoutineContext r $ case routineBody r of
    Nothing -> pure top
    Just (Stmt p (Block ss)) -> do
      params <- foldM insertBinding M.empty (routineParams r)
      (body,_) <- resolveList [params,globals] sigs {currentOwner=routineOwner r} ss
      pure (Function r { routineBody = Just (Stmt p (Block body)) })
    Just body -> do
      (body',_) <- resolveStmt [globals] sigs {currentOwner=routineOwner r} body
      pure (Function r { routineBody = Just body' })

resolveList :: Env -> Signatures -> [Stmt] -> Result ([Stmt],Env)
resolveList env _ [] = pure ([],env)
resolveList env sigs (s:ss) = do
  (s',env') <- resolveStmt env sigs s
  (ss',end) <- resolveList env' sigs ss
  pure (s':ss',end)

resolveStmt :: Env -> Signatures -> Stmt -> Result (Stmt,Env)
resolveStmt env sigs (Stmt p node) = do
  let ex = resolveExpr env sigs
      sub s = fst <$> resolveStmt env sigs s
      same n = pure (Stmt p n,env)
  case node of
    Block ss -> do (ss',_) <- resolveList (M.empty:env) sigs ss; same (Block ss')
    Declaration rawDecl -> do
      normalized <- normalizeDecl rawDecl
      let b = declBinding normalized
      -- Resolve the initializer before introducing the binding: no accidental self-read.
      finalDecl <- case normalized of
        Decl _ ini -> Decl b <$> traverse ex ini
        ArrayDecl _ lengthExpr -> pure (ArrayDecl b lengthExpr)
        KetteDecl _ capacity ini -> pure (KetteDecl b capacity ini)
      case env of
        scope:outer -> do scope' <- insertBinding scope b; pure (Stmt p (Declaration finalDecl),scope':outer)
        [] -> failure "semantic" p "internal empty scope"
    Expression e -> ex e >>= same . Expression
    If c a b -> do c' <- ex c; a' <- sub a; b' <- traverse sub b; same (If c' a' b')
    While c b -> do c' <- ex c; b' <- sub b; same (While c' b')
    For ini c step b -> do
      let inner = M.empty:env
      (ini',loopEnv) <- case ini of
        Nothing -> pure (Nothing,inner)
        Just s -> do (s',env') <- resolveStmt inner sigs s; pure (Just s',env')
      c' <- traverse (resolveExpr loopEnv sigs) c
      step' <- traverse (resolveExpr loopEnv sigs) step
      (b',_) <- resolveStmt loopEnv sigs b
      same (For ini' c' step' b')
    Return e -> traverse ex e >>= same . Return
    Switch value arms -> do
      value' <- ex value
      arms' <- mapM (\arm -> do
        label <- traverse ex (armValue arm)
        (body,_) <- resolveList (M.empty:env) sigs (armBody arm)
        pure arm {armValue = label, armBody = body}) arms
      same (Switch value' arms')
    _ -> same node

resolveExpr :: Env -> Signatures -> Expr -> Result Expr
resolveExpr env sigs e = do
  let go = resolveExpr env sigs
      owner = currentOwner sigs
      fields = maybe [] (structMembers (definitions sigs) . StructT) owner
      local = lookupVar (if owner == Nothing then env else take (length env - 1) env)
      global = lookupVar (drop (length env - 1) env)
      field name = [declBinding d | Field _ d <- fields, bindName (declBinding d) == name]
      receiver ident = expression (exprPos e) (Receiver ident)
      value name = case local name of
        Just b -> Just (Bound b)
        Nothing -> case (owner,field name) of
          (Just ident,b:_) -> Just (FieldAccess (receiver ident) b)
          _ -> Bound <$> global name
  n <- case exprNode e of
    Variable name -> case value name of
      Just node -> pure node
      Nothing -> failure "semantic" (exprPos e) ("unknown variable: " ++ name)
    Call name args -> case local name of
      Just b -> indirectOrError name (Bound b) args
      Nothing -> case (owner,field name) of
        (_,b:_) -> indirectOrError name (FieldAccess (receiver (case owner of Just ident -> ident; Nothing -> error "internal field without owner")) b) args
        (Just ident,[]) | M.member (methodName ident name) (routines sigs) ->
          MemberCall (receiver ident) name <$> mapM go args
        _ -> case global name of
          Just b -> indirectOrError name (Bound b) args
          Nothing -> do
            unless (name `elem` ["AUSGABE","SCHREIBEN","LESEN"] || M.member name (routines sigs)) $
              failure "semantic" (exprPos e) ("unknown routine: " ++ name)
            Call name <$> mapM go args
    Member base name -> Member <$> go base <*> pure name
    MemberCall base name args -> MemberCall <$> go base <*> pure name <*> mapM go args
    Cast t x -> Cast t <$> go x
    AddressOf x -> case exprNode x of
      Variable name | value name == Nothing, Just r <- M.lookup name (routines sigs), routineOwner r == Nothing ->
        pure (RoutineAddress name)
      _ -> AddressOf <$> go x
    Dereference x -> Dereference <$> go x
    ReferenceBind x -> ReferenceBind <$> go x
    Allocate x -> Allocate <$> go x
    Free x -> Free <$> go x
    IndirectCall target args -> IndirectCall <$> go target <*> mapM go args
    ReferenceResult referent x -> ReferenceResult referent <$> go x
    Unary op x -> Unary op <$> go x
    Binary op x y -> Binary op <$> go x <*> go y
    Assign x y -> Assign <$> go x <*> go y
    Increment op pre x -> Increment op pre <$> go x
    ArrayAccess array index -> ArrayAccess <$> go array <*> go index
    other -> pure other
  pure e { exprNode = n }
  where
    indirectOrError name node args
      | isFunctionPointer (valueType node) = IndirectCall (expression (exprPos e) node) <$> mapM (resolveExpr env sigs) args
      | otherwise = failure "semantic" (exprPos e) ("variable is not callable: " ++ name)
    valueType node = case node of
      Bound b -> bindType b
      FieldAccess _ b -> bindType b
      _ -> Unit
