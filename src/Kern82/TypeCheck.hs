module Kern82.TypeCheck (checkProgram) where

import Control.Monad (unless, when)
import qualified Data.Map.Strict as M
import Kern82.AST
import Kern82.Semantic (Signatures(..))
import Kern82.Types (copyable, copyFailure, structMembers, addressable)
import Kern82.ConstEval (caseConstant)
import Kern82.Flow (returns)

type Constants = M.Map String Expr
data FallContext = OutsideCase | InCase Bool Bool

checkProgram :: Signatures -> Program -> Result Program
checkProgram sigs (Program ts) = Program <$> mapM top ts
  where
    constants = M.fromList [(bindingC b,e) | Decl b (Just e) <- programDecls (Program ts), bindConst b]
    top t@(TypeTop _) = pure t
    top (Global d) = do
      when (containsStoredReference (bindType (declBinding d))) $
        failure "type" (bindPos (declBinding d)) "REFERENZ is supported only for local variables and routine parameters"
      case d of
        Decl b (Just e) -> unless (constantExpr e) $
          failure "type" (bindPos b) "global initializer must be a literal constant expression"
        _ -> pure ()
      Global <$> checkDecl sigs d
    top (Generic _ r _) = failure "type" (routinePos r) "unexpanded generic template"
    top (Instantiate p _ _ _ _) = failure "type" p "unexpanded generic instantiation"
    top (Function r) = inRoutineContext r $ do
      let context = sigs {currentOwner=routineOwner r}
      case representation (routineResult r) of
        ReferenceT referent -> when (containsReference referent) $
          failure "type" (routinePos r) "nested REFERENZ return types are not supported"
        _ -> when (containsStoredReference (routineResult r)) $
          failure "type" (routinePos r) "nested REFERENZ return types are not supported"
      mapM_ (\b -> when (case representation (bindType b) of
          ReferenceT referent -> containsReference referent
          _ -> containsStoredReference (bindType b)) $
        failure "type" (bindPos b) "nested REFERENZ parameter types are not supported") (routineParams r)
      mapM_ (\t -> unless (t == Unit || isReference t || copyable (definitions sigs) t) $
        failure "type" (routinePos r) ("non-copyable type cannot be a routine parameter or return value: " ++
          copyFailure (definitions sigs) t))
        (routineResult r : map bindType (routineParams r))
      body <- traverse (checkStmt context constants (routineResult r) 0 OutsideCase) (routineBody r)
      case body of
        Just b | routineResult r /= Unit && not (returns b) ->
          failure "type" (routinePos r) "function may finish without RÜCKGABE"
        _ -> pure ()
      pure (Function r {routineBody = body})

constantExpr :: Expr -> Bool
constantExpr e = case exprNode e of
  IntLit _ -> True; FloatLit _ -> True; BoolLit _ -> True; CharLit _ -> True
  EnumLit _ _ -> True
  NullLit -> True
  Unary _ x -> constantExpr x
  Binary _ x y -> constantExpr x && constantExpr y
  Cast _ x -> constantExpr x
  _ -> False

checkDecl :: Signatures -> Decl -> Result Decl
checkDecl sigs (Decl b ini) = do
  case representation (bindType b) of
    ReferenceT referent -> do
      when (containsReference referent) $
        failure "type" (bindPos b) "nested REFERENZ types are not supported"
      when (bindConst b) $ failure "type" (bindPos b) "KONST REFERENZ is not supported"
      value <- maybe (failure "type" (bindPos b) "REFERENZ requires an initializer") pure ini
      bound <- bindReference sigs (bindPos b) referent value
      pure (Decl b (Just bound))
    _ -> ordinary
  where
    ordinary = do
      when (containsStoredReference (bindType b)) $
        failure "type" (bindPos b) "nested REFERENZ types and arrays of REFERENZ are not supported"
      when (bindConst b && isStruct (bindType b)) $
        failure "semantic" (bindPos b) "KONST STRUCT is not supported"
      when (ini /= Nothing && not (copyable (definitions sigs) (bindType b))) $
        failure "type" (bindPos b) (copyFailure (definitions sigs) (bindType b))
      when (bindConst b && ini == Nothing) $ failure "type" (bindPos b) "KONST requires an initializer"
      ini' <- traverse (checkExpr sigs (Just (bindType b))) ini
      pure (Decl b ini')
checkDecl _ d@(ArrayDecl b _)
  | containsStoredReference (bindType b) = failure "type" (bindPos b) "arrays of REFERENZ are not supported"
  | otherwise = pure d
checkDecl sigs (KetteDecl b capacity ini) =
  KetteDecl b capacity <$> traverse (checkExpr sigs (Just TextT)) ini

checkStmt :: Signatures -> Constants -> Type -> Int -> FallContext -> Stmt -> Result Stmt
checkStmt sigs constants result loops fallContext (Stmt p node) = Stmt p <$> case node of
  Block ss -> Block <$> checkList sigs constants result loops fallContext ss
  Declaration d -> Declaration <$> checkDecl sigs d
  Expression e -> do
    checked <- checkExpr sigs Nothing e
    when (isArray (checkedType checked)) $
      failure "type" (exprPos checked) "whole-array expressions are not supported; index an element"
    when (isKette (checkedType checked)) $
      failure "type" (exprPos checked) "KETTE expressions are permitted only in output or indexed access"
    pure (Expression checked)
  If c a b -> If <$> bool c <*> sub a <*> traverse sub b
  While c b -> While <$> bool c <*> checkStmt sigs constants result (loops+1) (nonFinal fallContext) b
  For ini c step b -> For <$> traverse sub ini <*> traverse bool c
    <*> traverse checkStep step <*> checkStmt sigs constants result (loops+1) (nonFinal fallContext) b
  Switch value arms -> do
    value' <- checkExpr sigs Nothing value
    let ty = checkedType value'
    unless (representation ty `elem` [Ganz,Lang,CharT] || isEnum ty) $
      failure "type" (exprPos value) "SWITCH requires GANZ, LANG, or CHAR, or ENUM"
    arms' <- sequence [checkArm ty (i+1 < length arms) a | (i,a) <- zip [0..] arms]
    let labels = [(armPos a,exprNode e) | a <- arms', Just e <- [armValue a]]
    checkDuplicates labels
    case [armPos a | a <- arms', armValue a == Nothing] of
      _:duplicate:_ -> failure "type" duplicate "duplicate DEFAULT in SWITCH"
      _ -> pure ()
    pure (Switch value' arms')
  Fallthrough -> case fallContext of
    OutsideCase -> failure "type" p "FALLTHROUGH outside CASE or DEFAULT"
    InCase _ False -> failure "type" p "FALLTHROUGH must be the last executable statement of its branch"
    InCase False _ -> failure "type" p "FALLTHROUGH has no following CASE or DEFAULT"
    InCase True True -> pure Fallthrough
  Return value -> case (result,value) of
    (Unit,Nothing) -> pure (Return Nothing)
    (Unit,Just _) -> failure "type" p "a procedure cannot return a value"
    (_,Nothing) -> failure "type" p "a function must return a value"
    (ReferenceT referent,Just e) -> Return . Just <$> bindReference sigs p referent e
    (_,Just e) -> Return . Just <$> checkExpr sigs (Just result) e
  Break -> if loops > 0 then pure Break else failure "type" p "ABBRUCH outside a loop"
  Continue -> if loops > 0 then pure Continue else failure "type" p "WEITER outside a loop"
  Empty -> pure Empty
  TypeStatement _ -> failure "type" p "internal unexpanded type declaration"
  where
    sub = checkStmt sigs constants result loops fallContext
    bool = checkExpr sigs (Just BoolT)
    checkStep e = do
      checked <- checkExpr sigs Nothing e
      when (isArray (checkedType checked) || isKette (checkedType checked)) $
        failure "type" (exprPos e) "aggregate value is not a FOR step expression"
      pure checked
    checkArm ty hasNext arm = do
      label <- traverse (\e -> do
        checked <- checkExpr sigs (Just ty) e
        n <- caseConstant resolveConstant checked
        pure checked {exprNode = if ty == CharT then CharLit (fromInteger n) else IntLit n}) (armValue arm)
      body <- checkList sigs constants result loops (InCase hasNext True) (armBody arm)
      pure arm {armValue = label, armBody = body}
    resolveConstant b = case M.lookup (bindingC b) constants of
      Just e -> checkExpr sigs (Just (bindType b)) e
      Nothing -> failure "type" (bindPos b) "CASE constant has no compile-time initializer"
    checkDuplicates [] = pure ()
    checkDuplicates ((_,n):xs) = case [pos | (pos,m) <- xs, n == m] of
      pos:_ -> failure "type" pos "duplicate CASE value in SWITCH"
      [] -> checkDuplicates xs

nonFinal :: FallContext -> FallContext
nonFinal OutsideCase = OutsideCase
nonFinal (InCase next _) = InCase next False

checkList :: Signatures -> Constants -> Type -> Int -> FallContext -> [Stmt] -> Result [Stmt]
checkList _ _ _ _ _ [] = pure []
checkList sigs constants result loops context (s:ss) = do
  let empty (Stmt _ Empty) = True
      empty _ = False
      here = if all empty ss then context else nonFinal context
  s' <- checkStmt sigs constants result loops here s
  ss' <- checkList sigs constants result loops context ss
  pure (s':ss')

checkExpr :: Signatures -> Maybe Type -> Expr -> Result Expr
checkExpr sigs wanted e = do
  result <- infer
  case wanted of
    Just t | checkedType result /= t ->
      if pointerConstConversion t (checkedType result)
      then pure result {exprType=Just t}
      else err ("expected " ++ typeName t ++ ", got " ++ typeName (checkedType result) ++
        if typeName t == typeName (checkedType result) then " (distinct nominal declarations)" else "")
    _ -> pure result
  where
    err = failure "type" (exprPos e)
    finish t n = pure (typed t e {exprNode=n})
    check = checkExpr sigs
    scalar t = representation t `elem` [Ganz,Lang,Gleit,CharT]
    literalType primitive = case wanted of
      Just ty | representation ty == primitive -> ty
      _ -> primitive
    intLiteral n = do
      let t = case wanted of
            Just ty | isNumeric ty -> ty
            _ -> if n >= -32768 && n <= 32767 then Ganz else Lang
      unless (fits t n) $ err ("integer literal is outside " ++ typeName t ++ " range")
      finish t (IntLit n)
    infer = case exprNode e of
      IntLit n -> intLiteral n
      FloatLit n -> finish (literalType Gleit) (FloatLit n)
      BoolLit b -> finish (literalType BoolT) (BoolLit b)
      CharLit c -> finish (literalType CharT) (CharLit c)
      StringLit s -> finish TextT (StringLit s)
      EnumLit ty n -> finish ty (EnumLit ty n)
      NullLit -> case wanted of
        Just ty | isPointer ty -> finish ty NullLit
        _ -> err "NULL requires a pointer type context"
      RoutineAddress name -> case M.lookup name (routines sigs) of
        Just r | routineOwner r == Nothing -> finish
          (FunctionPointerT (routineResult r) (map bindType (routineParams r))) (RoutineAddress name)
        _ -> err "member routine pointers are not supported"
      AddressOf x -> do
        a <- check Nothing x
        unless (addressable a) $ err "ADDRESSE requires addressable mutable storage"
        when (isArray (checkedType a) || isKette (checkedType a)) $
          err "ADDRESSE requires an object or final array element; aggregate array/KETTE addresses are not supported"
        let target = checkedType a
        case wanted of
          Just (PointerT (ConstT expected)) | expected == target ->
            finish (PointerT (ConstT expected)) (AddressOf a)
          Just (PointerT expected) | expected == target ->
            writable a >> finish (PointerT expected) (AddressOf a)
          Just expected | isDataPointer expected ->
            err ("expected " ++ typeName expected ++ ", got ZEIGER " ++ typeName target)
          _ -> writable a >> finish (PointerT target) (AddressOf a)
      Dereference x -> do
        a <- check Nothing x
        case representation (checkedType a) of
          PointerT ty -> finish (unqualified ty) (Dereference a)
          _ -> err "INHALT requires a ZEIGER value"
      ReferenceBind _ -> err "internal reference binding outside a declaration or call"
      ReferenceResult _ _ -> err "internal checked reference result"
      Allocate count -> case wanted of
        Just ty | PointerT target <- representation ty -> do
          when (containsReference target) $ err "ALLOKIEREN cannot allocate REFERENZ elements"
          n <- check Nothing count
          unless (checkedType n `elem` [Ganz,Lang]) $ err "ALLOKIEREN count must be GANZ or LANG"
          finish ty (Allocate n)
        _ -> err "ALLOKIEREN requires an expected ZEIGER type"
      Free x -> do
        a <- case exprNode x of
          NullLit -> check (Just (PointerT Ganz)) x
          _ -> check Nothing x
        unless (isDataPointer (checkedType a)) $ err "FREIGEBEN requires a data ZEIGER value or NULL"
        finish Unit (Free a)
      Bound b -> case representation (bindType b) of
        ReferenceT referent -> finish (unqualified referent) (Bound b)
        _ -> finish (bindType b) (Bound b)
      Receiver ident -> finish (StructT ident) (Receiver ident)
      Member base name -> do
        obj <- check Nothing base
        case [(access,declBinding d) | Field access d <- structMembers (definitions sigs) (checkedType obj), bindName (declBinding d) == name] of
          (access,b):_ -> accessible obj access >> finish (bindType b) (FieldAccess obj b)
          [] -> err ("unknown STRUCT field: " ++ name)
      FieldAccess base b -> do
        obj <- check Nothing base
        finish (bindType b) (FieldAccess obj b)
      MemberCall base name args -> do
        obj <- check Nothing base
        case [(access,declBinding d) | Field access d <- structMembers (definitions sigs) (checkedType obj), bindName (declBinding d) == name] of
          (access,b):_ | isFunctionPointer (bindType b) -> do
            accessible obj access
            let target = typed (bindType b) (expression (exprPos e) (FieldAccess obj b))
            indirect target args
          _ -> do
            ident <- case representation (checkedType obj) of
              StructT i -> pure i
              _ -> err "member call requires STRUCT"
            let key = methodName ident name
            case [(access,r) | Method access r <- structMembers (definitions sigs) (checkedType obj), routineName r == key] of
              (access,r):_ -> do
                accessible obj access
                writable obj
                as <- arguments r args
                routineCall (MethodCall obj key as) r
              [] -> err ("unknown STRUCT member routine: " ++ name)
      MethodCall _ _ _ -> err "internal already checked member call"
      IndirectCall target args -> do
        callable <- check Nothing target
        indirect callable args
      ArrayAccess array index -> do
        base <- check Nothing array
        element <- case checkedType base of
          ArrayT ty _ -> pure ty
          KetteT _ -> pure CharT
          other -> err ("indexing requires an array, got " ++ typeName other)
        idx <- check Nothing index
        unless (checkedType idx `elem` [Ganz,Lang]) $
          failure "type" (exprPos index) (if isKette (checkedType base)
            then "KETTE index must be GANZ or LANG" else "array index must be GANZ or LANG")
        finish element (ArrayAccess base idx)
      Variable _ -> err "internal unresolved variable"
      Assign lhs rhs -> do
        a <- check Nothing lhs
        when (isArray (checkedType a)) $ err "whole-array assignment is not supported"
        when (isKette (checkedType a)) $ err "whole-KETTE assignment is not permitted"
        unless (copyable (definitions sigs) (checkedType a)) $
          err (copyFailure (definitions sigs) (checkedType a))
        writable a
        b <- check (Just (checkedType a)) rhs
        finish (checkedType a) (Assign a b)
      Increment op pre x -> do
        a <- check Nothing x; writable a
        unless (isInteger (checkedType a) || isDataPointer (checkedType a)) $
          err (if isFunctionPointer (checkedType a)
            then "function pointer increment is not supported"
            else "increment requires GANZ, LANG, or a data ZEIGER")
        finish (checkedType a) (Increment op pre a)
      Unary "-" x | IntLit n <- exprNode x -> intLiteral (-n)
      Unary op x | op `elem` ["!","NOT"] -> do
        a <- check (case wanted of Just ty | representation ty == BoolT -> Just ty; _ -> Nothing) x
        unless (representation (checkedType a) == BoolT) $ err "expected BOOL"
        finish (checkedType a) (Unary op a)
      Unary op x -> do
        a <- check (numericContext wanted) x
        unless (if op == "~" then isInteger (checkedType a) else isNumeric (checkedType a)) $
          err ("invalid operand for " ++ op)
        finish (checkedType a) (Unary op a)
      Binary op x y | op `elem` ["&&","AND","||","OR","XOR"] -> do
        (a,b) <- pair (case wanted of Just ty | representation ty == BoolT -> Just ty; _ -> Nothing) x y
        unless (representation (checkedType a) == BoolT) $ err "expected BOOL"
        finish (checkedType a) (Binary op a b)
      Binary "+" x y -> do
        a <- check Nothing x
        b <- check Nothing y
        when (isFunctionPointer (checkedType a) || isFunctionPointer (checkedType b)) $
          err "function pointer arithmetic is not supported"
        case (isDataPointer (checkedType a),isDataPointer (checkedType b)) of
          (True,False) | checkedType b `elem` [Ganz,Lang] -> finish (checkedType a) (Binary "+" a b)
          (False,True) | checkedType a `elem` [Ganz,Lang] -> finish (checkedType b) (Binary "+" a b)
          (False,False) -> ordinaryArithmetic "+" x y
          _ -> err "pointer arithmetic requires one ZEIGER and one exact GANZ or LANG operand"
      Binary "-" x y -> do
        a <- check Nothing x
        b <- check Nothing y
        when (isFunctionPointer (checkedType a) || isFunctionPointer (checkedType b)) $
          err "function pointer arithmetic is not supported"
        case (isDataPointer (checkedType a),isDataPointer (checkedType b)) of
          (True,False) | checkedType b `elem` [Ganz,Lang] -> finish (checkedType a) (Binary "-" a b)
          (True,True) | checkedType a == checkedType b -> finish Lang (Binary "-" a b)
          (True,True) -> err "pointer subtraction requires identical ZEIGER types"
          (False,False) -> ordinaryArithmetic "-" x y
          _ -> err "pointer arithmetic requires ZEIGER - exact GANZ or LANG"
      Binary op x y -> do
        let compareOp = op `elem` ["==","!=","<","<=",">",">="]
            hint = if compareOp then Nothing else numericContext wanted
        (a,b) <- pair hint x y
        let t = checkedType a
        unless (t == checkedType b) $ err "mixed operand types require an explicit cast"
        when (isArray t) $ err "whole-array comparison is not supported"
        when (isKette t) $ err "KETTE comparison and concatenation are not permitted"
        when (isPointer t) $ err "pointer arithmetic and comparison are not supported"
        if compareOp then do
          unless (isNumeric t || representation t == CharT || ((representation t == BoolT || isEnum t) && op `elem` ["==","!="])) $
            err "invalid comparison operands"
          finish BoolT (Binary op a b)
        else do
          unless (if op `elem` ["%","&","|","^","<<",">>"] then isInteger t else isNumeric t) $
            err ("invalid arithmetic operands for " ++ op)
          finish t (Binary op a b)
      Cast t x -> do
        a <- check Nothing x
        when (isPointer t || isPointer (checkedType a)) $
          err "pointer casts are not supported"
        when (isStruct t && representation t == representation (checkedType a) &&
              not (copyable (definitions sigs) t)) $ err (copyFailure (definitions sigs) t)
        let sameAggregate = representation t == representation (checkedType a) &&
              (isEnum t || isStruct t) && copyable (definitions sigs) t
            enumOrdinal = isEnum (checkedType a) && t `elem` [Ganz,Lang]
        unless (sameAggregate || enumOrdinal || (scalar t && scalar (checkedType a))) $
          err "casts are allowed only among GANZ, LANG, GLEIT, CHAR or compatible aggregate types; ENUM converts only to GANZ or LANG; BOOL is separate"
        finish t (Cast t a)
      Call name args | name `elem` ["AUSGABE","SCHREIBEN"] -> do
        as <- mapM (check Nothing) args
        when (any ((== Unit) . checkedType) as) $ err "cannot print a procedure result"
        when (any (isArray . checkedType) as) $ err "whole arrays cannot be printed; index an element"
        when (any (isPointer . checkedType) as) $ err "pointer values cannot be printed directly; use INHALT"
        when (any (\a -> isStruct (checkedType a) || isEnum (checkedType a)) as) $
          err "STRUCT and ENUM values cannot be printed directly"
        finish Unit (Call name as)
      Call "LESEN" args -> do
        when (null args) $ err "LESEN requires at least one variable"
        as <- mapM (check Nothing) args
        mapM_ writable as
        when (any (isPointer . checkedType) as) $ err "LESEN does not accept pointer values; use INHALT"
        when (any (\a -> isStruct (checkedType a) || isEnum (checkedType a)) as) $
          err "LESEN does not accept STRUCT or ENUM values"
        finish BoolT (Call "LESEN" as)
      Call name args -> case M.lookup name (routines sigs) of
        Nothing -> err ("unknown routine: " ++ name)
        Just r -> do
          as <- arguments r args
          routineCall (Call name as) r
    routineCall node r = case representation (routineResult r) of
      ReferenceT referent -> do
        let raw = typed (routineResult r) (expression (exprPos e) node)
        finish (unqualified referent) (ReferenceResult referent raw)
      _ -> finish (routineResult r) node
    indirect callable args = case representation (checkedType callable) of
      FunctionPointerT result parameters -> do
        unless (length args == length parameters) $ err "wrong argument count for function pointer"
        as <- sequence (zipWith argumentType parameters args)
        let raw = typed result (expression (exprPos e) (IndirectCall callable as))
        case representation result of
          ReferenceT referent -> finish (unqualified referent) (ReferenceResult referent raw)
          _ -> finish result (IndirectCall callable as)
      _ -> err "expression is not a function or procedure pointer"
    arguments r args = do
      unless (length args == length (routineParams r)) $ err ("wrong argument count for " ++ routineName r)
      sequence (zipWith argument (routineParams r) args)
    argument b a = case representation (bindType b) of
      ReferenceT referent -> bindReference sigs (exprPos a) referent a
      _ -> check (Just (bindType b)) a
    argumentType parameter a = case representation parameter of
      ReferenceT referent -> bindReference sigs (exprPos a) referent a
      _ -> check (Just parameter) a
    accessible obj access = case representation (checkedType obj) of
      StructT ident -> when (access == Private && currentOwner sigs /= Just ident) $ err "PRIVATE member access is forbidden"
      _ -> err "field access requires STRUCT"
    pair hint x y
      | flexible x && not (flexible y) = do
          b <- check hint y; a <- check (Just (checkedType b)) x; pure (a,b)
      | otherwise = do
          a <- check hint x; b <- check (Just (checkedType a)) y; pure (a,b)
    ordinaryArithmetic op x y = do
      (a,b) <- pair (numericContext wanted) x y
      unless (checkedType a == checkedType b) $ err "mixed operand types require an explicit cast"
      let t = checkedType a
      unless (isNumeric t) $ err ("invalid arithmetic operands for " ++ op)
      finish t (Binary op a b)

bindReference :: Signatures -> Pos -> Type -> Expr -> Result Expr
bindReference sigs p referent source = do
  checked <- checkExpr sigs Nothing source
  let target = unqualified referent
  unless (checkedType checked == target) $
    failure "type" p ("expected REFERENZ " ++ typeName referent ++ ", got " ++ typeName (checkedType checked))
  unless (addressable checked) $
    failure "type" p (if isConstType referent
      then "REFERENZ initializer must be addressable storage"
      else "REFERENZ initializer must be mutable addressable storage")
  when (isArray target || isKette target) $
    failure "type" p "REFERENZ cannot bind an aggregate array or KETTE value"
  unless (isConstType referent) (writable checked)
  pure (typed (ReferenceT referent) (expression p (ReferenceBind checked)))

numericContext :: Maybe Type -> Maybe Type
numericContext (Just t) | isNumeric t = Just t
numericContext _ = Nothing
flexible :: Expr -> Bool
flexible e = case exprNode e of
  IntLit _ -> True
  Unary op x | op `elem` ["+","-"] -> flexible x
  Binary op x y | op `elem` ["+","-","*","/","%","&","|","^","<<",">>"] -> flexible x && flexible y
  _ -> False
fits :: Type -> Integer -> Bool
fits (NominalT _ t) n = fits t n
fits Ganz n = n >= -32768 && n <= 32767
fits Lang n = n >= -2147483648 && n <= 2147483647
fits Gleit n = abs n <= 16777216
fits _ _ = False
writable :: Expr -> Result ()
writable e | isArray (checkedType e) = failure "type" (exprPos e) "whole-array use is not supported; index an element"
writable e = case exprNode e of
  Bound b | bindConst b -> immutable
          | ReferenceT referent <- representation (bindType b), isConstType referent -> readOnly
          | otherwise -> pure ()
  ArrayAccess base _ -> mutableRoot base
  FieldAccess base b -> do
    when (bindConst b) $ failure "type" (exprPos e) " ADDRESSE requires mutable storage"
    mutableRoot base
  Receiver _ -> pure ()
  Dereference pointer | constPointee (checkedType pointer) -> readOnly
                      | otherwise -> pure ()
  ReferenceResult referent _ | isConstType referent -> readOnly
                             | otherwise -> pure ()
  _ -> failure "type" (exprPos e) "assignment/input/increment requires a mutable variable"
  where
    immutable = failure "type" (exprPos e) " ADDRESSE requires mutable storage"
    readOnly = failure "type" (exprPos e) "cannot modify read-only storage"

mutableRoot :: Expr -> Result ()
mutableRoot e = case exprNode e of
  Bound b | bindConst b -> immutable
          | ReferenceT referent <- representation (bindType b), isConstType referent -> readOnly
          | otherwise -> pure ()
  Receiver _ -> pure ()
  Dereference pointer -> when (constPointee (checkedType pointer)) $
    readOnly
  ReferenceResult referent _ -> when (isConstType referent) $
    readOnly
  ArrayAccess base _ -> mutableRoot base
  FieldAccess base b -> do
    when (bindConst b) $ failure "type" (exprPos e) " ADDRESSE requires mutable storage"
    mutableRoot base
  _ | not (addressable e) -> failure "type" (exprPos e) "assignment/input/increment requires a mutable variable"
    | otherwise -> pure ()
  where
    immutable = failure "type" (exprPos e) " ADDRESSE requires mutable storage"
    readOnly = failure "type" (exprPos e) "cannot modify read-only storage"

isArray :: Type -> Bool
isArray (ArrayT _ _) = True
isArray _ = False

isKette :: Type -> Bool
isKette (KetteT _) = True
isKette _ = False

constPointee :: Type -> Bool
constPointee ty = case representation ty of
  PointerT pointee -> isConstType pointee
  _ -> False

pointerConstConversion :: Type -> Type -> Bool
pointerConstConversion expected actual = case (representation expected,representation actual) of
  (PointerT (ConstT target),PointerT source) -> target == unqualified source
  _ -> False

containsStoredReference :: Type -> Bool
containsStoredReference ty = case ty of
  ReferenceT _ -> True
  PointerT t -> containsStoredReference t
  ArrayT t _ -> containsStoredReference t
  ConstT t -> containsStoredReference t
  NominalT _ t -> containsStoredReference t
  FunctionPointerT _ _ -> False
  _ -> False
