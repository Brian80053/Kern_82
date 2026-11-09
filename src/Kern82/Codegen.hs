module Kern82.Codegen (generateC) where

import Control.Monad (ap)
import Data.List (intercalate, nub)
import Numeric (showOct)
import Kern82.AST
import Kern82.Runtime (runtime)
import Kern82.Types (addressable)

newtype Gen a = Gen { runGen :: Int -> (a,Int) }
instance Functor Gen where fmap f g = Gen $ \n -> let (a,m) = runGen g n in (f a,m)
instance Applicative Gen where pure a = Gen $ \n -> (a,n); (<*>) = ap
instance Monad Gen where g >>= f = Gen $ \n -> let (a,m) = runGen g n in runGen (f a) m
fresh :: Gen String
fresh = Gen $ \n -> ("k_t_" ++ show n,n+1)
indent :: [String] -> [String]
indent = map ("    " ++)
braces :: [String] -> [String]
braces xs = ["{"] ++ indent xs ++ ["}"]
ctype :: Type -> String
ctype t = case t of
  Ganz -> "int16_t"; Lang -> "int32_t"; BoolT -> "bool"; Gleit -> "float"
  CharT -> "uint8_t"; TextT -> "const uint8_t *"; Unit -> "void"
  ArrayT _ _ -> error "internal: array is not a C scalar type"
  KetteT capacity -> "k_kette_" ++ show capacity
  PointerT _ -> "k_pointer"
  ReferenceT _ -> "k_pointer"
  ConstT value -> ctype value
  FunctionPointerT _ _ -> "k_function_pointer"
  NominalT _ base -> ctype base
  EnumT _ -> "int32_t"
  StructT ident -> typeC ident
  TypeName _ -> error "internal: unresolved type name"
comma :: [String] -> String
comma = intercalate ", "
position :: Expr -> String
position e = show (line (exprPos e)) ++ ", " ++ show (column (exprPos e))
bounds :: Type -> String
bounds Ganz = "-32768LL, 32767LL"
bounds Lang = "(-2147483647LL - 1), 2147483647LL"
bounds CharT = "0, 255"
bounds (NominalT _ base) = bounds base
bounds _ = error "internal: bounds for non-integral type"
checked :: Type -> Expr -> String -> String
checked t e value
  | representation t == Gleit = "k_float(" ++ value ++ ", " ++ position e ++ ")"
  | otherwise = "(" ++ ctype t ++ ")k_range(" ++ value ++ ", " ++ bounds t ++ ", " ++ position e ++ ")"

-- Every value read is captured in a temporary. This specifies left-to-right
-- evaluation and avoids C's unsequenced side effects in f(i++, i++), etc.
temporary :: Type -> [String] -> String -> Gen ([String],String)
temporary Unit prefix value = pure (prefix ++ [value ++ ";"], "")
temporary t prefix value = do
  name <- fresh
  pure (prefix ++ [ctype t ++ " " ++ name ++ " = " ++ value ++ ";"],name)
bindingLife :: Binding -> String
bindingLife b = "k_life_" ++ drop 4 (bindingC b)

genStorage :: Expr -> Gen ([String],String,String)
genStorage e = case exprNode e of
  Bound b -> case representation (bindType b) of
    ReferenceT _ -> pure (["k_check_pointer(" ++ bindingC b ++ ", " ++ position e ++ ");"],
      "(*(" ++ ctype (checkedType e) ++ " *)k_pointer_address(" ++ bindingC b ++ ", " ++ position e ++ "))",
      bindingC b ++ ".life")
    _ -> pure ([],bindingC b,bindingLife b)
  Receiver _ -> pure (["k_check_pointer(k_self, " ++ position e ++ ");"],
    "(*(" ++ ctype (checkedType e) ++ " *)k_pointer_address(k_self, " ++ position e ++ "))","k_self.life")
  Dereference pointer -> do
    (pre,value) <- genExpr pointer
    pure (pre ++ ["k_check_pointer(" ++ value ++ ", " ++ position e ++ ");"],
      "(*(" ++ ctype (checkedType e) ++ " *)k_pointer_address(" ++ value ++ ", " ++ position e ++ "))",
      value ++ ".life")
  ReferenceResult _ call -> do
    (pre,value) <- genExpr call
    pure (pre ++ ["k_check_pointer(" ++ value ++ ", " ++ position e ++ ");"],
      "(*( " ++ ctype (checkedType e) ++ " *)k_pointer_address(" ++ value ++ ", " ++ position e ++ "))",
      value ++ ".life")
  FieldAccess base b -> do
    (pre,target,life) <- genStorage base
    pure (pre,target ++ "." ++ bindingC b,life)
  ArrayAccess array index -> do
    (basePre,target,life) <- genStorage array
    (pre,value) <- genExpr index
    case checkedType array of
      ArrayT _ count -> do
        ix <- fresh
        pure (basePre ++ pre ++ ["size_t " ++ ix ++ " = k_index((int64_t)" ++ value ++ ", " ++
          show count ++ "LL, " ++ position e ++ ");"],target ++ "[" ++ ix ++ "]",life)
      KetteT _ -> do
        ix <- fresh
        pure (basePre ++ pre ++ ["size_t " ++ ix ++ " = k_kette_index((int64_t)" ++ value ++ ", " ++
          target ++ ".length, " ++ position e ++ ");"],target ++ ".data[" ++ ix ++ "]",life)
      _ -> error "internal: invalid array lvalue after type checking"
  _ -> error "internal: invalid lvalue after type checking"

genLValue :: Expr -> Gen ([String],String)
genLValue e = do
  (pre,target,_) <- genStorage e
  pure (pre,target)

genAddress :: Expr -> Gen ([String],String)
genAddress e = case exprNode e of
  Dereference pointer -> do
    (pre,value) <- genExpr pointer
    pure (pre,"k_pointer_checked(" ++ value ++ ", " ++ position e ++ ")")
  Bound b | ReferenceT _ <- representation (bindType b) ->
    pure ([],"k_pointer_checked(" ++ bindingC b ++ ", " ++ position e ++ ")")
  ReferenceResult _ call -> do
    (pre,value) <- genExpr call
    pure (pre,"k_pointer_checked(" ++ value ++ ", " ++ position e ++ ")")
  ArrayAccess array index -> do
    (basePre,target,life) <- genStorage array
    (indexPre,value) <- genExpr index
    ix <- fresh
    case checkedType array of
      ArrayT element count -> pure (basePre ++ indexPre ++
        ["size_t " ++ ix ++ " = k_index((int64_t)" ++ value ++ ", " ++ show count ++ "LL, " ++ position e ++ ");"],
        "k_pointer_make((void *)&(" ++ target ++ "[0]), " ++ show count ++ ", " ++ ix ++ ", sizeof(" ++ ctype element ++ "), " ++ life ++ ", true)")
      KetteT _ -> pure (basePre ++ indexPre ++
        ["size_t " ++ ix ++ " = k_kette_index((int64_t)" ++ value ++ ", " ++ target ++ ".length, " ++ position e ++ ");"],
        "k_pointer_make((void *)" ++ target ++ ".data, " ++ target ++ ".length, " ++ ix ++ ", sizeof(uint8_t), " ++ life ++ ", false)")
      _ -> error "internal: address of invalid indexed storage"
  _ -> do
    (pre,target,life) <- genStorage e
    pure (pre,"k_pointer_make((void *)&(" ++ target ++ "), 1, 0, sizeof(" ++ ctype (checkedType e) ++ "), " ++ life ++ ", true)")

-- Capture the receiver and index once, validate before RHS, then validate and
-- store/append atomically with respect to the source language after RHS effects.
genKetteStore :: Expr -> Expr -> Gen ([String],String) -> Gen ([String],String)
genKetteStore whole array rhs = do
  (pre,store) <- genKetteWrite whole array
  (rightPre,value) <- rhs
  temporary CharT (pre ++ rightPre) (store value)

genKetteWrite :: Expr -> Expr -> Gen ([String],String -> String)
genKetteWrite whole array = case exprNode whole of
  ArrayAccess _ index -> do
    (basePre,target) <- genLValue array
    pointer <- fresh
    (indexPre,ix) <- genExpr index
    let capacity = case checkedType array of KetteT n -> show n; _ -> error "internal KETTE store"
        pointerDecl = ctype (checkedType array) ++ " *" ++ pointer ++ " = &(" ++ target ++ ");"
        check = "(void)k_kette_write_index((int64_t)" ++ ix ++ ", " ++ pointer ++ "->length, " ++ capacity ++ ", " ++ position whole ++ ");"
    pure (basePre ++ [pointerDecl] ++ indexPre ++ [check],
      \value -> "k_kette_store(" ++ pointer ++ "->data, &" ++ pointer ++ "->length, " ++ capacity ++
       ", (int64_t)" ++ ix ++ ", " ++ value ++ ", " ++ position whole ++ ")")
  _ -> error "internal: KETTE store requires an index"

genExpr :: Expr -> Gen ([String],String)
genExpr e = case exprNode e of
  IntLit n -> temp [] (if representation t == Gleit then show n ++ ".0f" else "(" ++ ctype t ++ ")(" ++ show n ++ "LL)")
  FloatLit n -> temp [] (show n ++ "f")
  BoolLit b -> temp [] (if b then "true" else "false")
  CharLit c -> temp [] (show c)
  StringLit s -> temp [] (cString s)
  EnumLit _ n -> temp [] ("(int32_t)(" ++ show n ++ "LL)")
  NullLit -> temp [] (if isFunctionPointer t then "k_null_function_pointer()" else "k_null_pointer()")
  RoutineAddress name -> temp [] ("k_function_pointer_make((void (*)(void))" ++ routineC name ++ ")")
  AddressOf x -> do
    (pre,value) <- genAddress x
    temp pre value
  ReferenceBind x -> do
    (pre,value) <- genAddress x
    temp pre value
  Allocate count -> do
    (pre,value) <- genExpr count
    pointer <- fresh
    raw <- fresh
    ix <- fresh
    let pointee = case representation t of
          PointerT ty -> ty
          _ -> error "internal: allocation without pointer target"
        element = ctype pointee
    pure (pre ++
      [ "k_pointer " ++ pointer ++ " = k_allocate((int64_t)" ++ value ++ ", sizeof(" ++ element ++ "), " ++ position e ++ ");"
      , element ++ " *" ++ raw ++ " = (" ++ element ++ " *)" ++ pointer ++ ".base;"
      , "for (size_t " ++ ix ++ " = 0; " ++ ix ++ " < " ++ pointer ++ ".count; ++" ++ ix ++ ") " ++ raw ++ "[" ++ ix ++ "] = (" ++ element ++ "){0};"
      ],pointer)
  Free pointer -> do
    (pre,value) <- genExpr pointer
    temp pre ("k_free_pointer(" ++ value ++ ", " ++ position e ++ ")")
  Dereference _ -> do
    (pre,target) <- genLValue e
    temp pre target
  ReferenceResult _ _ -> do
    (pre,target) <- genLValue e
    temp pre target
  Bound b -> case bindType b of
    ArrayT _ _ -> error "internal: whole array reached C expression backend"
    ty | isReference ty -> do
      (pre,target) <- genLValue e
      temp pre target
    _ -> temp [] (bindingC b)
  FieldAccess base b | not (addressable base) -> do
    (pre,value) <- genExpr base
    temp pre (value ++ "." ++ bindingC b)
  FieldAccess _ _ -> do
    (pre,target) <- genLValue e
    temp pre target
  Receiver _ -> temp [] "(*k_self)"
  ArrayAccess _ _ -> do
    (pre,target) <- genLValue e
    temp pre target
  Assign lhs rhs | ArrayAccess base _ <- exprNode lhs, KetteT _ <- checkedType base ->
    genKetteStore lhs base (genExpr rhs)
  Assign lhs rhs -> do
    (leftPre,target) <- genLValue lhs
    (rightPre,value) <- genExpr rhs
    temp (leftPre ++ rightPre ++ [target ++ " = " ++ value ++ ";"]) value
  Increment op prefix x -> do
    (leftPre,target) <- genLValue x
    (before,old) <- temporary t leftPre target
    let pointerDelta = if op == "++" then "1" else "-1"
        delta = if op == "++" then "+ 1LL" else "- 1LL"
        update = target ++ " = " ++ if isDataPointer t
          then "k_pointer_add(" ++ old ++ ", " ++ pointerDelta ++ ", " ++ position e ++ ");"
          else checked t e ("(int64_t)" ++ old ++ delta) ++ ";"
    temp (before ++ [update]) (if prefix then target else old)
  Cast target x -> do
    (pre,value) <- genExpr x
    let conversion
          | isStruct target || isEnum target = value
          | representation target == Gleit = "(float)" ++ value
          | representation (checkedType x) == Gleit = "(" ++ ctype target ++ ")k_from_float(" ++ value ++ ", " ++ bounds target ++ ", " ++ position e ++ ")"
          | otherwise = checked target e ("(int64_t)" ++ value)
    temp pre conversion
  Unary op x -> do
    (pre,v) <- genExpr x
    let value = case op of
          "!" -> "!" ++ v
          "NOT" -> "!" ++ v
          "+" -> v
          "-" -> checked t e ((if representation t == Gleit then "-" else "-(int64_t)") ++ v)
          "~" -> "(" ++ ctype t ++ ")k_bits(~(uint32_t)" ++ v ++ ", " ++ width t ++ ")"
          _ -> error "internal: unary operator"
    temp pre value
  Binary op x y | op `elem` ["&&","AND","||","OR"] -> do
    (a,v) <- genExpr x
    name <- fresh
    (b,w) <- genExpr y
    let condition = if op `elem` ["&&","AND"] then name else "!" ++ name
    pure (a ++ ["bool " ++ name ++ " = " ++ v ++ ";", "if (" ++ condition ++ ")"] ++
      braces (b ++ [name ++ " = " ++ w ++ ";"]),name)
  Binary op x y -> do
    (a,v) <- genExpr x; (b,w) <- genExpr y
    let operandType = checkedType x
        value
          | op == "+" && isDataPointer (checkedType x) = "k_pointer_add(" ++ v ++ ", (int64_t)" ++ w ++ ", " ++ position e ++ ")"
          | op == "+" && isDataPointer (checkedType y) = "k_pointer_add(" ++ w ++ ", (int64_t)" ++ v ++ ", " ++ position e ++ ")"
          | op == "-" && isDataPointer (checkedType x) && isDataPointer (checkedType y) = "(int32_t)k_pointer_subtract(" ++ v ++ ", " ++ w ++ ", " ++ position e ++ ")"
          | op == "-" && isDataPointer (checkedType x) = "k_pointer_add(" ++ v ++ ", -(int64_t)" ++ w ++ ", " ++ position e ++ ")"
          | op == "XOR" = v ++ " != " ++ w
          | op `elem` ["==","!=","<","<=",">",">="] = v ++ " " ++ op ++ " " ++ w
          | op `elem` ["&","|","^"] = "(" ++ ctype t ++ ")k_bits((uint32_t)" ++ v ++ " " ++ op ++ " (uint32_t)" ++ w ++ ", " ++ width t ++ ")"
          | op `elem` ["<<",">>"] = checked t e ("k_shift(" ++ v ++ ", " ++ w ++ ", " ++ width t ++ ", " ++ (if op == "<<" then "true" else "false") ++ ", " ++ position e ++ ")")
          | op `elem` ["/","%"] && isInteger operandType = checked t e ("k_div(" ++ v ++ ", " ++ w ++ ", " ++ (if op == "%" then "true" else "false") ++ ", " ++ position e ++ ")")
          | representation t == Gleit && op == "/" = "k_fdiv(" ++ v ++ ", " ++ w ++ ", " ++ position e ++ ")"
          | representation t == Gleit = checked t e (v ++ " " ++ op ++ " " ++ w)
          | otherwise = checked t e ("(int64_t)" ++ v ++ " " ++ op ++ " (int64_t)" ++ w)
    temp (a++b) value
  Call "LESEN" args -> do
    result <- fresh
    inputOperations <- mapM (genReadArg result) args
    let readIfSuccessful statements = ["if (" ++ result ++ ")"] ++ braces statements
    pure (["bool " ++ result ++ " = true;"] ++ concatMap readIfSuccessful inputOperations, result)
  Call name args | name `elem` ["AUSGABE","SCHREIBEN"] -> do
    values <- mapM genExpr args
    let prefix = concatMap fst values
        prints = zipWith printArg args (map snd values)
    pure (prefix ++ prints ++ ["putchar('\\n');" | name == "AUSGABE"], "")
  Call name args -> do
    values <- mapM genExpr args
    temp (concatMap fst values) (routineC name ++ "(" ++ comma (map snd values) ++ ")")
  IndirectCall target args -> do
    (targetPre,callee) <- genExpr target
    values <- mapM genExpr args
    let (result,parameters) = case representation (checkedType target) of
          FunctionPointerT r ps -> (r,ps)
          _ -> error "internal: indirect call without function pointer"
        parameterTypes = if null parameters then "void" else comma (map ctype parameters)
        function = "((" ++ ctype result ++ " (*)(" ++ parameterTypes ++ "))" ++ callee ++ ".code)"
    temp (targetPre ++ ["k_check_function_pointer(" ++ callee ++ ", " ++ position e ++ ");"] ++
      concatMap fst values) (function ++ "(" ++ comma (map snd values) ++ ")")
  MethodCall base name args -> do
    (pre,receiverValue) <- genAddress base
    receiver <- fresh
    values <- mapM genExpr args
    let capture = "k_pointer " ++ receiver ++ " = " ++ receiverValue ++ ";"
    temp (pre ++ [capture] ++ concatMap fst values) (routineC name ++ "(" ++ comma (receiver:map snd values) ++ ")")
  Member _ _ -> error "internal: unresolved member"
  MemberCall _ _ _ -> error "internal: unresolved member call"
  Variable _ -> error "internal: unresolved variable reached C backend"
  where
    t = checkedType e
    temp = temporary t
    width ty = if representation ty == Ganz then "16" else "32"
    printArg a v = case representation (checkedType a) of
      Ganz -> "printf(\"%d\", (int)" ++ v ++ ");"
      Lang -> "printf(\"%ld\", (long)" ++ v ++ ");"
      Gleit -> "printf(\"%.9g\", (double)" ++ v ++ ");"
      BoolT -> "fputs(" ++ v ++ " ? \"WAHR\" : \"FALSCH\", stdout);"
      CharT -> "k_put_char(" ++ v ++ ");"
      TextT -> "k_put_string(" ++ v ++ ");"
      Unit -> error "internal: printing procedure"
      ArrayT _ _ -> error "internal: printing whole array"
      KetteT _ -> "k_write_bytes(" ++ v ++ ".data, " ++ v ++ ".length);"
      _ -> error "internal: unsupported output type"

genReadArg :: String -> Expr -> Gen [String]
genReadArg result a
  | KetteT capacity <- checkedType a = do
      (pre,target) <- genLValue a
      pure (pre ++ [result ++ " = k_read_kette(" ++ target ++ ".data, " ++ show capacity ++
        ", &" ++ target ++ ".length, " ++ position a ++ ");"])
  | otherwise = do
      (pre,store) <- case exprNode a of
        ArrayAccess base _ | KetteT _ <- checkedType base -> do
          (before,write) <- genKetteWrite a base
          pure (before,\value -> "(void)" ++ write value ++ ";")
        _ -> do
          (before,target) <- genLValue a
          pure (before,\value -> target ++ " = " ++ value ++ ";")
      let ty = checkedType a
      (readInput,value) <- case representation ty of
        CharT -> do
          character <- fresh
          pure ([ "uint8_t " ++ character ++ ";"
                , result ++ " = k_read_char(&" ++ character ++ ", " ++ position a ++ ");"
                ], character)
        base -> do
          token <- fresh
          let parameters = token ++ ", " ++ position a ++ ")"
              parsed = case base of
                BoolT -> "k_parse_bool(" ++ parameters
                Gleit -> "k_parse_float(" ++ parameters
                _ -> "(" ++ ctype ty ++ ")k_parse_int(" ++ token ++ ", " ++ bounds ty ++ ", " ++ position a ++ ")"
          pure ([ "const char *" ++ token ++ " = k_token(" ++ position a ++ ");"
                , result ++ " = " ++ token ++ " != NULL;"
                ], parsed)
      pure (pre ++ readInput ++ ["if (" ++ result ++ ")"] ++ braces [store value])

genDecl :: Bool -> Decl -> Gen [String]
genDecl global (Decl b ini) = do
  (pre,value) <- maybe (pure ([],zero (bindType b))) genExpr ini
  let lhs = if global then bindingC b else
        bindingDeclaration b
  pure (pre ++ [lhs ++ " = " ++ value ++ ";"])
  where
    zero ty | isStruct ty = "(" ++ ctype ty ++ "){0}"
            | isFunctionPointer ty = "k_null_function_pointer()"
            | isDataPointer ty = "k_null_pointer()"
            | representation ty == BoolT = "false"
            | otherwise = "0"
genDecl global (ArrayDecl b _)
  | global = pure []
  | otherwise = pure [bindingDeclaration b ++ " = {0};"]
genDecl global d@(KetteDecl _ _ _)
  | global = pure []
  | otherwise = pure [ketteDeclaration d]

constPrefix :: Binding -> String
constPrefix b = if bindConst b then "const " else ""

declarator :: Type -> String -> String
declarator ty name = let (element,counts) = arrayShape ty in
  ctype element ++ " " ++ name ++ concatMap (\n -> "[" ++ show n ++ "]") counts

-- KONST freezes the stored pointer itself, not its mutable pointee.
bindingDeclaration :: Binding -> String
bindingDeclaration b =
  let (element,_) = arrayShape (bindType b)
  in if bindConst b && isPointer element
     then declarator (bindType b) ("const " ++ bindingC b)
     else constPrefix b ++ declarator (bindType b) (bindingC b)

ketteDeclaration :: Decl -> String
ketteDeclaration (KetteDecl b _ ini) =
  let bytes = case ini of
        Nothing -> []
        Just e -> case exprNode e of
          StringLit xs -> xs
          _ -> error "internal: nonliteral KETTE initializer"
      contents = if null bytes then "0" else comma (map show bytes)
  in constPrefix b ++ ctype (bindType b) ++ " " ++ bindingC b ++ " = {" ++
    show (length bytes) ++ ", {" ++ contents ++ "}};"
ketteDeclaration _ = error "internal: expected KETTE declaration"

-- A FOR continue jumps to the step clause, while WHILE uses C continue.
lifetimeOwner :: Binding -> Bool
lifetimeOwner b = not (isReference (bindType b))

lifetimeDeclaration :: Binding -> String
lifetimeDeclaration b = "k_lifetime *" ++ bindingLife b ++ " = k_lifetime_local(" ++
  show (line (bindPos b)) ++ ", " ++ show (column (bindPos b)) ++ ");"

cleanup :: [String] -> [String]
cleanup = map (\life -> "k_lifetime_end(" ++ life ++ ");")

directBindings :: [Stmt] -> [Binding]
directBindings ss = [declBinding d | Stmt _ (Declaration d) <- ss, lifetimeOwner (declBinding d)]

cleanupExited :: [String] -> [String] -> [String]
cleanupExited active keep = cleanup (take (max 0 (length active - length keep)) active)

genScopedContents :: [String] -> Maybe [String] -> Maybe String -> Maybe (String,[String]) -> [Stmt] -> Gen [String]
genScopedContents active loopKeep continuation falling ss = do
  let bindings = directBindings ss
      own = map bindingLife bindings
      here = own ++ active
  body <- concat <$> mapM (genStmtWith here loopKeep continuation falling) ss
  pure (map lifetimeDeclaration bindings ++ body ++ cleanup own)

genStmt :: [String] -> Maybe [String] -> Maybe String -> Stmt -> Gen [String]
genStmt active loopKeep continuation = genStmtWith active loopKeep continuation Nothing

genStmtWith :: [String] -> Maybe [String] -> Maybe String -> Maybe (String,[String]) -> Stmt -> Gen [String]
genStmtWith active loopKeep continuation falling (Stmt _ node) = case node of
  Block ss -> braces <$> genScopedContents active loopKeep continuation falling ss
  Declaration d -> genDecl False d
  Expression e -> do (pre,v) <- genExpr e; pure (pre ++ ["(void)" ++ v ++ ";" | not (null v)])
  Empty -> pure [";"]
  TypeStatement _ -> error "internal: unexpanded type statement"
  Return Nothing -> pure (cleanup active ++ ["return;"])
  Return (Just e) -> do (pre,v) <- genExpr e; pure (pre ++ cleanup active ++ ["return " ++ v ++ ";"])
  Break -> case loopKeep of
    Just keep -> pure (cleanupExited active keep ++ ["break;"])
    Nothing -> error "internal: break without loop"
  Continue -> case loopKeep of
    Just keep -> pure (cleanupExited active keep ++ [maybe "continue;" (\label -> "goto " ++ label ++ ";") continuation])
    Nothing -> error "internal: continue without loop"
  Fallthrough -> case falling of
    Just (label,keep) -> pure (cleanupExited active keep ++ ["goto " ++ label ++ ";"])
    Nothing -> error "internal: invalid FALLTHROUGH reached backend"
  If c a b -> do
    (pre,v) <- genExpr c; yes <- sub a
    no <- maybe (pure []) (fmap ("else":) . sub) b
    pure (pre ++ ["if (" ++ v ++ ")"] ++ yes ++ no)
  While c body -> do
    (pre,v) <- genExpr c; b <- genStmt active (Just active) Nothing body
    pure (["while (true)"] ++ braces (pre ++ ["if (!" ++ v ++ ") break;"] ++ b))
  For ini condition step body -> do
    let bindings = [declBinding d | Just (Stmt _ (Declaration d)) <- [ini], lifetimeOwner (declBinding d)]
        own = map bindingLife bindings
        here = own ++ active
    initCode <- maybe (pure []) (genStmt here loopKeep continuation) ini
    label <- fresh
    cond <- case condition of
      Nothing -> pure []
      Just e -> do (pre,v) <- genExpr e; pure (pre ++ ["if (!" ++ v ++ ") break;"])
    bodyCode <- genStmt here (Just here) (Just label) body
    stepCode <- case step of
      Nothing -> pure []
      Just e -> do (pre,v) <- genExpr e; pure (pre ++ ["(void)" ++ v ++ ";" | not (null v)])
    pure (braces (map lifetimeDeclaration bindings ++ initCode ++ ["while (true)"] ++ braces
      (cond ++ bodyCode ++ ["goto " ++ label ++ ";",label ++ ": ;"] ++ stepCode) ++ cleanup own))
  Switch value arms -> do
    (pre,v) <- genExpr value
    end <- fresh
    labels <- mapM (const fresh) arms
    let dispatch = ["if (" ++ v ++ " == " ++ constant e ++ ") goto " ++ label ++ ";"
                   | (arm,label) <- zip arms labels, Just e <- [armValue arm]]
        defaults = [label | (arm,label) <- zip arms labels, armValue arm == Nothing]
        fallback = case defaults of label:_ -> label; [] -> end
        nexts = map Just (drop 1 labels) ++ [Nothing]
    bodies <- sequence [do
      body <- genScopedContents active loopKeep continuation (fmap (\nextLabel -> (nextLabel,active)) next) (armBody arm)
      pure ([label ++ ":"] ++ braces (body ++ ["goto " ++ end ++ ";"]))
      | (arm,label,next) <- zip3 arms labels nexts]
    pure (braces (pre ++ dispatch ++ ["goto " ++ fallback ++ ";"] ++ concat bodies ++ [end ++ ": ;"]))
  where
    sub = genStmtWith active loopKeep continuation falling
    constant e = case exprNode e of
      IntLit n -> "(" ++ show n ++ "LL)"
      CharLit n -> show n
      _ -> error "internal: CASE constant not folded"

prototype :: Routine -> String
prototype r = ctype (routineResult r) ++ " " ++ routineC (routineName r) ++ "(" ++
  (if null parameters then "void" else comma parameters) ++ ")"
  where parameters = maybe [] (const ["k_pointer k_self"]) (routineOwner r) ++
          [ctype (bindType b) ++ " " ++ bindingC b | b <- routineParams r]

generateC :: Program -> String
generateC program@(Program tops) = unlines $ ["/* Generated by Kern 82. C99; host source decoded from UTF-8. */",runtime] ++
  ["typedef struct { size_t length; uint8_t data[" ++ show n ++ "]; } " ++ ctype (KetteT n) ++ ";"
   | n <- nub [capacity | KetteDecl b _ _ <- programDecls program, KetteT capacity <- [bindType b]]] ++
  ["typedef struct " ++ typeC ident ++ " " ++ typeC ident ++ ";" | TypeTop (StructDef ident _) <- tops] ++
  concat [structure ident ms | TypeTop (StructDef ident ms) <- tops] ++
  [prototype r ++ ";" | Function r <- tops] ++
  concat [["static k_lifetime " ++ bindingLife b ++ "_storage = {K_LIVE_GLOBAL};",
           "static k_lifetime *" ++ bindingLife b ++ " = &" ++ bindingLife b ++ "_storage;"]
          | Global d <- tops, let b = declBinding d] ++
  [globalDeclaration d | Global d <- tops] ++
  fst (runGen generate 0)
  where
    generate = do
      globals <- concat <$> mapM (genDecl True) [d | Global d <- tops]
      routines <- concat <$> mapM genRoutine [r | Function r <- tops]
      pure (["static void k_initialize(void)"] ++ braces globals ++ routines ++
        ["int main(void)"] ++ braces ["k_initialize();",routineC "main" ++ "();","return 0;"])
    genRoutine r = case routineBody r of
      Nothing -> pure []
      Just s -> do
        let params = [b | b <- routineParams r, lifetimeOwner b]
            active = map bindingLife params
        body <- genStmt active Nothing Nothing s
        pure (prototype r : braces (map lifetimeDeclaration params ++ body ++ cleanup active))

    globalDeclaration d = case d of
      Decl b _ -> ctype (bindType b) ++ " " ++ bindingC b ++ ";"
      ArrayDecl b _ -> bindingDeclaration b ++
        (if bindConst b then " = {0};" else ";")
      KetteDecl _ _ _ -> ketteDeclaration d

structure :: TypeId -> [MemberDecl] -> [String]
structure ident members =
  ["struct " ++ typeC ident ++ " {"] ++
  indent (case [fieldDeclaration d | Field _ d <- members] of [] -> ["uint8_t k_empty;"]; fields -> fields) ++
  ["};"]
  where
    fieldDeclaration d = let b = declBinding d in declarator (bindType b) (bindingC b) ++ ";"

cString :: [Int] -> String
cString s = "(const uint8_t *)\"" ++ concatMap byte s ++ "\""
  where
    byte n = let raw = showOct n "" in '\\' : replicate (3-length raw) '0' ++ raw
