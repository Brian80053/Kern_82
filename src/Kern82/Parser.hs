module Kern82.Parser (parseProgram, parseCompilationUnit) where

import Control.Monad (ap)
import Kern82.AST
import Kern82.Lexer

newtype P a = P { runP :: [Token] -> Result (a, [Token]) }
instance Functor P where fmap f p = P $ \ts -> do (x,rs) <- runP p ts; pure (f x,rs)
instance Applicative P where pure x = P $ \ts -> Right (x,ts); (<*>) = ap
instance Monad P where p >>= f = P $ \ts -> do (x,rs) <- runP p ts; runP (f x) rs
peek :: P Token
peek = P $ \ts -> case ts of
  t:_ -> Right (t,ts)
  [] -> failure "parser" (Pos 1 1) "unexpected end of token stream"
pop :: P Token
pop = do t <- peek; P $ \ts -> Right (t,drop 1 ts)
bad :: String -> P a
bad msg = do t <- peek; P $ \_ -> failure "parser" (tokenPos t) msg
at :: String -> P Bool
at s = (== Symbol s) . tokenKind <$> peek
accept :: String -> P Bool
accept s = do yes <- at s; if yes then pop >> pure True else pure False
expect :: String -> P ()
expect s = do yes <- accept s; if yes then pure () else bad ("expected " ++ s)
identifier :: P (Pos,String)
identifier = do
  t <- peek
  case tokenKind t of
    Word n -> pop >> pure (tokenPos t,n)
    _ -> bad "expected an identifier (keywords are uppercase)"
parseType :: P Type
parseType = do
  t <- peek
  case tokenKind t of
    Word name -> pop >> pure (TypeName name)
    Symbol "ZEIGER" -> do
      _ <- pop
      next <- peek
      case tokenKind next of
        Symbol "FUNKTION" -> do
          _ <- pop
          result <- parseType
          expect "("
          FunctionPointerT result <$> separated ")" parseType
        Symbol "PROZEDUR" -> do
          _ <- pop
          expect "("
          FunctionPointerT Unit <$> separated ")" parseType
        _ -> PointerT <$> parseType
    Symbol "REFERENZ" -> pop >> ReferenceT <$> parseType
    Symbol "KONST" -> pop >> ConstT <$> parseType
    Symbol s -> case lookup s [("GANZ",Ganz),("LANG",Lang),("BOOL",BoolT),("GLEIT",Gleit),("CHAR",CharT)] of
      Just ty -> pop >> pure ty
      Nothing -> bad "expected GANZ, LANG, BOOL, GLEIT, or CHAR"
    _ -> bad "expected primitive type"
isDecl :: P Bool
isDecl = do
  t <- peek
  named <- P $ \ts -> Right ((case ts of
    Token _ (Word name):Token _ (Word _):_ -> name `notElem` ["ganz","lang","bool","gleit","char","kette"]
    _ -> False), ts)
  pure $ named || tokenKind t `elem` map Symbol
    ["KONST","REGISTER","GANZ","LANG","BOOL","GLEIT","CHAR","KETTE","ZEIGER","REFERENZ"]

parseProgram :: [Token] -> Result Program
parseProgram ts = do
  unit <- parseCompilationUnit ts
  case (unitModuleDecl unit,unitImportDecls unit) of
    (Nothing,[]) -> pure (Program (map unitTopValue (unitTopDecls unit)))
    _ -> failure "module" (Pos 1 1) "module-aware compilation requires an input file path"

parseCompilationUnit :: [Token] -> Result CompilationUnit
parseCompilationUnit ts = fst <$> runP unit ts
  where
    unit = do
      moduleDecl <- do
        present <- accept "MODUL"
        if present then do
          (p,name) <- identifier
          pure (Just (ModuleDecl p name))
        else pure Nothing
      (imports,items) <- contents
      pure (CompilationUnit moduleDecl imports items)
    contents = do
      t <- peek
      case tokenKind t of
        EndOfFile -> pure ([],[])
        Symbol "MODUL" -> bad "duplicate MODUL declaration or MODUL is not at the start of the file"
        Symbol "IMPORT" -> do
          _ <- pop
          (p,name) <- identifier
          expect ";"
          (imports,items) <- contents
          pure (ImportDecl p name:imports,items)
        Symbol "GENERISCH" -> do
          _ <- pop
          parameters <- genericParameters
          exported <- accept "EXPORT"
          item <- genericDefinition parameters
          (imports,items) <- contents
          pure (imports,UnitTop exported item:items)
        Symbol "EXPORT" -> do
          _ <- pop
          item <- topItem
          (imports,items) <- contents
          pure (imports,UnitTop True item:items)
        _ -> do
          item <- topItem
          (imports,items) <- contents
          pure (imports,UnitTop False item:items)
    topItem = do
      t <- peek
      case tokenKind t of
        Symbol "TYP" -> TypeTop <$> typeDeclaration
        Symbol "FUNKTION" -> routineTop
        Symbol "PROZEDUR" -> routineTop
        Symbol "GENERISCH" -> pop >> genericParameters >>= genericDefinition
        Symbol "IMPORT" -> bad "IMPORT cannot be prefixed with EXPORT"
        _ -> do
          d <- isDecl
          if d then do x <- declaration; expect ";"; pure (Global x)
          else bad "unsupported top-level construct; expected a declaration or routine"

genericParameters :: P [GenericParam]
genericParameters = do
  (p,name) <- identifier
  t <- peek
  constraint <- case tokenKind t of
    Symbol "VERGLEICHBAR" -> pop >> pure Comparable
    Symbol "GEORDNET" -> pop >> pure Ordered
    Symbol "NUMERISCH" -> pop >> pure Numeric
    _ -> pure Unconstrained
  more <- accept ","
  rest <- if more then genericParameters else pure []
  pure (GenericParam p name constraint:rest)

genericDefinition :: [GenericParam] -> P Top
genericDefinition parameters = do
  t <- peek
  if tokenKind t `elem` [Symbol "FUNKTION",Symbol "PROZEDUR"] then pure ()
    else bad "GENERISCH applies only to a top-level FUNKTION or PROZEDUR"
  r <- routine
  if routineName r == "main" then bad "generic main is not allowed" else pure ()
  case routineBody r of
    Nothing -> bad "generic routine requires a definition body"
    Just _ -> pure (Generic parameters r Nothing)

routineTop :: P Top
routineTop = do
  t <- pop
  result <- if tokenKind t == Symbol "FUNKTION" then parseType else pure Unit
  (_,name) <- identifier
  instantiation <- accept "="
  if instantiation then do
    expect "NEU"
    (_,template) <- identifier
    expect "("
    arguments <- separated ")" parseType
    expect ";"
    pure (Instantiate (tokenPos t) name result template arguments)
  else Function <$> routineTail (tokenPos t) name result

routine :: P Routine
routine = do
  t <- pop
  result <- if tokenKind t == Symbol "FUNKTION" then parseType else pure Unit
  (_,name) <- identifier
  routineTail (tokenPos t) name result

routineTail :: Pos -> String -> Type -> P Routine
routineTail p name result = do
  expect "("
  ps <- separated ")" parameter
  prototype <- accept ";"
  body <- if prototype then pure Nothing else Just <$> block
  pure (Routine p name result ps body Nothing Nothing)
  where
    parameter = do
      ty <- parseType; (parameterPos,n) <- identifier
      array <- at "["
      if array then bad "array parameters are not supported"
        else pure (Binding parameterPos n ty False)

typeDeclaration :: P TypeDecl
typeDeclaration = do
  expect "TYP"
  enumeration <- accept "ENUM"
  structure <- if enumeration then pure False else accept "STRUCT"
  (p,name) <- identifier
  let ident = TypeId p name
  if enumeration then do
    expect "BEGIN"
    values <- enumerators
    _ <- accept ";"
    pure (EnumDef ident values)
  else if structure then do
    expect "BEGIN"
    members <- structureMembers Public
    _ <- accept ";"
    pure (StructDef ident members)
  else do
    expect "="; base <- parseType; expect ";"
    pure (AliasDef ident base)
  where
    enumerators = do
      done <- accept "END"
      if done then pure [] else do
        (p,n) <- identifier
        hasValue <- accept "="
        value <- if hasValue then Just <$> expr else pure Nothing
        more <- accept ","
        rest <- if more then enumerators else expect "END" >> pure []
        pure (Enumerator p n value : rest)
    structureMembers access = do
      t <- peek
      case tokenKind t of
        Symbol "END" -> pop >> pure []
        Symbol "PUBLIC" -> pop >> structureMembers Public
        Symbol "PRIVATE" -> pop >> structureMembers Private
        Symbol "FUNKTION" -> (:) . Method access <$> routine <*> structureMembers access
        Symbol "PROZEDUR" -> (:) . Method access <$> routine <*> structureMembers access
        _ -> do
          d <- isDecl
          if d then do
            field <- declaration; expect ";"
            (Field access field :) <$> structureMembers access
          else bad "expected a STRUCT field, member routine, PUBLIC, PRIVATE, or END"

separated :: String -> P a -> P [a]
separated close item = do
  done <- accept close
  if done then pure [] else do
    x <- item
    comma <- accept ","
    if comma then (x:) <$> more else expect close >> pure [x]
  where
    more = do
      x <- item; comma <- accept ","
      if comma then (x:) <$> more else expect close >> pure [x]

declaration :: P Decl
declaration = do
  constant <- accept "KONST"
  _ <- accept "REGISTER"
  kette <- accept "KETTE"
  ty <- if kette then pure (KetteT 0) else parseType
  (p,name) <- identifier
  array <- accept "["
  if array then do
    slicing <- at ":"
    if slicing then bad "unsupported array syntax" else pure ()
    lengthExpr <- expr
    arrayClose
    dimensions <- if kette then pure [] else moreDimensions
    initializer <- accept "="
    if kette then do
      value <- if initializer then Just <$> expr else pure Nothing
      pure (KetteDecl (Binding p name ty constant) lengthExpr value)
    else if initializer then bad "array initializers are not supported"
      else pure (ArrayDecl (Binding p name (ArrayT ty 0) constant) (lengthExpr:dimensions))
  else do
    if kette then bad "KETTE requires a fixed capacity in brackets" else pure ()
    initPresent <- accept "="
    value <- if initPresent then Just <$> expr else pure Nothing
    pure (Decl (Binding p name ty constant) value)
  where
    moreDimensions = do
      another <- accept "["
      if not another then pure [] else do
        slicing <- at ":"
        if slicing then bad "unsupported array syntax" else pure ()
        size <- expr
        arrayClose
        (size:) <$> moreDimensions

block :: P Stmt
block = do p <- tokenPos <$> peek; expect "BEGIN"; Stmt p . Block <$> contents
  where
    contents = do
      done <- accept "END"
      if done then pure [] else do
        t <- peek
        if tokenKind t == EndOfFile then bad "expected END" else (:) <$> stmt <*> contents

stmt :: P Stmt
stmt = do
  t <- peek
  let p = tokenPos t
  case tokenKind t of
    Symbol "BEGIN" -> block
    Symbol "TYP" -> Stmt p . TypeStatement <$> typeDeclaration
    Symbol "IF" -> do
      _ <- pop; c <- expr; expect "THEN"; a <- block
      hasElse <- accept "ELSE"
      b <- if hasElse then do
        chained <- at "IF"
        if chained then do
          branch@(Stmt branchPos _) <- stmt
          -- Keep condition temporaries inside the else branch in generated C.
          pure (Just (Stmt branchPos (Block [branch])))
          else Just <$> block
        else pure Nothing
      pure (Stmt p (If c a b))
    Symbol "WHILE" -> do
      _ <- pop; expect "("; c <- expr; expect ")"; b <- block
      pure (Stmt p (While c b))
    Symbol "FOR" -> do
      _ <- pop; expect "("
      emptyInit <- accept ";"
      i <- if emptyInit then pure Nothing else do
        d <- isDecl
        s <- if d then Stmt p . Declaration <$> declaration else Stmt p . Expression <$> expr
        expect ";"; pure (Just s)
      c <- optionalExpr ";"
      step <- optionalExpr ")"
      b <- block
      pure (Stmt p (For i c step b))
    Symbol "RÜCKGABE" -> pop >> (Stmt p . Return <$> optionalExpr ";")
    Symbol "ABBRUCH" -> pop >> expect ";" >> pure (Stmt p Break)
    Symbol "WEITER" -> pop >> expect ";" >> pure (Stmt p Continue)
    Symbol "FALLTHROUGH" -> pop >> expect ";" >> pure (Stmt p Fallthrough)
    Symbol "SWITCH" -> do
      _ <- pop; value <- expr; expect "BEGIN"
      arms <- switchArms
      pure (Stmt p (Switch value arms))
    Symbol ";" -> pop >> pure (Stmt p Empty)
    _ -> do
      d <- isDecl
      s <- if d then Declaration <$> declaration else Expression <$> expr
      expect ";"; pure (Stmt p s)

switchArms :: P [SwitchArm]
switchArms = do
  done <- accept "END"
  if done then pure [] else do
    t <- peek
    value <- case tokenKind t of
      Symbol "CASE" -> pop >> (Just <$> expr)
      Symbol "DEFAULT" -> pop >> pure Nothing
      _ -> bad "expected CASE, DEFAULT, or END in SWITCH"
    expect ":"
    body <- contents
    (SwitchArm (tokenPos t) value body :) <$> switchArms
  where
    contents = do
      t <- peek
      case tokenKind t of
        Symbol s | s `elem` ["CASE","DEFAULT","END"] -> pure []
        EndOfFile -> bad "expected END in SWITCH"
        _ -> (:) <$> stmt <*> contents

optionalExpr :: String -> P (Maybe Expr)
optionalExpr close = do
  done <- accept close
  if done then pure Nothing else do e <- expr; expect close; pure (Just e)

expr :: P Expr
expr = do
  lhs <- precedence 1
  assignment <- accept "="
  if assignment then expression (exprPos lhs) . Assign lhs <$> expr else pure lhs

-- C precedence, with logical XOR strictly between OR and AND.
levels :: [[String]]
levels = [["||","OR"],["XOR"],["&&","AND"],["|"],["^"],["&"],
          ["==","!="],["<","<=",">",">="],["<<",">>"],["+","-"],["*","/","%"]]
precedence :: Int -> P Expr
precedence level
  | level > length levels = unary
  | otherwise = precedence (level+1) >>= rest
  where
    rest lhs = do
      t <- peek
      case tokenKind t of
        Symbol op | op `elem` (levels !! (level-1)) -> do
          _ <- pop; rhs <- precedence (level+1)
          rest (expression (tokenPos t) (Binary op lhs rhs))
        _ -> pure lhs
unary :: P Expr
unary = do
  t <- peek
  case tokenKind t of
    Symbol op | op `elem` ["+","-","!","NOT","~","++","--"] -> do
      _ <- pop; e <- unary
      pure $ expression (tokenPos t) $ if op `elem` ["++","--"]
        then Increment op True e else Unary op e
    _ -> primary >>= postfix

postfix :: Expr -> P Expr
postfix e = do
  t <- peek
  case tokenKind t of
    Symbol "." -> do
      field <- P $ \ts -> Right ((case ts of _:Token _ (Word _):_ -> True; _ -> False),ts)
      if not field then pure e else do
        _ <- pop; (_,name) <- identifier
        call <- accept "("
        node <- if call then MemberCall e name <$> separated ")" expr else pure (Member e name)
        postfix (expression (tokenPos t) node)
    Symbol "[" -> do
      _ <- pop
      slicing <- at ":"
      if slicing then bad "unsupported array syntax" else pure ()
      index <- expr
      arrayClose
      postfix (expression (tokenPos t) (ArrayAccess e index))
    Symbol op | op `elem` ["++","--"] -> do
      _ <- pop
      postfix (expression (tokenPos t) (Increment op False e))
    _ -> pure e

arrayClose :: P ()
arrayClose = do
  t <- peek
  case tokenKind t of
    Symbol ":" -> bad "unsupported array syntax"
    Symbol "." -> bad "unsupported array syntax"
    _ -> expect "]"
primary :: P Expr
primary = do
  t <- pop
  let e = expression (tokenPos t)
      call n = do expect "("; e . Call n <$> separated ")" expr
  case tokenKind t of
    IntegerToken n -> pure (e (IntLit n))
    FloatToken n -> pure (e (FloatLit n))
    CharacterToken c -> pure (e (CharLit c))
    StringToken s -> pure (e (StringLit s))
    Symbol "WAHR" -> pure (e (BoolLit True))
    Symbol "FALSCH" -> pure (e (BoolLit False))
    Symbol "NULL" -> pure (e NullLit)
    Symbol name | name `elem` ["ADDRESSE","INHALT","ALLOKIEREN","FREIGEBEN"] -> do
      expect "("; x <- expr; expect ")"
      pure (e (case name of
        "ADDRESSE" -> AddressOf x
        "INHALT" -> Dereference x
        "ALLOKIEREN" -> Allocate x
        _ -> Free x))
    Symbol "(" -> do x <- expr; expect ")"; pure x
    Word name -> do
      isCall <- at "("
      if isCall then call name else pure (e (Variable name))
    Symbol name | name `elem` ["AUSGABE","SCHREIBEN","LESEN"] -> call name
    Symbol name | name `elem` ["GANZ","LANG","GLEIT","CHAR","BOOL"] -> do
      let ty = case name of "GANZ" -> Ganz; "LANG" -> Lang; "GLEIT" -> Gleit; "CHAR" -> CharT; _ -> BoolT
      expect "("; x <- expr; expect ")"; pure (e (Cast ty x))
    _ -> P $ \_ -> failure "parser" (tokenPos t)
      ("expected expression; unsupported or misplaced token " ++ show (tokenKind t))
