module Kern82.AST where

import Kern82.Charset (encodeIdentifier)

data Pos = Pos { line :: Int, column :: Int } deriving (Eq, Ord, Show)
data Diagnostic = Diagnostic String Pos String
  | InstantiationDiagnostic GenericOrigin Diagnostic deriving (Eq, Show)
data GenericOrigin = GenericOrigin String [Type] (Maybe FilePath) deriving (Eq, Show)
inRoutineContext :: Routine -> Result a -> Result a
inRoutineContext r result = case (routineOrigin r,result) of
  (Just origin,Left issue) -> Left (InstantiationDiagnostic origin issue)
  _ -> result
type Result a = Either Diagnostic a
failure :: String -> Pos -> String -> Result a
failure phase p msg = Left (Diagnostic phase p msg)

data Type = Ganz | Lang | BoolT | Gleit | CharT | TextT | Unit
  | ArrayT Type Integer | KetteT Integer | PointerT Type | ReferenceT Type
  | ConstT Type | FunctionPointerT Type [Type]
  | TypeName String | NominalT TypeId Type | EnumT TypeId | StructT TypeId
  deriving (Eq, Ord, Show)
data TypeId = TypeId Pos String deriving (Eq, Ord, Show)
typeIdName :: TypeId -> String
typeIdName (TypeId _ name) = name
typeC :: TypeId -> String
typeC (TypeId (Pos l c) name) = "k_type_" ++ show l ++ "_" ++ show c ++ "_" ++ encode name
methodName :: TypeId -> String -> String
methodName ident name = typeC ident ++ "." ++ name
representation :: Type -> Type
representation (NominalT _ t) = representation t
representation (ConstT t) = representation t
representation t = t
typeName :: Type -> String
typeName t = case t of
  Ganz -> "GANZ"; Lang -> "LANG"; BoolT -> "BOOL"; Gleit -> "GLEIT"
  CharT -> "CHAR"; TextT -> "string literal"; Unit -> "procedure"
  ArrayT _ _ -> let (element,counts) = arrayShape t in
    typeName element ++ concatMap (\n -> "[" ++ show n ++ "]") counts
  KetteT capacity -> "KETTE[" ++ show capacity ++ "]"
  PointerT pointee -> "ZEIGER " ++ typeName pointee
  ReferenceT referent -> "REFERENZ " ++ typeName referent
  ConstT value -> "KONST " ++ typeName value
  FunctionPointerT result parameters -> "ZEIGER " ++
    (if result == Unit then "PROZEDUR" else "FUNKTION " ++ typeName result) ++
    "(" ++ commaTypes parameters ++ ")"
  TypeName name -> name
  NominalT ident _ -> typeIdName ident
  EnumT ident -> typeIdName ident
  StructT ident -> typeIdName ident
  where commaTypes [] = ""
        commaTypes [x] = typeName x
        commaTypes (x:xs) = typeName x ++ ", " ++ commaTypes xs
arrayShape :: Type -> (Type,[Integer])
arrayShape (ArrayT element count) = let (leaf,counts) = arrayShape element in (leaf,count:counts)
arrayShape t = (t,[])
isInteger, isNumeric :: Type -> Bool
isInteger t = representation t `elem` [Ganz, Lang]
isNumeric t = representation t `elem` [Ganz, Lang, Gleit]
isEnum, isStruct :: Type -> Bool
isEnum t = case representation t of EnumT _ -> True; _ -> False
isStruct t = case representation t of StructT _ -> True; _ -> False
isPointer :: Type -> Bool
isPointer t = case representation t of PointerT _ -> True; FunctionPointerT _ _ -> True; _ -> False
isDataPointer, isFunctionPointer :: Type -> Bool
isDataPointer t = case representation t of PointerT _ -> True; _ -> False
isFunctionPointer t = case representation t of FunctionPointerT _ _ -> True; _ -> False
isReference :: Type -> Bool
isReference t = case representation t of ReferenceT _ -> True; _ -> False
isConstType :: Type -> Bool
isConstType (ConstT _) = True
isConstType _ = False
unqualified :: Type -> Type
unqualified (ConstT t) = t
unqualified t = t
containsReference :: Type -> Bool
containsReference t = case t of
  ReferenceT _ -> True
  PointerT x -> containsReference x
  FunctionPointerT result parameters -> containsReference result || any containsReference parameters
  ConstT x -> containsReference x
  ArrayT x _ -> containsReference x
  NominalT _ x -> containsReference x
  _ -> False

data Binding = Binding
  { bindPos :: Pos, bindName :: String, bindType :: Type, bindConst :: Bool }
  deriving (Eq, Show)
bindingC :: Binding -> String
bindingC b = "k_v_" ++ show (line (bindPos b)) ++ "_" ++
  show (column (bindPos b)) ++ "_" ++ encode (bindName b)
encode :: String -> String
encode = encodeIdentifier
routineC :: String -> String
routineC n = "k_f_" ++ encode n

data Expr = Expr { exprPos :: Pos, exprType :: Maybe Type, exprNode :: ExprNode }
  deriving (Eq, Show)
data ExprNode
  = IntLit Integer | FloatLit Float | BoolLit Bool | CharLit Int | StringLit [Int]
  | Variable String | Bound Binding | Call String [Expr] | Cast Type Expr
  | Unary String Expr | Binary String Expr Expr | Assign Expr Expr
  | Increment String Bool Expr | ArrayAccess Expr Expr
  | EnumLit Type Integer | Member Expr String | MemberCall Expr String [Expr]
  | FieldAccess Expr Binding | MethodCall Expr String [Expr] | Receiver TypeId
  | AddressOf Expr | Dereference Expr | NullLit | ReferenceBind Expr
  | Allocate Expr | Free Expr | RoutineAddress String | IndirectCall Expr [Expr]
  | ReferenceResult Type Expr
  deriving (Eq, Show)
expression :: Pos -> ExprNode -> Expr
expression p n = Expr p Nothing n
typed :: Type -> Expr -> Expr
typed t e = e { exprType = Just t }
checkedType :: Expr -> Type
checkedType e = case exprType e of
  Just t -> t
  Nothing -> error "internal error: untyped expression reached backend"

data Decl = Decl Binding (Maybe Expr) | ArrayDecl Binding [Expr]
  | KetteDecl Binding Expr (Maybe Expr) deriving (Eq, Show)
declBinding :: Decl -> Binding
declBinding (Decl b _) = b
declBinding (ArrayDecl b _) = b
declBinding (KetteDecl b _ _) = b
data Stmt = Stmt Pos StmtNode deriving (Eq, Show)
data StmtNode
  = Block [Stmt] | Declaration Decl | Expression Expr
  | If Expr Stmt (Maybe Stmt) | While Expr Stmt
  | For (Maybe Stmt) (Maybe Expr) (Maybe Expr) Stmt
  | Return (Maybe Expr) | Break | Continue | Empty
  | Switch Expr [SwitchArm] | Fallthrough
  | TypeStatement TypeDecl
  deriving (Eq, Show)
-- Nothing identifies DEFAULT. Arms remain in source order for FALLTHROUGH.
data SwitchArm = SwitchArm
  { armPos :: Pos, armValue :: Maybe Expr, armBody :: [Stmt] }
  deriving (Eq, Show)

programDecls :: Program -> [Decl]
programDecls (Program tops) = concatMap top tops
  where
    top (Global d) = [d]
    top (Function r) = maybe [] stmtDecls (routineBody r)
    top (TypeTop (StructDef _ members)) = [d | Field _ d <- members]
    top (TypeTop _) = []
    top _ = []
stmtDecls :: Stmt -> [Decl]
stmtDecls (Stmt _ node) = case node of
  Declaration d -> [d]
  Block ss -> concatMap stmtDecls ss
  If _ a b -> stmtDecls a ++ maybe [] stmtDecls b
  While _ b -> stmtDecls b
  For ini _ _ b -> maybe [] stmtDecls ini ++ stmtDecls b
  Switch _ arms -> concatMap (concatMap stmtDecls . armBody) arms
  _ -> []
data Routine = Routine
  { routinePos :: Pos, routineName :: String, routineResult :: Type
  , routineParams :: [Binding], routineBody :: Maybe Stmt, routineOwner :: Maybe TypeId
  , routineOrigin :: Maybe GenericOrigin }
  deriving (Eq, Show)
data Access = Public | Private deriving (Eq, Show)
data Enumerator = Enumerator Pos String (Maybe Expr) deriving (Eq, Show)
data MemberDecl = Field Access Decl | Method Access Routine deriving (Eq, Show)
data TypeDecl = AliasDef TypeId Type | EnumDef TypeId [Enumerator]
  | StructDef TypeId [MemberDecl] deriving (Eq, Show)
typeDeclId :: TypeDecl -> TypeId
typeDeclId d = case d of AliasDef i _ -> i; EnumDef i _ -> i; StructDef i _ -> i
data Constraint = Unconstrained | Comparable | Ordered | Numeric deriving (Eq, Show)
data GenericParam = GenericParam Pos String Constraint deriving (Eq, Show)
data Top = Global Decl | Function Routine | TypeTop TypeDecl
  | Generic [GenericParam] Routine (Maybe FilePath)
  | Instantiate Pos String Type String [Type]
  deriving (Eq, Show)
newtype Program = Program [Top] deriving (Eq, Show)

data ModuleDecl = ModuleDecl Pos String deriving (Eq, Show)
data ImportDecl = ImportDecl Pos String deriving (Eq, Show)
data UnitTop = UnitTop { unitTopExported :: Bool, unitTopValue :: Top }
  deriving (Eq, Show)
data CompilationUnit = CompilationUnit
  { unitModuleDecl :: Maybe ModuleDecl
  , unitImportDecls :: [ImportDecl]
  , unitTopDecls :: [UnitTop]
  } deriving (Eq, Show)
