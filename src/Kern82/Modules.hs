module Kern82.Modules (SourceDiagnostic(..), loadModuleProgram, loadModuleProgramWithSearch, cloneRoutine, readUtf8) where

import Control.Exception (evaluate)
import Data.List (nub)
import qualified Data.Map.Strict as M
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)
import System.IO
import Kern82.AST
import Kern82.Lexer (lexSource)
import Kern82.Parser (parseCompilationUnit)

data SourceDiagnostic = SourceDiagnostic FilePath String Diagnostic

data LoadedUnit = LoadedUnit
  { loadedPath :: FilePath
  , loadedSource :: String
  , loadedName :: Maybe String
  , loadedImports :: [ImportDecl]
  , loadedTops :: [UnitTop]
  }

data SymbolKind = TypeSymbol | ValueSymbol deriving (Eq, Ord)
data Symbol = Symbol
  { symbolSource :: String
  , symbolTarget :: String
  , symbolKind :: SymbolKind
  , symbolPos :: Pos
  }

data RenameEnv = RenameEnv
  { renameOwn :: M.Map (String,SymbolKind) Symbol
  , renameImports :: M.Map String [Symbol]
  , renameLocalValues :: [M.Map String String]
  , renameLocalTypes :: [M.Map String String]
  , renameIdentity :: Maybe String
  , renameClosed :: Bool
  }

loadModuleProgram :: FilePath -> IO (Either SourceDiagnostic (String,Program))
loadModuleProgram = loadModuleProgramWithSearch []

loadModuleProgramWithSearch :: [FilePath] -> FilePath -> IO (Either SourceDiagnostic (String,Program))
loadModuleProgramWithSearch searchPaths rootPath = do
  rootResult <- parseFile rootPath
  case rootResult of
    Left issue -> pure (Left issue)
    Right (source,unit) -> do
      let root = fromUnit rootPath source Nothing unit
      case unitModuleDecl unit of
        Just (ModuleDecl p _) | hasMain root ->
          pure (Left (problem root p "main is not allowed inside a MODUL file"))
        _ -> do
          imported <- loadImports (takeDirectory rootPath : searchPaths) [] M.empty [] root (loadedImports root)
          pure $ case imported of
            Left issue -> Left issue
            Right (_,ordered) -> case linkUnits (ordered ++ [root]) of
              Left issue -> Left issue
              Right program -> Right (source,program)

parseFile :: FilePath -> IO (Either SourceDiagnostic (String,CompilationUnit))
parseFile path = do
  source <- readUtf8 path
  pure $ case lexSource source >>= parseCompilationUnit of
    Left diagnostic -> Left (SourceDiagnostic path source diagnostic)
    Right unit -> Right (source,unit {unitTopDecls=map attach (unitTopDecls unit)})
  where
    attach (UnitTop exported (Generic parameters routine _)) =
      UnitTop exported (Generic parameters routine (Just path))
    attach item = item

readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \handle -> do
  hSetEncoding handle utf8
  source <- hGetContents handle
  _ <- evaluate (length source)
  pure source

fromUnit :: FilePath -> String -> Maybe String -> CompilationUnit -> LoadedUnit
fromUnit path source identity unit =
  LoadedUnit path source identity (unitImportDecls unit) (unitTopDecls unit)

problem :: LoadedUnit -> Pos -> String -> SourceDiagnostic
problem unit p message =
  SourceDiagnostic (loadedPath unit) (loadedSource unit) (Diagnostic "module" p message)

loadImports :: [FilePath] -> [String] -> M.Map String LoadedUnit -> [LoadedUnit]
  -> LoadedUnit -> [ImportDecl]
  -> IO (Either SourceDiagnostic (M.Map String LoadedUnit,[LoadedUnit]))
loadImports _ _ loaded ordered _ [] = pure (Right (loaded,ordered))
loadImports directories stack loaded ordered importer (ImportDecl p name:rest)
  | name `elem` stack = pure (Left (problem importer p ("circular import involving " ++ name)))
  | M.member name loaded = loadImports directories stack loaded ordered importer rest
  | otherwise = do
      -- The project root stays first, even for imports inside stdlib modules.
      -- This keeps project overrides independent of import traversal order.
      let candidates = case directories of
            project:standard -> nub (project : takeDirectory (loadedPath importer) : standard)
            [] -> [takeDirectory (loadedPath importer)]
      found <- findModule candidates name
      case found of
        Nothing -> pure (Left (problem importer p ("module not found: " ++ name)))
        Just path -> do
          parsed <- parseFile path
          case parsed of
            Left issue -> pure (Left issue)
            Right (source,unit) -> case unitModuleDecl unit of
              Nothing -> pure (Left (SourceDiagnostic path source (Diagnostic "module" (Pos 1 1)
                ("imported file must declare MODUL " ++ name))))
              Just (ModuleDecl declarationPos declared)
                | declared /= name -> pure (Left (SourceDiagnostic path source
                    (Diagnostic "module" declarationPos
                      ("module name mismatch: IMPORT " ++ name ++ " found MODUL " ++ declared))))
                | otherwise -> do
                    let loadedUnit = fromUnit path source (Just name) unit
                    if hasMain loadedUnit
                      then pure (Left (problem loadedUnit declarationPos
                        "main is not allowed inside a MODUL file"))
                      else do
                        dependencies <- loadImports directories (name:stack) loaded ordered
                          loadedUnit (loadedImports loadedUnit)
                        case dependencies of
                          Left issue -> pure (Left issue)
                          Right (withDependencies,dependencyOrder) ->
                            loadImports directories stack (M.insert name loadedUnit withDependencies)
                              (dependencyOrder ++ [loadedUnit]) importer rest

findModule :: [FilePath] -> String -> IO (Maybe FilePath)
findModule [] _ = pure Nothing
findModule (directory:rest) name = do
  let path = directory </> name ++ ".k82"
  exists <- doesFileExist path
  if exists then pure (Just path) else findModule rest name

hasMain :: LoadedUnit -> Bool
hasMain unit = any isMain (loadedTops unit)
  where
    isMain (UnitTop _ (Function routine)) = routineName routine == "main"
    isMain (UnitTop _ (Generic _ routine _)) = routineName routine == "main"
    isMain (UnitTop _ (Instantiate _ name _ _ _)) = name == "main"
    isMain _ = False

linkUnits :: [LoadedUnit] -> Either SourceDiagnostic Program
linkUnits units = do
  let exportTable = M.fromList
        [(name,exportsOf unit) | unit <- units, Just name <- [loadedName unit]]
  Program . concat <$> mapM (renameUnit exportTable) units

exportsOf :: LoadedUnit -> [Symbol]
exportsOf unit = deduplicate
  [ symbolFor unit source kind pos
  | UnitTop exported top <- loadedTops unit
  , exported
  , (source,kind,pos) <- topSymbols top
  ]

deduplicate :: [Symbol] -> [Symbol]
deduplicate = M.elems . M.fromList . map (\symbol ->
  ((symbolSource symbol,symbolKind symbol),symbol))

symbolFor :: LoadedUnit -> String -> SymbolKind -> Pos -> Symbol
symbolFor unit source kind pos =
  Symbol source (qualified (loadedName unit) source) kind pos

qualified :: Maybe String -> String -> String
qualified Nothing name = name
qualified (Just moduleName) name = moduleName ++ "$" ++ name

topSymbols :: Top -> [(String,SymbolKind,Pos)]
topSymbols top = case top of
  Global declaration -> let binding = declBinding declaration
    in [(bindName binding,ValueSymbol,bindPos binding)]
  Function routine -> [(routineName routine,ValueSymbol,routinePos routine)]
  Generic _ routine _ -> [(routineName routine,ValueSymbol,routinePos routine)]
  Instantiate p name _ _ _ -> [(name,ValueSymbol,p)]
  TypeTop declaration -> case declaration of
    AliasDef (TypeId p name) _ -> [(name,TypeSymbol,p)]
    StructDef (TypeId p name) _ -> [(name,TypeSymbol,p)]
    EnumDef (TypeId p name) members -> (name,TypeSymbol,p):
      [(memberName,ValueSymbol,memberPos) | Enumerator memberPos memberName _ <- members]

renameUnit :: M.Map String [Symbol] -> LoadedUnit -> Either SourceDiagnostic [Top]
renameUnit exportTable unit = wrap $ do
  let ownSymbols = deduplicate
        [symbolFor unit source kind pos
        | UnitTop _ top <- loadedTops unit
        , (source,kind,pos) <- topSymbols top]
      own = M.fromList [((symbolSource symbol,symbolKind symbol),symbol)
        | symbol <- ownSymbols]
      importedNames = nub [name | ImportDecl _ name <- loadedImports unit]
      imported = M.fromListWith (++)
        [(symbolSource symbol,[symbol])
        | name <- importedNames
        , symbol <- M.findWithDefault [] name exportTable]
      conflicts = [(symbolPos symbol,symbolSource symbol)
        | symbol <- ownSymbols
        , not (null (M.findWithDefault [] (symbolSource symbol) imported))]
  case conflicts of
    (p,name):_ -> failure "module" p
      ("local top-level symbol conflicts with imported symbol: " ++ name)
    [] -> pure ()
  let env = RenameEnv own imported [M.empty] [M.empty] (loadedName unit) False
  mapM (renameTop env . unitTopValue) (loadedTops unit)
  where
    wrap result = case result of
      Left diagnostic -> Left (SourceDiagnostic (loadedPath unit) (loadedSource unit) diagnostic)
      Right value -> Right value

renameTop :: RenameEnv -> Top -> Result Top
renameTop env top = case top of
  Global declaration -> Global <$> renameDecl True env declaration
  Function routine -> Function <$> renameRoutine True env M.empty routine
  TypeTop declaration -> TypeTop <$> renameTypeDecl True env declaration
  Generic parameters routine source -> do
    let parameterNames = M.fromList [(name,name) | GenericParam _ name _ <- parameters]
        inner = env {renameLocalTypes=parameterNames:renameLocalTypes env,renameClosed=True}
    changed <- renameRoutine True inner M.empty routine
    pure (Generic parameters changed source)
  Instantiate p name result template arguments -> Instantiate p (ownTarget env ValueSymbol name)
    <$> renameType env p result <*> resolveValueName env p template <*> mapM (renameType env p) arguments

-- Reuse lexical renaming for recursion and specialization-local type identities.
cloneRoutine :: String -> Routine -> Result Routine
cloneRoutine name routine = do
  let old = routineName routine
      symbol = Symbol old name ValueSymbol (routinePos routine)
      env = RenameEnv (M.singleton (old,ValueSymbol) symbol) M.empty [M.empty] [M.empty]
        (Just ("instance$" ++ name)) False
  changed <- renameRoutine False env M.empty routine
  pure changed {routineName=name}

renameDecl :: Bool -> RenameEnv -> Decl -> Result Decl
renameDecl topLevel env declaration = case declaration of
  Decl binding initializer ->
    Decl <$> renameBinding topLevel env binding <*> traverse (renameExpr env) initializer
  ArrayDecl binding dimensions ->
    ArrayDecl <$> renameBinding topLevel env binding <*> mapM (renameExpr env) dimensions
  KetteDecl binding capacity initializer ->
    KetteDecl <$> renameBinding topLevel env binding <*> renameExpr env capacity
      <*> traverse (renameExpr env) initializer

renameBinding :: Bool -> RenameEnv -> Binding -> Result Binding
renameBinding topLevel env binding = do
  ty <- renameType env (bindPos binding) (bindType binding)
  let name = if topLevel then ownTarget env ValueSymbol (bindName binding)
        else bindName binding
  pure binding {bindName=name,bindType=ty}

renameRoutine :: Bool -> RenameEnv -> M.Map String String -> Routine -> Result Routine
renameRoutine topLevel env implicit routine = do
  result <- renameType env (routinePos routine) (routineResult routine)
  parameters <- mapM (renameBinding False env) (routineParams routine)
  owner <- traverse renameOwner (routineOwner routine)
  let parameterNames = M.fromList [(bindName binding,bindName binding)
        | binding <- parameters]
      bodyEnv = env {renameLocalValues=(parameterNames `M.union` implicit):renameLocalValues env}
      name = if topLevel then ownTarget env ValueSymbol (routineName routine)
        else routineName routine
  body <- traverse (renameStmt bodyEnv) (routineBody routine)
  pure routine {routineName=name,routineResult=result,routineParams=parameters,
    routineBody=body,routineOwner=owner}
  where
    renameOwner (TypeId p name) = TypeId p <$> resolveTypeName env p name

renameTypeDecl :: Bool -> RenameEnv -> TypeDecl -> Result TypeDecl
renameTypeDecl topLevel env declaration = case declaration of
  AliasDef (TypeId p name) base ->
    AliasDef (TypeId p (typeTarget p name)) <$> renameType env p base
  EnumDef (TypeId p name) members -> do
    let targets = M.fromList [(enumName,valueTarget memberPos enumName)
          | Enumerator memberPos enumName _ <- members]
        enumEnv = env {renameLocalValues=targets:renameLocalValues env}
    renamed <- mapM (renameEnumerator enumEnv targets) members
    pure (EnumDef (TypeId p (typeTarget p name)) renamed)
  StructDef (TypeId p name) members -> do
    let target = typeTarget p name
        selfEnv = addLocalType name target env
        memberNames = M.fromList
          [(memberName member,memberName member) | member <- members]
    renamed <- mapM (renameMember selfEnv memberNames) members
    pure (StructDef (TypeId p target) renamed)
  where
    typeTarget p name = if topLevel then ownTarget env TypeSymbol name
      else localUnique env p name
    valueTarget p name = if topLevel then ownTarget env ValueSymbol name
      else localUnique env p name
    renameEnumerator enumEnv targets (Enumerator p name explicit) =
      Enumerator p (targets M.! name) <$> traverse (renameExpr enumEnv) explicit
    memberName member = case member of
      Field _ field -> bindName (declBinding field)
      Method _ method -> routineName method

renameMember :: RenameEnv -> M.Map String String -> MemberDecl -> Result MemberDecl
renameMember env names member = case member of
  Field access declaration -> Field access <$> renameDecl False env declaration
  Method access routine -> Method access <$> renameRoutine False env names routine

renameType :: RenameEnv -> Pos -> Type -> Result Type
renameType env p ty = case ty of
  TypeName name -> TypeName <$> resolveTypeName env p name
  ArrayT element count -> ArrayT <$> renameType env p element <*> pure count
  PointerT pointee -> PointerT <$> renameType env p pointee
  ReferenceT referent -> ReferenceT <$> renameType env p referent
  ConstT value -> ConstT <$> renameType env p value
  FunctionPointerT result parameters ->
    FunctionPointerT <$> renameType env p result <*> mapM (renameType env p) parameters
  NominalT ident base -> NominalT <$> renameTypeId env ident <*> renameType env p base
  EnumT ident -> EnumT <$> renameTypeId env ident
  StructT ident -> StructT <$> renameTypeId env ident
  _ -> pure ty

renameTypeId :: RenameEnv -> TypeId -> Result TypeId
renameTypeId env (TypeId p name) = TypeId p <$> resolveTypeName env p name

renameStmt :: RenameEnv -> Stmt -> Result Stmt
renameStmt env (Stmt p node) = Stmt p <$> case node of
  Block statements -> do
    (renamed,_) <- renameStatements (pushScope env) statements
    pure (Block renamed)
  Declaration declaration -> Declaration <$> renameDecl False env declaration
  Expression expressionNode -> Expression <$> renameExpr env expressionNode
  If condition yes no ->
    If <$> renameExpr env condition <*> renameStmt env yes <*> traverse (renameStmt env) no
  While condition body -> While <$> renameExpr env condition <*> renameStmt env body
  For initializer condition step body -> do
    let inner = pushScope env
    (renamedInitializer,loopEnv) <- case initializer of
      Nothing -> pure (Nothing,inner)
      Just statement -> do
        (items,next) <- renameStatements inner [statement]
        pure (case items of item:_ -> Just item; [] -> Nothing,next)
    For renamedInitializer <$> traverse (renameExpr loopEnv) condition
      <*> traverse (renameExpr loopEnv) step <*> renameStmt loopEnv body
  Return value -> Return <$> traverse (renameExpr env) value
  Switch value arms -> Switch <$> renameExpr env value <*> mapM renameArm arms
  TypeStatement declaration -> TypeStatement <$> renameTypeDecl False env declaration
  other -> pure other
  where
    renameArm arm = do
      label <- traverse (renameExpr env) (armValue arm)
      (body,_) <- renameStatements (pushScope env) (armBody arm)
      pure arm {armValue=label,armBody=body}

renameStatements :: RenameEnv -> [Stmt] -> Result ([Stmt],RenameEnv)
renameStatements env [] = pure ([],env)
renameStatements env (statement@(Stmt _ node):rest) = do
  renamed <- renameStmt env statement
  next <- case (node,renamed) of
    (Declaration declaration,_) -> pure (addLocalValue (bindName (declBinding declaration))
      (bindName (declBinding declaration)) env)
    (TypeStatement original,Stmt _ (TypeStatement changed)) ->
      pure (addLocalTypeDecl original changed env)
    _ -> pure env
  (others,end) <- renameStatements next rest
  pure (renamed:others,end)

renameExpr :: RenameEnv -> Expr -> Result Expr
renameExpr env expressionNode = do
  let go = renameExpr env
      p = exprPos expressionNode
  node <- case exprNode expressionNode of
    Variable name -> Variable <$> resolveValueName env p name
    Call name arguments -> Call <$> resolveCallName env p name <*> mapM go arguments
    Cast ty value -> Cast <$> renameType env p ty <*> go value
    Unary op value -> Unary op <$> go value
    Binary op left right -> Binary op <$> go left <*> go right
    Assign left right -> Assign <$> go left <*> go right
    Increment op prefix value -> Increment op prefix <$> go value
    ArrayAccess array index -> ArrayAccess <$> go array <*> go index
    Member base name -> Member <$> go base <*> pure name
    MemberCall base name arguments -> MemberCall <$> go base <*> pure name <*> mapM go arguments
    AddressOf value -> AddressOf <$> go value
    Dereference value -> Dereference <$> go value
    ReferenceBind value -> ReferenceBind <$> go value
    Allocate value -> Allocate <$> go value
    Free value -> Free <$> go value
    IndirectCall target arguments -> IndirectCall <$> go target <*> mapM go arguments
    ReferenceResult referent value -> ReferenceResult <$> renameType env p referent <*> go value
    RoutineAddress name -> RoutineAddress <$> resolveValueName env p name
    FieldAccess base binding -> FieldAccess <$> go base <*> renameBinding False env binding
    MethodCall base name arguments -> MethodCall <$> go base <*> pure name <*> mapM go arguments
    Receiver ident -> Receiver <$> renameTypeId env ident
    EnumLit ty value -> EnumLit <$> renameType env p ty <*> pure value
    other -> pure other
  pure expressionNode {exprNode=node}

resolveValueName :: RenameEnv -> Pos -> String -> Result String
resolveValueName env p name = case lookupScopes name (renameLocalValues env) of
  Just target -> pure target
  Nothing -> resolveExternal env p ValueSymbol name

resolveTypeName :: RenameEnv -> Pos -> String -> Result String
resolveTypeName env p name = case lookupScopes name (renameLocalTypes env) of
  Just target -> pure target
  Nothing -> resolveExternal env p TypeSymbol name

resolveCallName :: RenameEnv -> Pos -> String -> Result String
resolveCallName env p name = case lookupScopes name (renameLocalValues env) of
  Just target -> pure target
  Nothing -> case lookupScopes name (renameLocalTypes env) of
    Just target -> pure target
    Nothing -> resolveAnyExternal env p name

lookupScopes :: String -> [M.Map String String] -> Maybe String
lookupScopes _ [] = Nothing
lookupScopes name (scope:outer) = case M.lookup name scope of
  Just target -> Just target
  Nothing -> lookupScopes name outer

resolveExternal :: RenameEnv -> Pos -> SymbolKind -> String -> Result String
resolveExternal env p kind name = case M.lookup (name,kind) (renameOwn env) of
  Just symbol -> pure (symbolTarget symbol)
  Nothing -> case filter ((== kind) . symbolKind)
      (M.findWithDefault [] name (renameImports env)) of
    [] -> pure (unresolvedName env name)
    [symbol] -> pure (symbolTarget symbol)
    _ -> failure "module" p ("imported symbol is ambiguous: " ++ name)

resolveAnyExternal :: RenameEnv -> Pos -> String -> Result String
resolveAnyExternal env p name = case ownMatches of
  [symbol] -> pure (symbolTarget symbol)
  [] -> case M.findWithDefault [] name (renameImports env) of
    [] -> pure (unresolvedName env name)
    [symbol] -> pure (symbolTarget symbol)
    _ -> failure "module" p ("imported symbol is ambiguous: " ++ name)
  _ -> failure "module" p ("top-level symbol is ambiguous: " ++ name)
  where
    ownMatches = [symbol | kind <- [ValueSymbol,TypeSymbol]
      , Just symbol <- [M.lookup (name,kind) (renameOwn env)]]

ownTarget :: RenameEnv -> SymbolKind -> String -> String
ownTarget env kind name = maybe (qualified (renameIdentity env) name) symbolTarget
  (M.lookup (name,kind) (renameOwn env))

localUnique :: RenameEnv -> Pos -> String -> String
localUnique env (Pos lineNumber columnNumber) name = case renameIdentity env of
  Nothing -> name
  Just moduleName -> qualified (Just moduleName)
    ("local$" ++ show lineNumber ++ "$" ++ show columnNumber ++ "$" ++ name)

pushScope :: RenameEnv -> RenameEnv
pushScope env = env {renameLocalValues=M.empty:renameLocalValues env,
  renameLocalTypes=M.empty:renameLocalTypes env}

addLocalValue :: String -> String -> RenameEnv -> RenameEnv
addLocalValue source target env = case renameLocalValues env of
  scope:outer -> env {renameLocalValues=M.insert source target scope:outer}
  [] -> env {renameLocalValues=[M.singleton source target]}

addLocalType :: String -> String -> RenameEnv -> RenameEnv
addLocalType source target env = case renameLocalTypes env of
  scope:outer -> env {renameLocalTypes=M.insert source target scope:outer}
  [] -> env {renameLocalTypes=[M.singleton source target]}

addLocalTypeDecl :: TypeDecl -> TypeDecl -> RenameEnv -> RenameEnv
addLocalTypeDecl original changed env =
  let TypeId _ source = typeDeclId original
      TypeId _ target = typeDeclId changed
      withType = addLocalType source target env
      pairs = case (original,changed) of
        (EnumDef _ oldMembers,EnumDef _ newMembers) ->
          zipWith (\(Enumerator _ oldName _) (Enumerator _ newName _) ->
            (oldName,newName)) oldMembers newMembers
        _ -> []
  in foldr (uncurry addLocalValue) withType pairs

unresolvedName :: RenameEnv -> String -> String
unresolvedName env name
  | renameClosed env && name `notElem` ["AUSGABE","SCHREIBEN","LESEN"] =
      qualified (renameIdentity env) ("unresolved$" ++ name)
  | otherwise = name
