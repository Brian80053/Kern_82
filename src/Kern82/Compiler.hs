module Kern82.Compiler
  ( compile, frontend, compileFile, frontendFile, compileFileWithSearch, frontendFileWithSearch
  , renderDiagnostic, renderSourceDiagnostic
  ) where

import Kern82.AST
import Kern82.Lexer
import Kern82.Modules
import Kern82.Parser
import Kern82.Semantic
import Kern82.TypeCheck
import Kern82.Codegen
import Kern82.Types (elaborateTypes)

frontend :: String -> Result Program
frontend source = do
  tokens <- lexSource source
  tree <- parseProgram tokens
  frontendProgram tree

frontendProgram :: Program -> Result Program
frontendProgram tree = do
  expanded <- elaborateTypes tree
  (resolved,signatures) <- analyze expanded
  checkProgram signatures resolved

compile :: String -> Result String
compile source = generateC <$> frontend source

frontendFile :: FilePath -> IO (Either SourceDiagnostic Program)
frontendFile = frontendFileWithSearch []

frontendFileWithSearch :: [FilePath] -> FilePath -> IO (Either SourceDiagnostic Program)
frontendFileWithSearch searchPaths path = do
  loaded <- loadModuleProgramWithSearch searchPaths path
  case loaded of
    Left issue -> pure (Left issue)
    Right (source,tree) -> case frontendProgram tree of
      Left diagnostic@(InstantiationDiagnostic (GenericOrigin _ _ (Just templateFile)) _) -> do
        templateSource <- readUtf8 templateFile
        pure (Left (SourceDiagnostic templateFile templateSource diagnostic))
      Left diagnostic -> pure (Left (SourceDiagnostic path source diagnostic))
      Right checked -> pure (Right checked)

compileFile :: FilePath -> IO (Either SourceDiagnostic String)
compileFile path = fmap (fmap generateC) (frontendFile path)

compileFileWithSearch :: [FilePath] -> FilePath -> IO (Either SourceDiagnostic String)
compileFileWithSearch searchPaths path = fmap (fmap generateC) (frontendFileWithSearch searchPaths path)

renderDiagnostic :: FilePath -> String -> Diagnostic -> String
renderDiagnostic file source (InstantiationDiagnostic (GenericOrigin name arguments _) issue) =
  "while instantiating " ++ name ++ "(" ++ comma arguments ++ "):\n" ++
  renderDiagnostic file source issue
  where
    comma [] = ""
    comma [t] = typeName t
    comma (t:ts) = typeName t ++ ", " ++ comma ts
renderDiagnostic file source (Diagnostic phase (Pos l c) message) =
  file ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ phase ++ " error: " ++ message ++ "\n" ++
  case drop (l-1) (lines source) of
    s:_ -> s ++ "\n" ++ map (\ch -> if ch == '\t' then '\t' else ' ') (take (c-1) s) ++ "^"
    [] -> ""

renderSourceDiagnostic :: SourceDiagnostic -> String
renderSourceDiagnostic (SourceDiagnostic file source diagnostic) =
  renderDiagnostic file source diagnostic
