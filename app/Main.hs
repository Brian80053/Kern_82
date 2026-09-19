module Main (main) where

import Control.Exception
  ( AsyncException, Handler(..), IOException, SomeException, bracket, catches
  , displayException, fromException, onException, throwIO )
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Version (showVersion)
import System.Directory
  ( canonicalizePath, copyFile, createDirectory, doesFileExist, findExecutable
  , getTemporaryDirectory, makeAbsolute, removeDirectoryRecursive, removeFile )
import System.Environment (getArgs, getExecutablePath, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath
  ( (</>), isPathSeparator, replaceExtension, takeBaseName, takeDirectory, takeExtension )
import System.Info (os)
import System.IO
import System.Process (rawSystem)
import Kern82.Compiler
import Paths_kern82 (version)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  status <- start `catches` [Handler ioFailure, Handler unexpectedFailure]
  exitWith status
  where
    start = do
      args <- getArgs
      executable <- getExecutablePath
      name <- getProgName
      let standard = takeDirectory (takeDirectory executable) </> "stdlib"
          legacy = map toLower (takeBaseName name) == "kern82"
      dispatch legacy [standard] args

ioFailure :: IOException -> IO ExitCode
ioFailure e = failure 1 ("kern: I/O error: " ++ displayException e)

unexpectedFailure :: SomeException -> IO ExitCode
unexpectedFailure e = case fromException e :: Maybe AsyncException of
  Just interrupt -> throwIO interrupt
  Nothing -> failure 3 ("kern: internal error: " ++ displayException e)

failure :: Int -> String -> IO ExitCode
failure code message = hPutStrLn stderr message >> pure (ExitFailure code)

dispatch :: Bool -> [FilePath] -> [String] -> IO ExitCode
dispatch _ _ ["--help"] = putStrLn usage >> pure ExitSuccess
dispatch _ _ ["--version"] = putStrLn ("Kern 82 Compiler " ++ showVersion version) >> pure ExitSuccess
dispatch _ paths ["run",path] = native paths path Nothing
dispatch _ paths ["build",path] = native paths path (Just (replaceExtension path executableExtension))
dispatch _ paths ["build",path,option,output]
  | option `elem` ["-o","--output"] = native paths path (Just output)
dispatch _ paths [command,path]
  | command `elem` ["check","--check","--dump-ast"] = withSource path $ \source -> do
      result <- frontendFileWithSearch paths source
      case result of
        Left issue -> failure 1 (renderSourceDiagnostic issue)
        Right tree -> do
          if command == "--dump-ast" then print tree else putStrLn "OK"
          pure ExitSuccess
dispatch _ paths [path,"-o",output]
  | not (reserved path) = emit paths path (Just output)
dispatch legacy paths [path]
  | not (reserved path) = do
      exists <- doesFileExist path
      if legacy then emit paths path Nothing
      else if exists || not (null (takeExtension path)) || any isPathSeparator path
        then native paths path Nothing
        else failure 2 ("kern: unknown command: " ++ path ++ "\n" ++ usage)
dispatch _ _ _ = failure 2 usage

reserved :: String -> Bool
reserved word = word `elem` ["run","build","check"] || "-" `isPrefixOf` word

executableExtension :: String
executableExtension = if os == "mingw32" then "exe" else ""

usage :: String
usage = unlines
  [ "Kern 82 compiler"
  , ""
  , "Usage:"
  , "  kern <file>                       Run a program"
  , "  kern run <file>"
  , "  kern build <file> [-o <output>]   Build beside source; also --output"
  , "  kern check <file>                 Check without gcc"
  , ""
  , "Options:"
  , "  --help"
  , "  --version"
  , ""
  , "Compatibility:"
  , "  kern82 <file> [-o <output.c>]     Generate C99"
  , "  kern <file> -o <output.c>         Generate C99"
  , "  kern --check <file>"
  , "  kern --dump-ast <file>"
  ]

withSource :: FilePath -> (FilePath -> IO ExitCode) -> IO ExitCode
withSource path action = do
  exists <- doesFileExist path
  if exists then makeAbsolute path >>= action
  else failure 1 ("kern: source file not found: " ++ path)

emit :: [FilePath] -> FilePath -> Maybe FilePath -> IO ExitCode
emit paths path output = withSource path $ \source -> do
  result <- compileFileWithSearch paths source
  case result of
    Left issue -> failure 1 (renderSourceDiagnostic issue)
    Right code -> do
      case output of
        Nothing -> putStr code
        Just target -> writeUtf8 target code
      pure ExitSuccess

native :: [FilePath] -> FilePath -> Maybe FilePath -> IO ExitCode
native paths path output = withSource path $ \source -> do
  result <- compileFileWithSearch paths source
  case result of
    Left issue -> failure 1 (renderSourceDiagnostic issue)
    Right code -> do
      compiler <- findExecutable "gcc"
      case compiler of
        Nothing -> failure 1 "kern: gcc was not found in PATH"
        Just gcc -> withWorkspace $ \workspace -> do
          let generated = workspace </> "program.c"
              binary = workspace </> if os == "mingw32" then "program.exe" else "program"
          writeUtf8 generated code
          compiled <- rawSystem gcc ["-std=c99", "-pedantic-errors", "-O2", generated, "-lm", "-o", binary]
          case compiled of
            ExitFailure n -> failure 1 ("kern: gcc failed (exit " ++ show n ++ ")")
            ExitSuccess -> case output of
              Nothing -> rawSystem binary []
              Just target -> do
                destination <- canonicalizePath target
                original <- canonicalizePath source
                if destination == original
                  then failure 2 "kern: build output must differ from the source file"
                  else copyFile binary target >> pure ExitSuccess

writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path contents = withFile path WriteMode $ \handle ->
  hSetEncoding handle utf8 >> hPutStr handle contents

-- Reserve a unique name with openTempFile and keep the reservation until release.
-- All gcc artifacts live in its sibling directory, never in the user's project.
withWorkspace :: (FilePath -> IO a) -> IO a
withWorkspace action = bracket acquire release (action . snd)
  where
    acquire = do
      temporary <- getTemporaryDirectory >>= makeAbsolute
      (reservation,handle) <- openTempFile temporary "kern-"
      let workspace = reservation ++ ".d"
      (hClose handle >> createDirectory workspace >> pure (reservation,workspace))
        `onException` removeFile reservation
    release (reservation,workspace) = do
      removeDirectoryRecursive workspace `onException` removeFile reservation
      removeFile reservation
