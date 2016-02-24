{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ShellUtils where

import Control.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List.Utils (endswith)
import qualified Data.Text as T
import Shelly (Sh, findWhen, fromText, hasExt, shelly, toTextIgnore)
import qualified Shelly as Sh
import System.Directory
import System.FilePath
import System.Process
import Text.Regex

import Constants
import Utils.Http as Http
import Utils.Zip as Zip

ensureSetup :: (MonadIO m, MonadError String m) => m ()
ensureSetup =
  do
    liftIO $ ensureTempDirExists
    ensureProtocAvailable
    ensureProtoJSAvailable
    ensureClosureLibraryAvailable
    ensureClosureCompilerAvailable

-- Common

ensureTempDirExists :: IO ()
ensureTempDirExists = do
    createDirectoryIfMissing True temp_dir
    whenM (doesDirectoryExist temp_js_out_dir) $ 
      removeDirectoryRecursive temp_js_out_dir
    createDirectoryIfMissing True temp_js_out_dir
    createDirectoryIfMissing True temp_protobuf_js_include_dir

whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = test >>= \b -> when b action

convertToShFilePath :: FilePath -> Sh.FilePath
convertToShFilePath = fromText . T.pack

-- Installing Protoccol Buffers

isProtocMissing :: IO Bool
isProtocMissing = not <$> doesFileExist protoc_exe

ensureProtocAvailable :: (MonadIO m, MonadError String m) => m ()
ensureProtocAvailable =
  whenM (liftIO isProtocMissing) $ do
    liftIO $ putStrLn "Downloading Protocol Buffers Compiler binary from GitHub (release archive)"
    Http.send protoc_url (Zip.extract protoc_dir)
    -- TODO break this down into a separate check? stronger idempotency!
    liftIO $ do
      perms <- getPermissions protoc_exe
      setPermissions protoc_exe (setOwnerExecutable True perms)

isProtoJSMising :: IO Bool
isProtoJSMising = not <$> doesDirectoryExist protobuf_js_include_dir

ensureProtoJSAvailable :: (MonadIO m, MonadError String m) => m ()
ensureProtoJSAvailable =
  whenM (liftIO isProtoJSMising) $ do
    liftIO $ putStrLn "Downloading Protocol Buffers JavaScript imports from GitHub (release archive)"
    Http.send protobuf_js_url (Zip.extract protobuf_js_dir)

-- Installing Closure Library

isClosureLibraryMissing :: IO Bool
isClosureLibraryMissing = not <$> doesDirectoryExist closure_library_include_dir -- AND MORE CHECKS ??

ensureClosureLibraryAvailable :: (MonadIO m, MonadError String m) => m ()
ensureClosureLibraryAvailable =
  whenM (liftIO isClosureLibraryMissing) $ liftIO $ do
    putStrLn "Cloning Closure Library repository from GitHub (git)"
    -- TODO pin a version
    callCommand $ unwords ["git", "clone", closure_library_repository, closure_library_dir]

-- Installing Closure Compiler

isClosureCompilerMissing :: IO Bool
isClosureCompilerMissing = not <$> doesFileExist closure_compiler_jar

ensureClosureCompilerAvailable :: (MonadIO m, MonadError String m) => m ()
ensureClosureCompilerAvailable =
  whenM (liftIO isClosureCompilerMissing) $ do
    liftIO $ putStrLn "Downloading Closure Compiler Jar from google"
    Http.send closure_compiler_url (Zip.extract closure_compiler_dir)

-- Invoking Protocol Buffers Compiler

getProtoFilesSh :: Sh.FilePath -> Sh [Sh.FilePath]
getProtoFilesSh = findWhen (return . hasExt "proto")

getProtoFiles :: FilePath -> IO [FilePath]
getProtoFiles sourceDirectory = shelly $
  ((T.unpack . toTextIgnore) <$>) <$> getProtoFilesSh (convertToShFilePath sourceDirectory)

runProtoc :: FilePath -> String -> IO ()
runProtoc sourceDirectory prefix = do
  inputfiles <- getProtoFiles sourceDirectory
  forM_ inputfiles (putStrLn . ("  with input file: " ++))
  callCommand . unwords $
    [ protoc_exe ] ++
    inputfiles ++
    [ "--proto_path"
    , sourceDirectory
    , concat ["--js_out=binary,library=", prefix, ":", temp_js_out_dir ]
    ]

parseAllProvided :: String -> [String]
parseAllProvided contents =
  let 
    provide_regex = mkRegex "^\\s*goog\\.provide\\(\\s*['\"](.+)['\"]\\s*\\)"
  in case matchRegexAll provide_regex contents of
    Just (_bef, _mat, rest, [provided]) -> 
      provided : parseAllProvided rest
    Just (_bef, _mat, rest, _) -> 
      parseAllProvided rest -- this case really shouldn't...
    Nothing -> []

runDepsGenerator :: String -> IO ()
runDepsGenerator prefix = 
  let 
    protoc_output_file = temp_js_out_dir </> prefix <.> "js"
    deps_output_file = temp_js_out_dir </> prefix ++ "_Deps" <.> "js"
    wrapper_provided = "goog.provide('" ++ prefix ++ "');"
    make_require req = "goog.require('" ++ req ++ "');"
  in do
    putStrLn $ "Reading from: " ++ protoc_output_file
    protoc_contents <- readFile protoc_output_file
    let provided = parseAllProvided protoc_contents
    whenM (doesFileExist deps_output_file) $ do
      putStrLn "Removing old deps output file"
      removeFile deps_output_file
    writeFile deps_output_file . unlines $ 
      wrapper_provided : "" : map make_require provided

runDepsWriter :: IO ()
runDepsWriter =
  callCommand $ unwords
    [ "python"
    , depswriter_script
    , "--output_file=" ++ js_deps_file
    , "--root=" ++ temp_js_out_dir
    ]

nonTestJsFile :: FilePath -> Bool
nonTestJsFile filename =
  (takeExtension filename == ".js") &&
  not (endswith "_test" $ takeBaseName filename)

copyFileInDirectory :: FilePath -> FilePath -> FilePath -> IO ()
copyFileInDirectory sourceDir destDir filename =
  let
    sourcePath = sourceDir </> filename
    destPath = destDir </> filename
  in copyFile sourcePath destPath

copyProtobufJsIncludes :: IO ()
copyProtobufJsIncludes = do
  let messageFilename = "message.js"
  copyFileInDirectory protobuf_js_include_dir temp_protobuf_js_include_dir messageFilename
  binaryFilenames <- filter nonTestJsFile <$> getDirectoryContents protobuf_js_binary_include_dir
  forM_ binaryFilenames $ copyFileInDirectory protobuf_js_binary_include_dir temp_protobuf_js_include_dir

runClosureBuilder :: FilePath -> String -> IO ()
runClosureBuilder outputDir prefix = do
  let nativeOutputDirectory = outputDir </> "Native" </> prefix
  createDirectoryIfMissing True nativeOutputDirectory
  callCommand $ unwords
    [ "python"
    , closurebuilder_script
    , "--output_file=" ++ (nativeOutputDirectory </> native_js_out_filename)
    , "--output_mode=script"
    , "--root=" ++ temp_js_out_dir
    , "--root=" ++ temp_protobuf_js_include_dir
    , "--root=" ++ closure_library_include_dir
    , "--root=" ++ closure_library_third_party_include_dir
    , "--input=" ++ js_deps_file
    , "--namespace=\"" ++ prefix ++ "\""
    ]

runClosureCompiler :: String -> IO ()
runClosureCompiler prefix = do
  putStrLn "Minifying protoc generated code & dependencies"
  callCommand $ unwords
    [ "java"
    , "-jar", closure_compiler_jar
    , "--dependency_mode", "STRICT"
    , "--entry_point", "goog:" ++ prefix
    , temp_js_out_dir
    , temp_protobuf_js_include_dir
    , closure_library_include_dir
    ]
