{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ShellUtils where

import Control.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Array ((!))
import Data.List.Utils (endswith)
import qualified Data.Text as T
import NeatInterpolation (text)
import Shelly (Sh, findWhen, fromText, hasExt, shelly, toTextIgnore)
import qualified Shelly as Sh
import System.Directory
import System.FilePath
import System.Process
import Text.Regex.Base (matchAllText, makeRegex)
import Text.Regex.PCRE.String (Regex)

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

-- TODO NOT DRY
ensureNativeOutputDirExists :: FilePath -> String ->  IO ()
ensureNativeOutputDirExists outputDir prefix =
  let
    nativeOutputDir = outputDir </> "Native" </> prefix </> "Internal"
  in
    createDirectoryIfMissing True nativeOutputDir

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
    (provide_regex :: Regex) = makeRegex ("^\\s*goog\\.provide\\(\\s*['\"](.+)['\"]\\s*\\)" :: String)
    matches = matchAllText provide_regex contents
  in map (fst . (! 1)) matches

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
    putStrLn "Finding dependencies"
    let provided = parseAllProvided protoc_contents
    whenM (doesFileExist deps_output_file) $ do
      putStrLn "Removing old deps output file"
      removeFile deps_output_file
    writeFile deps_output_file . unlines $
      wrapper_provided : "" : map make_require provided

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

addNativeModuleWrapper :: String -> String -> String
addNativeModuleWrapper prefix content =
  let
    proto_modulename = T.pack $ "Native." ++ prefix
    contentT = T.pack content
    prefixT = T.pack prefix
  in
    T.unpack $ [text|
      Elm.${proto_modulename} = Elm.${proto_modulename} || {};
      Elm.${proto_modulename}.Internal = Elm.${proto_modulename}.Internal || {};
      Elm.${proto_modulename}.Internal.Proto = Elm.${proto_modulename}.Internal.Proto || {};
      Elm.${proto_modulename}.Internal.Proto.make = function(_elm) {
        "use strict";
        _elm.${proto_modulename} = _elm.${proto_modulename} || {};
        _elm.${proto_modulename}.Internal = _elm.${proto_modulename}.Internal || {};
        _elm.${proto_modulename}.Internal.Proto = _elm.${proto_modulename}.Internal.Proto || {};
        if (_elm.${proto_modulename}.Internal.Proto.values) {
          return _elm.${proto_modulename}.Internal.Proto.values;
        }

        ${contentT}

        return _elm.${proto_modulename}.Internal.Proto.values = proto;
      }
    |]

runClosureCompiler :: FilePath -> String -> IO ()
runClosureCompiler outputDir prefix = do
  putStrLn "Minifying protoc generated code & dependencies"
  compiler_output <- readProcess "java"
    [ "-jar", closure_compiler_jar
    , "--dependency_mode", "STRICT"
    , "--entry_point", "goog:" ++ prefix
    , temp_js_out_dir
    , temp_protobuf_js_include_dir
    , closure_library_include_dir
    ] ""
  let nativeModuleSrc = addNativeModuleWrapper prefix compiler_output
  ensureNativeOutputDirExists outputDir prefix
  let nativeOutputFile = outputDir </> "Native" </> prefix </> "Internal" </> "Proto" <.> "js"
  putStrLn $ "Writing proto JS to: " ++ nativeOutputFile
  writeFile nativeOutputFile nativeModuleSrc
