{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ShellUtils (ensureSetup, runProtoc) where

import Control.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Text as T
import Shelly (Sh, echo, findWhen, fromText, hasExt, run_, shelly, toTextIgnore)
import qualified Shelly as Sh
import System.Directory
import System.Process

import Constants
import Utils.Http as Http
import Utils.Zip as Zip

ensureSetup :: (MonadIO m, MonadError String m) => m ()
ensureSetup =
  do
    liftIO $ ensureTempDirExists
    ensureProtocAvailable
    ensureProtoJSAvailable
    ensureClosureAvailable

-- Common

ensureTempDirExists :: IO ()
ensureTempDirExists = createDirectoryIfMissing True tempDir

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

isProtoJSMising :: IO Bool
isProtoJSMising = not <$> doesDirectoryExist protobuf_js_include_dir

ensureProtoJSAvailable :: (MonadIO m, MonadError String m) => m ()
ensureProtoJSAvailable =
  whenM (liftIO isProtoJSMising) $ do
    liftIO $ putStrLn "Downloading Protocol Buffers JavaScript imports from GitHub (release archive)"
    Http.send protobuf_js_url (Zip.extract protobuf_js_dir)

-- Installing Closure Library

isClosureMissing :: IO Bool
isClosureMissing = not <$> doesDirectoryExist closure_library_include_dir -- AND MORE CHECKS ??

ensureClosureAvailable :: (MonadIO m, MonadError String m) => m ()
ensureClosureAvailable =
  whenM (liftIO isClosureMissing) $ liftIO $ do
    putStrLn "Cloning Closure Library repository from GitHub (git)"
    -- TODO pin a version
    callCommand $ unwords ["git", "clone", closure_library_repository, closure_library_dir]

-- Invoking Protocol Buffers Compiler

getProtoFiles :: Sh.FilePath -> Sh [Sh.FilePath]
getProtoFiles = findWhen (return . hasExt "proto")

runProtoc :: FilePath -> FilePath -> String -> IO ()
runProtoc sourceDirectory destinationDirectory prefix = shelly $ do
  inputfiles <- (<$>) toTextIgnore <$> getProtoFiles (convertToShFilePath sourceDirectory)
  forM_ inputfiles (echo . ("  with input file: " `T.append`))
  let protoc_filepath = convertToShFilePath protoc_exe
  run_ protoc_filepath $ inputfiles ++
    [ "--proto_path"
    , T.pack sourceDirectory
    , T.concat ["--js_out=binary,namespace_prefix=", T.pack prefix, ":", T.pack destinationDirectory ]
    ]
