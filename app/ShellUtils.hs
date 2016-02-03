{-# LANGUAGE FlexibleContexts #-}
module ShellUtils (ensureSetup) where

--import Shelly

import Control.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO, liftIO)
import System.Directory

import Constants
import Utils.Http as Http
import Utils.Zip as Zip

ensureSetup :: (MonadIO m, MonadError String m) => m ()
ensureSetup =
  do
    liftIO $ ensureTempDirExists
    ensureProtocAvailable
    ensureProtoJSAvailable

-- Common

ensureTempDirExists :: IO ()
ensureTempDirExists = createDirectoryIfMissing True tempDir

whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = test >>= \b -> when b action

-- Installing Protoccol Buffers

isProtocMissing :: IO Bool
isProtocMissing = not <$> doesFileExist protoc_exe

ensureProtocAvailable :: (MonadIO m, MonadError String m) => m ()
ensureProtocAvailable =
  whenM (liftIO isProtocMissing) $ do
    liftIO $ putStrLn "Downloading protocol buffers compiler"
    Http.send protoc_url (Zip.extract protoc_dir)

isProtoJSMising :: IO Bool
isProtoJSMising = not <$> doesDirectoryExist protobuf_js_include_dir

ensureProtoJSAvailable :: (MonadIO m, MonadError String m) => m ()
ensureProtoJSAvailable =
  whenM (liftIO isProtoJSMising) $ do
    liftIO $ putStrLn "Downloading protocol buffers JavaScript imports"
    Http.send protobuf_js_url (Zip.extract protobuf_js_dir)

-- Installing Closure Library
