{-# LANGUAGE FlexibleContexts #-}
module ShellUtils where

--import Shelly

import Control.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO, liftIO)
import System.Directory

import Constants
import Utils.Http as Http
import Utils.Zip as Zip

-- Common

ensureTempDirExists :: IO ()
ensureTempDirExists = createDirectoryIfMissing True tempDir

-- Installing Protoc

isProtocMissing :: IO Bool
isProtocMissing = not <$> doesFileExist protoc_exe

whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = test >>= \b -> when b action

ensureProtocAvailable :: (MonadIO m, MonadError String m) => m ()
ensureProtocAvailable =
  whenM (liftIO isProtocMissing) $ do
    liftIO $ putStrLn "Downloading protocol buffers compiler"
    Http.send protoc_url (Zip.extract protoc_dir)
