
module ShellUtils where

--import Shelly

import Control.Monad
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

ensureProtocAvailable :: IO ()
ensureProtocAvailable = do
  whenM isProtocMissing $ do
    return ()
    
