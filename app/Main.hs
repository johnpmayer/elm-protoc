module Main where

import Control.Monad.Except
--import Shelly

import Arguments
--import Constants
--import Lib
import ShellUtils

main :: IO ()
main = do
  args <- arguments
  ensureTempDirExists
  runExceptT ensureProtocAvailable >>= putStrLn . show
  putStrLn $ "TODO\n" ++ show args
