module Main where

import Control.Monad.Except
--import Shelly

import Arguments
--import Constants
--import Lib
import ShellUtils (ensureSetup)

main :: IO ()
main = do
  args <- arguments
  runExceptT ensureSetup >>= putStrLn . show
  putStrLn $ "GO: " ++ show args
