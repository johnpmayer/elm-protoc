module Main where

import Shelly

import Arguments
import Constants
import Lib
import ShellUtils

main :: IO ()
main = do
  args <- arguments
  ensureTempDirExists
  putStrLn $ "TODO" ++ show args
