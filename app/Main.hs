module Main where

import Arguments
import Constants
import Lib

main :: IO ()
main = do
  args <- arguments
  putStrLn $ "TODO" ++ show args
