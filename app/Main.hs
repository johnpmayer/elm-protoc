module Main where

import Control.Monad.Except

import Arguments
import Constants
--import Lib
import ShellUtils (ensureSetup, runProtoc)

main :: IO ()
main = do
  ElmProtocArguments inputDir _ prefix <- arguments
  runExceptT ensureSetup >>= putStrLn . show
  putStrLn $ "Generating JavaScript with protoc"
  runProtoc inputDir temp_js_out_dir prefix
