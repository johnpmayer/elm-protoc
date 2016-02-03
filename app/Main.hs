module Main where

import Control.Monad.Except

import Arguments
--import Lib
import ShellUtils (ensureSetup, runProtoc)

main :: IO ()
main = do
  ElmProtocArguments inputDir outputDir prefix <- arguments
  runExceptT ensureSetup >>= putStrLn . show
  putStrLn $ "Generating JavaScript with protoc"
  runProtoc inputDir outputDir prefix
