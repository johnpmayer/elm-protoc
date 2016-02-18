module Main where

import Control.Monad.Except

import Arguments
import ShellUtils

main :: IO ()
main = do
  ElmProtocArguments inputDir outputDir prefix <- arguments
  runExceptT ensureSetup >>= putStrLn . show
  putStrLn $ "Generating JavaScript with protoc"
  runProtoc inputDir prefix
  copyProtobufJsIncludes
  runDepsWriter
  runClosureBuilder outputDir prefix
