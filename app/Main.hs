module Main where

import Control.Monad.Except

import Arguments
import ShellUtils

main :: IO ()
main = do
  ElmProtocArguments inputDir _outputDir prefix <- arguments
  --inputFiles <- getProtoFiles inputDir
  runExceptT ensureSetup >>= putStrLn . show
  putStrLn $ "Generating JavaScript with protoc"
  runProtoc inputDir prefix
  runDepsGenerator prefix
  copyProtobufJsIncludes
  --runDepsWriter
  --runClosureBuilder outputDir prefix
  runClosureCompiler prefix
