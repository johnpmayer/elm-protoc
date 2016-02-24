module Main where

import Control.Monad.Except

import Arguments
import Lib
import ShellUtils

main :: IO ()
main = do
  ElmProtocArguments inputDir outputDir prefix <- arguments
  runExceptT ensureSetup >>= putStrLn . show
  putStrLn $ "Generating JavaScript with protoc"
  runProtoc inputDir prefix
  runDepsGenerator prefix
  copyProtobufJsIncludes
  runClosureCompiler outputDir prefix
  inputFiles <- getProtoFiles inputDir
  putStrLn $ "Generating Elm and Native Wrappers"
  forM_ inputFiles $ \protoFile -> parseProtoFile prefix protoFile outputDir
  
