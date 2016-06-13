module Main where

import Control.Monad.Except

import Arguments
import Lib
import ShellUtils

main :: IO ()
main = do
  ElmProtocArguments inputDir outputDir owner project prefix <- arguments
  runExceptT ensureSetup >>= putStrLn . show
  putStrLn $ "Generating JavaScript with protoc"
  runProtoc inputDir prefix
  runDepsGenerator prefix
  copyProtobufJsIncludes
  runClosureCompiler outputDir owner project prefix
  inputFiles <- getProtoFiles inputDir
  putStrLn $ "Generating Elm and Native Wrappers"
  forM_ inputFiles $ \protoFile -> parseProtoFile owner project prefix protoFile outputDir
