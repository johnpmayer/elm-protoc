module Main where

import Control.Monad.Except

import Arguments
import ShellUtils (ensureSetup, runProtoc, runDepsWriter, runClosureBuilder)

main :: IO ()
main = do
  ElmProtocArguments inputDir outputDir prefix <- arguments
  runExceptT ensureSetup >>= putStrLn . show
  putStrLn $ "Generating JavaScript with protoc"
  runProtoc inputDir prefix
  runDepsWriter
  runClosureBuilder outputDir prefix
