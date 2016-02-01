
module Arguments where

import Options.Applicative

data ElmProtocArguments = ElmProtocArguments
  { inputDir :: FilePath
  , outputDir :: FilePath
  , modulePrefix :: String } deriving (Show)

argumentsP :: Parser ElmProtocArguments
argumentsP = ElmProtocArguments
  <$> strOption
    ( long "input-directory" <>
      help "Directory where to find *.proto files" )
  <*> strOption
    ( long "output-directory" <>
      help "Directory where to write *.elm & Native/*.js files" )
  <*> strOption
    ( long "module-prefix" <>
      help "Path with which generated modules will begin" )

arguments :: IO ElmProtocArguments
arguments =
  let
    opts = info (helper <*> argumentsP)
      ( fullDesc <>
        progDesc "Print a greeting for TARGET" <>
        header "hello - a test for optparse-applicative" )
  in execParser opts
