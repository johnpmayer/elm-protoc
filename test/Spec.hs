
import Control.Monad (forM_)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), takeExtension)

import Lib (parseProtoFile)

examplesDir :: FilePath
examplesDir = "examples"

outputDir :: FilePath
outputDir = "contracts"

main :: IO ()
main = do 
  examples <- (map (examplesDir </>) . filter (((==) ".proto") . takeExtension)) <$> getDirectoryContents examplesDir
  putStrLn . show $ examples
  forM_ examples $ \example -> parseProtoFile example outputDir
