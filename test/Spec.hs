
import Lib (parseProtoFile)

example :: FilePath
example = "examples/addressbook.proto"

outputDir :: FilePath
outputDir = "contracts"

main :: IO ()
main = do
  parseProtoFile example outputDir
