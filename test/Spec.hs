
import Lib (parseProtoFile)

oneofFile :: FilePath
oneofFile = "examples/addressbook.proto"

main :: IO ()
main = do
  parseProtoFile oneofFile
