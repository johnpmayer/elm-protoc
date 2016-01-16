module Lib
    ( parseProtoFile
    ) where

import Prelude hiding                           (readFile)
    
import Data.ByteString.Lazy                     (readFile)
import System.IO                                (FilePath)
import Text.ProtocolBuffers.ProtoCompile.Parser (parseProto)

parseProtoFile :: FilePath -> IO ()
parseProtoFile filename = do
  contents <- readFile filename
  case parseProto filename contents of
    Left _ -> error $ "Failed to parse " ++ filename
    Right fileDescriptor -> putStrLn (show fileDescriptor)
  
