module Lib
    ( parseProtoFile
    ) where

import Prelude hiding                             (readFile)
    
import Data.ByteString.Lazy                       (ByteString, readFile)
import System.IO                                  (FilePath)
import Text.DescriptorProtos.FileDescriptorProto  (FileDescriptorProto)
import Text.ProtocolBuffers.ProtoCompile.Parser   (parseProto)

import Templates

parseProtoFile :: FilePath -> IO ()
parseProtoFile filename = do
  protoContents <- readFile filename
  case parseProto filename protoContents of
    Left _ -> error $ "Failed to parse " ++ filename
    Right fileDescriptor -> putStrLn (show fileDescriptor)

genNativeModule :: ByteString -> FileDescriptorProto -> ByteString
genNativeModule protoContents fileDescriptor = undefined