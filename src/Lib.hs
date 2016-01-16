module Lib
    ( parseProtoFile
    ) where

import Prelude hiding                             (readFile, putStr)
    
import Data.ByteString.Lazy                       (ByteString, readFile, putStr)
import Data.Maybe                                 (fromJust)
import Data.Text.Lazy                             (fromStrict, toStrict)
import Data.Text.Lazy.Encoding                    (decodeUtf8, encodeUtf8)
import System.IO                                  (FilePath)
import Text.DescriptorProtos.FileDescriptorProto  (FileDescriptorProto, package)
import Text.ProtocolBuffers.Basic                 (utf8)
import Text.ProtocolBuffers.ProtoCompile.Parser   (parseProto)

import Templates                                  (nativeModule)

parseProtoFile :: FilePath -> IO ()
parseProtoFile filename = 
  do
    protoContents <- readFile filename
    case parseProto filename protoContents of
      Left _ -> error $ "Failed to parse " ++ filename
      Right fileDescriptor -> 
        do
          putStrLn (show fileDescriptor)
          putStr $ genNativeModule protoContents fileDescriptor
          

genNativeModule :: ByteString -> FileDescriptorProto -> ByteString
genNativeModule protoContents fileDescriptor = 
  let
    packagename = toStrict . decodeUtf8 . utf8 . fromJust $ package fileDescriptor
    modulename = packagename
    protoSource = toStrict . decodeUtf8 $ protoContents
  in encodeUtf8 . fromStrict $ nativeModule packagename modulename protoSource