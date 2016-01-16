module Lib
    ( parseProtoFile
    ) where

import Prelude hiding                             (readFile, putStr)
    
import Data.ByteString.Lazy                       (ByteString, readFile, putStr)
import Data.Foldable                              (toList)
import Data.Maybe                                 (fromMaybe)
import Data.Text                                  (Text)
import                                            qualified Data.Text as T
import Data.Text.Lazy                             (fromStrict, toStrict)
import Data.Text.Lazy.Encoding                    (decodeUtf8, encodeUtf8)
import System.IO                                  (FilePath)
import Text.DescriptorProtos.DescriptorProto      (DescriptorProto)
import                                            qualified Text.DescriptorProtos.DescriptorProto as D
import Text.DescriptorProtos.FileDescriptorProto  (FileDescriptorProto)
import                                            qualified Text.DescriptorProtos.FileDescriptorProto as FD
import Text.ProtocolBuffers.Basic                 (utf8)
import Text.ProtocolBuffers.ProtoCompile.Parser   (parseProto)

import Templates

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

messageTypename :: DescriptorProto -> Text
messageTypename = toStrict . decodeUtf8 . utf8 . fromMaybe (error "Didn't find a name for the descriptor") . D.name

genNativeModule :: ByteString -> FileDescriptorProto -> ByteString
genNativeModule protoContents fileDescriptor = 
  let
    packagename :: Text
    packagename = toStrict . decodeUtf8 . utf8 . fromMaybe (error "Did not find a package name in the .proto file") $ FD.package fileDescriptor
    descriptors :: [DescriptorProto]
    descriptors = toList $ FD.message_type fileDescriptor
    encoders :: Text
    encoders = T.concat $ do
      descriptor <- descriptors
      let typename = messageTypename descriptor
      return . encodeNative $ typename
    decoders :: Text
    decoders = T.concat $ do
      descriptor <- descriptors
      let typename = messageTypename descriptor
      return . decodeNative $ typename
    values :: Text
    values = T.concat [decoders, encoders]
    exports :: Text
    exports = T.concat $ do
      descriptor <- toList descriptors
      let typename = messageTypename descriptor
      prefix <- ["encode", "decode"]
      return . nativeModuleExport $ T.concat [T.pack prefix, typename]
    modulename :: Text
    modulename = T.toTitle packagename
    protoSource :: Text
    protoSource = toStrict . decodeUtf8 $ protoContents
  in encodeUtf8 . fromStrict $ nativeModule packagename modulename protoSource values exports
