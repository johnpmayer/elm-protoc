module Lib
    ( parseProtoFile
    ) where

import Prelude hiding                             (readFile, writeFile)
    
import Data.ByteString.Lazy                       (ByteString, readFile, writeFile)
import Data.Foldable                              (toList)
import Data.Maybe                                 (fromMaybe)
import Data.Text                                  (Text)
import                                            qualified Data.Text as T
import Data.Text.Lazy                             (fromStrict, toStrict)
import Data.Text.Lazy.Encoding                    (decodeUtf8, encodeUtf8)
import System.Directory                           (createDirectoryIfMissing)
import System.FilePath                            ((</>), (<.>))
import System.IO                                  (FilePath)
import Text.DescriptorProtos.DescriptorProto      (DescriptorProto)
import                                            qualified Text.DescriptorProtos.DescriptorProto as D
import Text.DescriptorProtos.FileDescriptorProto  (FileDescriptorProto)
import                                            qualified Text.DescriptorProtos.FileDescriptorProto as FD
import Text.ProtocolBuffers.Basic                 (Utf8, utf8)
import Text.ProtocolBuffers.ProtoCompile.Parser   (parseProto)

import Templates

parseProtoFile :: FilePath -> FilePath -> IO ()
parseProtoFile filename outputDir = 
  do
    protoContents <- readFile filename
    case parseProto filename protoContents of
      Left parseError -> error $ "Failed to parse " ++ filename ++ show parseError
      Right fileDescriptor -> 
        do
          createDirectoryIfMissing True outputDir
          let packagename = toText "Did not find a package name in the .proto file" . FD.package $ fileDescriptor
          let modulename = T.toTitle packagename
          let nativeDir = outputDir </> "Native"
          createDirectoryIfMissing True nativeDir
          let nativeFile = nativeDir </> T.unpack modulename <.> "js"
          let elmFile = outputDir </> T.unpack modulename <.> "elm"
          writeFile nativeFile $ genNativeModule protoContents fileDescriptor
          writeFile elmFile $ genElmModule fileDescriptor

toText :: String -> Maybe Utf8 -> Text
toText errorMsg = toStrict . decodeUtf8 . utf8 . fromMaybe (error errorMsg) 

genNativeModule :: ByteString -> FileDescriptorProto -> ByteString
genNativeModule protoContents fileDescriptor = 
  let
    packagename :: Text
    packagename = toText "Did not find a package name in the .proto file" . FD.package $ fileDescriptor
    
    modulename :: Text
    modulename = T.toTitle packagename
    
    descriptors :: [DescriptorProto]
    descriptors = toList $ FD.message_type fileDescriptor
    
    typenames :: [Text]
    typenames = toText "Didn't find a name for the descriptor" . D.name <$> descriptors
    
    encoders :: Text
    encoders = T.concat $ nativeEncode <$> typenames
    
    decoders :: Text
    decoders = T.concat $ nativeDecode <$> typenames
    
    values :: Text
    values = T.concat [decoders, encoders]
    
    exports :: Text
    exports = T.concat $ do
      typename <- typenames
      prefix <- ["encode", "decode"]
      return . nativeModuleExport $ T.concat [T.pack prefix, typename]
    
    protoSource :: Text
    protoSource = toStrict . decodeUtf8 $ protoContents
  in encodeUtf8 . fromStrict $ nativeModule packagename modulename protoSource values exports

genElmModule :: FileDescriptorProto -> ByteString
genElmModule fileDescriptor = 
  let
    packagename :: Text
    packagename = toText "Did not find a package name in the .proto file" . FD.package $ fileDescriptor
    
    modulename :: Text
    modulename = T.toTitle packagename
    
    descriptors :: [DescriptorProto]
    descriptors = toList $ FD.message_type fileDescriptor
    
    typenames :: [Text]
    typenames = toText "Didn't find a name for the descriptor" . D.name <$> descriptors
    
    exportPrefixes :: [Text]
    exportPrefixes = T.pack "(" : repeat (T.pack ",")
    
    exports :: Text
    exports = T.concat . zipWith elmExport exportPrefixes $ concat [typeExports, valueExports]
    
    typeExports :: [Text]
    typeExports = (\x -> T.append x (T.pack "Contract")) <$> typenames
    
    valueExports :: [Text]
    valueExports = do
      typename <- typenames
      prefix <- ["encode", "decode", "marshal", "unmarshal"]
      return  $ T.concat [T.pack prefix, typename]
    
    contractTypeDefs :: Text
    contractTypeDefs = T.concat $ elmContractTypeDef <$> typenames
        
    values :: Text
    values = T.concat $ do
      typename <- typenames
      valueBuilder <- [elmEncode, elmDecode, elmMarshal, elmUnmarshal]
      return $ valueBuilder modulename typename

  in encodeUtf8 . fromStrict $ elmModule modulename exports contractTypeDefs values
    