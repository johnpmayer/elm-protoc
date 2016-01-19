{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseProtoFile
    ) where

import Prelude hiding                             (readFile, writeFile)
    
import            Data.ByteString.Lazy                        (ByteString, readFile, writeFile)
import            Data.Foldable                               (toList)
import            Data.Maybe                                  (fromMaybe)
import            Data.Text                                   (Text)
import qualified  Data.Text                                   as T
import            Data.Text.Lazy                              (fromStrict, toStrict)
import            Data.Text.Lazy.Encoding                     (decodeUtf8, encodeUtf8)
import            System.Directory                            (createDirectoryIfMissing)
import            System.FilePath                             ((</>), (<.>))
import            System.IO                                   (FilePath)
import            Text.DescriptorProtos.DescriptorProto       (DescriptorProto)
import qualified  Text.DescriptorProtos.DescriptorProto       as D
import            Text.DescriptorProtos.FieldDescriptorProto  (FieldDescriptorProto)
import qualified  Text.DescriptorProtos.FieldDescriptorProto  as FE
import            Text.DescriptorProtos.FileDescriptorProto   (FileDescriptorProto)
import qualified  Text.DescriptorProtos.FileDescriptorProto   as F
import            Text.ProtocolBuffers.Basic                  (Utf8, utf8)
import            Text.ProtocolBuffers.ProtoCompile.Parser    (parseProto)

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
          let packagename = toText "Did not find a package name in the .proto file" . F.package $ fileDescriptor
          let modulename = T.toTitle packagename
          let nativeDir = outputDir </> "Native"
          createDirectoryIfMissing True nativeDir
          let nativeFile = nativeDir </> T.unpack modulename <.> "js"
          let elmFile = outputDir </> T.unpack modulename <.> "elm"
          writeFile nativeFile $ genNativeModule protoContents fileDescriptor
          writeFile elmFile $ genElmModule fileDescriptor

toText :: String -> Maybe Utf8 -> Text
toText errorMsg = toStrict . decodeUtf8 . utf8 . fromMaybe (error errorMsg) 

genNativeMarshal :: DescriptorProto -> Text
genNativeMarshal descriptor = undefined

genNativeUnmarshal :: DescriptorProto -> Text
genNativeUnmarshal descriptor = undefined

genNativeModule :: ByteString -> FileDescriptorProto -> ByteString
genNativeModule protoContents fileDescriptor = 
  let
    packagename :: Text
    packagename = toText "Did not find a package name in the .proto file" . F.package $ fileDescriptor
    
    modulename :: Text
    modulename = T.toTitle packagename
    
    descriptors :: [DescriptorProto]
    descriptors = toList $ F.message_type fileDescriptor
    
    typenames :: [Text]
    typenames = toText "Didn't find a name for the descriptor" . D.name <$> descriptors
    
    encoders :: Text
    encoders = T.concat $ nativeEncode <$> typenames
    
    decoders :: Text
    decoders = T.concat $ nativeDecode <$> typenames
    
    encodeValues :: Text
    encodeValues = T.concat $ do
      typename <- typenames
      valueBuilder <- [nativeEncode, nativeDecode]
      return $ valueBuilder typename
    
    marshalValues :: Text
    marshalValues = T.concat $ do
      descriptor <- descriptors
      let typename = toText "Didn't find a name for the descriptor" . D.name $ descriptor
      valueBuilder <- [genNativeMarshal, genNativeUnmarshal]
      return $ valueBuilder descriptor
    
    values :: Text
    --values = T.append encodeValues marshalValues
    values = encodeValues
    
    exports :: Text
    exports = T.concat $ do
      typename <- typenames
      prefix <- ["encode", "decode", "marshal", "unmarshal"]
      return . nativeModuleExport $ T.concat [T.pack prefix, typename]
    
    protoSource :: Text
    protoSource = toStrict . decodeUtf8 $ protoContents
  in encodeUtf8 . fromStrict $ nativeModule packagename modulename protoSource values exports

genElmField :: FieldDescriptorProto -> (Text, Text)
genElmField = undefined
  
genElmType :: DescriptorProto -> Text
genElmType descriptor = 
  let
    fields :: [FieldDescriptorProto]
    fields = toList $ D.field descriptor
    
    elmFields :: [(Text, Text)]
    elmFields = genElmField <$> fields
  in ""
 
genElmModule :: FileDescriptorProto -> ByteString
genElmModule fileDescriptor = 
  let
    packagename :: Text
    packagename = toText "Did not find a package name in the .proto file" . F.package $ fileDescriptor
    
    modulename :: Text
    modulename = T.toTitle packagename
    
    descriptors :: [DescriptorProto]
    descriptors = toList $ F.message_type fileDescriptor
    
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
    