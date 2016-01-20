{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseProtoFile
    ) where

import Prelude hiding                             (readFile, writeFile)
    
import            Data.ByteString.Lazy                              (ByteString, readFile, writeFile)
import            Data.Foldable                                     (toList)
import            Data.Int                                          (Int32)
import            Data.Maybe                                        (fromMaybe)
import            Data.Text                                         (Text)
import qualified  Data.Text                                         as T
import            Data.Text.Lazy                                    (fromStrict, toStrict)
import            Data.Text.Lazy.Encoding                           (decodeUtf8, encodeUtf8)
import            System.Directory                                  (createDirectoryIfMissing)
import            System.FilePath                                   ((</>), (<.>))
import            System.IO                                         (FilePath)
import            Text.DescriptorProtos.DescriptorProto             (DescriptorProto)
import qualified  Text.DescriptorProtos.DescriptorProto             as D
import            Text.DescriptorProtos.FieldDescriptorProto        (FieldDescriptorProto)
import qualified  Text.DescriptorProtos.FieldDescriptorProto        as FE
import qualified  Text.DescriptorProtos.FieldDescriptorProto.Label  as FEL
import qualified  Text.DescriptorProtos.FieldDescriptorProto.Type   as FET
import            Text.DescriptorProtos.FileDescriptorProto         (FileDescriptorProto)
import qualified  Text.DescriptorProtos.FileDescriptorProto         as F
import            Text.Groom                                        (groom)
import            Text.ProtocolBuffers.Basic                        (Utf8, utf8)
import            Text.ProtocolBuffers.ProtoCompile.Parser          (parseProto)

import Templates

parseProtoFile :: FilePath -> FilePath -> IO ()
parseProtoFile filename outputDir = 
  do
    protoContents <- readFile filename
    case parseProto filename protoContents of
      Left parseError -> error $ "Failed to parse " ++ filename ++ show parseError
      Right fileDescriptor -> 
        do
          putStrLn . groom $ fileDescriptor
          createDirectoryIfMissing True outputDir
          let packagename = toTextE "Did not find a package name in the .proto file" . F.package $ fileDescriptor
          let modulename = T.toTitle packagename
          let nativeDir = outputDir </> "Native"
          createDirectoryIfMissing True nativeDir
          let nativeFile = nativeDir </> T.unpack modulename <.> "js"
          let elmFile = outputDir </> T.unpack modulename <.> "elm"          
          writeFile nativeFile $ genNativeModule protoContents fileDescriptor
          writeFile elmFile $ genElmModule fileDescriptor

toText :: Utf8 -> Text
toText = toStrict . decodeUtf8 . utf8

toTextE :: String -> Maybe Utf8 -> Text
toTextE errorMsg = toText . fromMaybe (error errorMsg) 

genNativeMarshal :: DescriptorProto -> Text
genNativeMarshal descriptor = undefined

genNativeUnmarshal :: DescriptorProto -> Text
genNativeUnmarshal descriptor = undefined

genNativeModule :: ByteString -> FileDescriptorProto -> ByteString
genNativeModule protoContents fileDescriptor = 
  let
    packagename :: Text
    packagename = toTextE "Did not find a package name in the .proto file" . F.package $ fileDescriptor
    
    modulename :: Text
    modulename = T.toTitle packagename
    
    descriptors :: [DescriptorProto]
    descriptors = toList $ F.message_type fileDescriptor
    
    typenames :: [Text]
    typenames = toTextE "Didn't find a name for the descriptor" . D.name <$> descriptors
    
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
      let typename = toTextE "Didn't find a name for the descriptor" . D.name $ descriptor
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

genPrimitiveTypeName :: FET.Type -> Text
genPrimitiveTypeName t =
  case t of
    FET.TYPE_DOUBLE   -> "Float"
    FET.TYPE_UINT64   -> "Int"
    FET.TYPE_INT32    -> "Int"
    FET.TYPE_STRING   -> "String"
    _                 -> error $ "type not implemented - pull request - " ++ show t


-- TODO there's a thing here around nested types and membership in a oneof
genElmField :: FieldDescriptorProto -> (Text, Text)
genElmField fieldDescriptor = 
  let
    name :: Text
    name = toTextE "Didn't find a name for the field descriptor" . FE.name $ fieldDescriptor
    
    typename :: Text
    typename = 
      case (FE.type' fieldDescriptor, FE.type_name fieldDescriptor) of
        (Just t, _) -> genPrimitiveTypeName t
        (_, Just tn) -> 
          let 
            basetype = case toText <$> FE.extendee fieldDescriptor of
              Nothing -> T.toTitle . toText $ tn
              Just ee -> error "where?" -- T.concat [T.toTitle ee, "_", toText tn]
          in
            case FE.label fieldDescriptor of
              Nothing -> error "what's the default - required/optional/repeated"
              Just (FEL.LABEL_OPTIONAL) -> T.concat ["Maybe (", basetype, ")"]
              Just (FEL.LABEL_REQUIRED) -> basetype
              Just (FEL.LABEL_REPEATED) -> T.concat ["List (", basetype, ")"]
    
    in (name, typename)
  
genElmType :: DescriptorProto -> Text
genElmType descriptor = 
  let
    typename :: Text
    typename = toTextE "Didn't find a name for the descriptor" . D.name $ descriptor
    
    fieldDescriptors :: [FieldDescriptorProto]
    fieldDescriptors = toList $ D.field descriptor
    
    standaloneFields :: [FieldDescriptorProto]
    standaloneFields = filter (\fd -> FE.oneof_index fd == Nothing) fieldDescriptors
    
    oneofFields :: [(Int32, FieldDescriptorProto)]
    oneofFields = 
      let 
        accum fd xs =
          case FE.oneof_index fd of
            Nothing -> xs
            Just index -> (index, fd) : xs
      in foldr accum [] fieldDescriptors
      
    -- oneofs is a group-by the index & zip with the oneof decls here!
    
    elmFields :: [(Text, Text)]
    elmFields = genElmField <$> standaloneFields -- TODO plus oneofs!
    
    recordPrefixes :: [Text]
    recordPrefixes = T.pack "{" : repeat (T.pack ",")
    
    fields :: Text
    fields = T.concat $ zipWith elmRecordField recordPrefixes elmFields
  in elmRecordTypeDef typename fields
 
genElmModule :: FileDescriptorProto -> ByteString
genElmModule fileDescriptor = 
  let
    packagename :: Text
    packagename = toTextE "Did not find a package name in the .proto file" . F.package $ fileDescriptor
    
    modulename :: Text
    modulename = T.toTitle packagename
    
    descriptors :: [DescriptorProto]
    descriptors = toList $ F.message_type fileDescriptor
    
    typenames :: [Text]
    typenames = toTextE "Didn't find a name for the descriptor" . D.name <$> descriptors
    
    exportPrefixes :: [Text]
    exportPrefixes = T.pack "(" : repeat (T.pack ",")
    
    exports :: Text
    exports = T.concat . zipWith elmExport exportPrefixes $ concat [typenames, contractTypeExports, valueExports]
        
    contractTypeExports :: [Text]
    contractTypeExports = (\x -> T.append x (T.pack "Contract")) <$> typenames
    
    valueExports :: [Text]
    valueExports = do
      typename <- typenames
      prefix <- ["encode", "decode", "marshal", "unmarshal"]
      return  $ T.concat [T.pack prefix, typename]
    
    types :: Text
    types = T.concat $ genElmType <$> descriptors
    
    contractTypes :: Text
    contractTypes = T.concat $ elmContractTypeDef <$> typenames
        
    values :: Text
    values = T.concat $ do
      typename <- typenames
      valueBuilder <- [elmEncode, elmDecode, elmMarshal, elmUnmarshal]
      return $ valueBuilder modulename typename

  in encodeUtf8 . fromStrict $ elmModule modulename exports types contractTypes values
    