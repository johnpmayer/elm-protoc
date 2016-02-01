{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseProtoFile
    ) where

import qualified  Prelude                                           as Prelude
import            Prelude hiding                                    (readFile, writeFile)

import            Control.Monad                                     (forM)
import            Data.ByteString.Lazy                              (ByteString, readFile, writeFile)
import qualified  Data.Char                                         as C
import            Data.Foldable                                     (toList)
import            Data.Int                                          (Int32)
import            Data.List                                         (groupBy)
import            Data.Maybe                                        (fromMaybe)
import            Data.Text                                         (Text)
import qualified  Data.Text                                         as T
import            Data.Text.Lazy                                    (fromStrict, toStrict)
import            Data.Text.Lazy.Encoding                           (decodeUtf8, encodeUtf8)
import            System.Directory                                  (createDirectoryIfMissing)
import            System.FilePath                                   ((</>), (<.>), takeDirectory, takeFileName)
import            System.IO                                         (FilePath)
import            Text.DescriptorProtos.DescriptorProto             (DescriptorProto)
import qualified  Text.DescriptorProtos.DescriptorProto             as D
import            Text.DescriptorProtos.FieldDescriptorProto        (FieldDescriptorProto)
import qualified  Text.DescriptorProtos.FieldDescriptorProto        as FE
import qualified  Text.DescriptorProtos.FieldDescriptorProto.Label  as FEL
import qualified  Text.DescriptorProtos.FieldDescriptorProto.Type   as FET
import            Text.DescriptorProtos.FileDescriptorProto         (FileDescriptorProto)
import qualified  Text.DescriptorProtos.FileDescriptorProto         as F
import            Text.DescriptorProtos.OneofDescriptorProto        (OneofDescriptorProto)
import qualified  Text.DescriptorProtos.OneofDescriptorProto        as O
import            Text.Groom                                        (groom)
import            Text.ProtocolBuffers.Basic                        (Utf8, utf8)
import            Text.ProtocolBuffers.ProtoCompile.Parser          (parseProto)

import Templates

toTitlePreserving :: Text -> Text
toTitlePreserving =
  let
    walk (capitalize, rem) char =
      let
        capitalizeNext = not $ C.isAlpha char
        newRem = T.snoc rem (if capitalize then C.toUpper char else char)
      in (capitalizeNext, newRem)
  in snd . T.foldl walk (True, T.empty)

groupByKey :: (Eq k) => (a -> k) -> [a] -> [[a]]
groupByKey keyFun =
  let
    compareFun rowA rowB = keyFun rowA == keyFun rowB
  in groupBy compareFun

loadProtoFile :: FilePath -> IO (ByteString,FileDescriptorProto)
loadProtoFile filename =
  do
    protoContents <- readFile filename
    return $ case parseProto filename protoContents of
      Left parseError -> error $ "Failed to parse " ++ filename ++ show parseError
      Right fileDescriptor -> (protoContents,fileDescriptor)

parseProtoFile :: String -> FilePath -> FilePath -> IO ()
parseProtoFile protoModulename filename outputDir =
  do
    (protoContents, fileDescriptor) <- loadProtoFile filename
    let dependencyFilenames = T.unpack . toText <$> (toList $ F.dependency fileDescriptor)
    let absoluteDependencyFilenames = ((takeDirectory filename)</>) <$> dependencyFilenames
    dependencyModulenames <- forM absoluteDependencyFilenames $ \dependencyFilename ->
      do
        (_, dependencyFileDescriptor) <- loadProtoFile dependencyFilename
        let dependencyPackagename = toTextE "Did not find a package name in the .proto file" . F.package $ dependencyFileDescriptor
        let dependencyModulename = toTitlePreserving dependencyPackagename
        return dependencyModulename
    createDirectoryIfMissing True outputDir
    let packagename = toTextE "Did not find a package name in the .proto file" . F.package $ fileDescriptor
    let modulename = toTitlePreserving packagename
    let nativeDir = outputDir </> "Native"
    let specDir = outputDir </> "spec"
    createDirectoryIfMissing True nativeDir
    createDirectoryIfMissing True specDir
    let nativeFile = nativeDir </> T.unpack modulename <.> "js"
    let elmFile = outputDir </> T.unpack modulename <.> "elm"
    let specFile = specDir </> T.unpack packagename <.> "spec"
    writeFile nativeFile $ genNativeModule (T.pack protoModulename) (takeFileName filename) protoContents fileDescriptor dependencyModulenames
    writeFile elmFile $ genElmModule fileDescriptor dependencyModulenames
    Prelude.writeFile specFile . groom $ fileDescriptor

toText :: Utf8 -> Text
toText = toStrict . decodeUtf8 . utf8

toTextE :: String -> Maybe Utf8 -> Text
toTextE errorMsg = toText . fromMaybe (error errorMsg)

genNativeMarshal :: DescriptorProto -> Text
genNativeMarshal descriptor = undefined

genNativeUnmarshal :: DescriptorProto -> Text
genNativeUnmarshal descriptor = undefined

genNativeModule :: Text -> FilePath -> ByteString -> FileDescriptorProto -> [Text] -> ByteString
genNativeModule protoModulename filename protoContents fileDescriptor dependencyModuleNames =
  let
    packagename :: Text
    packagename = toTextE "Did not find a package name in the .proto file" . F.package $ fileDescriptor

    modulename :: Text
    modulename = toTitlePreserving packagename

    descriptors :: [DescriptorProto]
    descriptors = toList $ F.message_type fileDescriptor

    typenames :: [Text]
    typenames = toTextE "Didn't find a name for the descriptor" . D.name <$> descriptors

    encodeValues :: Text
    encodeValues = T.concat $ do
      typename <- typenames
      valueBuilder <- [nativeEncode, nativeDecode]
      return $ valueBuilder protoModulename typename

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
  in encodeUtf8 . fromStrict $ nativeModule protoModulename (T.pack filename) packagename modulename protoSource values exports

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
              Nothing -> toTitlePreserving . toText $ tn
              Just ee -> error "where?" -- T.concat [toTitlePreserving ee, "_", toText tn]
          in
            case FE.label fieldDescriptor of
              Nothing -> error "what's the default - required/optional/repeated"
              Just (FEL.LABEL_OPTIONAL) ->
                case FE.oneof_index fieldDescriptor of
                  Nothing -> T.concat ["Maybe (", basetype, ")"]
                  Just _ -> basetype
              Just (FEL.LABEL_REQUIRED) -> basetype
              Just (FEL.LABEL_REPEATED) -> T.concat ["List (", basetype, ")"]

    in (name, typename)

genOneofType :: Text -> OneofDescriptorProto -> [FieldDescriptorProto] -> Text
genOneofType baseTypename oneofDescriptor fieldDescriptors =
  let
    oneofSubTypeName = toTextE "Didn't find a name for the oneof" . O.name $ oneofDescriptor
    typename = T.concat [baseTypename, "_oneof_", oneofSubTypeName]
    oneofFields = genElmField <$> fieldDescriptors
    oneofPrefixes = T.pack "=" : repeat (T.pack "|")
    fields =
      case oneofFields of
        [] -> error "Sum type with no fields?"
        _ -> T.concat $ zipWith (elmSumField typename) oneofPrefixes oneofFields
  in elmSumTypeDef typename fields

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

    oneofDescriptors :: [OneofDescriptorProto]
    oneofDescriptors = toList $ D.oneof_decl descriptor

    oneofFieldNames :: [Text]
    oneofFieldNames = toTextE "Didn't find a name for the oneof" . O.name <$> oneofDescriptors

    oneofRecordFields :: [(Text, Text)]
    oneofRecordFields = do
      name <- oneofFieldNames
      let fieldTypename = T.concat [typename, "_oneof_", name]
      return (name, fieldTypename)

    oneofTypes :: [(OneofDescriptorProto, [FieldDescriptorProto])]
    oneofTypes =
      let
        groupedOneofFields = map (map snd) $ groupByKey fst oneofFields
      in zip oneofDescriptors groupedOneofFields

    oneofTypeDefs :: Text
    oneofTypeDefs = T.concat $ map (uncurry $ genOneofType typename) oneofTypes

    elmFields :: [(Text, Text)]
    elmFields = (genElmField <$> standaloneFields) ++ oneofRecordFields

    recordPrefixes :: [Text]
    recordPrefixes = T.pack "{" : repeat (T.pack ",")

    fields :: Text
    fields =
      case elmFields of
        [] -> T.pack "{"
        _ -> T.concat $ zipWith elmRecordField recordPrefixes elmFields
  in elmRecordTypeDef typename oneofTypeDefs fields

genElmModule :: FileDescriptorProto -> [Text] -> ByteString
genElmModule fileDescriptor dependencyModulenames =
  let
    packagename :: Text
    packagename = toTextE "Did not find a package name in the .proto file" . F.package $ fileDescriptor

    modulename :: Text
    modulename = toTitlePreserving packagename

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

    imports :: Text
    imports = T.concat $ elmDependencyImport <$> dependencyModulenames

  in encodeUtf8 . fromStrict $ elmModule modulename exports imports types contractTypes values
