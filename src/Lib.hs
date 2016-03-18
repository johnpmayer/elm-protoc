{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import qualified  Data.List                                         as L
import            Data.List                                         (groupBy)
import            Data.Maybe                                        (fromMaybe)
import qualified  Data.Map                                          as M
import            Data.Map                                          (Map)
import qualified  Data.Set                                          as S
import            Data.Set                                          (Set)
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
parseProtoFile outputPrefix filename outputDir =
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
    let prefixDir = outputDir </> outputPrefix
    let nativeDir = outputDir </> "Native" </> outputPrefix
    let specDir = outputDir </> "spec"
    createDirectoryIfMissing True prefixDir
    createDirectoryIfMissing True nativeDir
    createDirectoryIfMissing True specDir
    let nativeFile = nativeDir </> T.unpack modulename <.> "js"
    let elmFile = prefixDir </> T.unpack modulename <.> "elm"
    let specFile = specDir </> T.unpack packagename <.> "spec"
    --let internalModule
    writeFile nativeFile $ genNativeModule (T.pack outputPrefix) (T.pack outputPrefix) (takeFileName filename) protoContents fileDescriptor dependencyModulenames
    writeFile elmFile $ genElmModule (T.pack outputPrefix) fileDescriptor dependencyModulenames
    Prelude.writeFile specFile . groom $ fileDescriptor

toText :: Utf8 -> Text
toText = toStrict . decodeUtf8 . utf8

toTextE :: String -> Maybe Utf8 -> Text
toTextE errorMsg = toText . fromMaybe (error errorMsg)

genNativeMarshal :: Scope -> DescriptorProto -> Text
genNativeMarshal protoModulename typename = error "genNativeMarshal"

genNativeFieldUnmarshal :: Scope -> FieldDescriptorProto -> (Text -> Text)
genNativeFieldUnmarshal scope fieldDescriptor =
  let
    name :: Text
    name = toTextE "Didn't find a name for the field descriptor" . FE.name $ fieldDescriptor

    --(label, qualifier, typename) :: (label, [Text], Text)
    (label, qualifierList, typename) = 
      case (FE.type' fieldDescriptor, FE.type_name fieldDescriptor) of
        (Just t, _) -> error "genNativeFieldUnmarshal primitive"
        (_, Just tn) -> 
          let
            (_,justTypename) = splitTypePath . toText $ tn
            justQualifier = getElmTypeQualifier scope $ toText tn
          in 
            case FE.label fieldDescriptor of
              Nothing -> error "what's the default - required/optional/repeated"
              Just (label) -> (label, justQualifier, justTypename)
    
    qualifier :: Text
    qualifier = T.intercalate (T.pack ".") qualifierList

  in
    case label of 
      FEL.LABEL_REQUIRED -> unmarshalFunc qualifier typename name 
      FEL.LABEL_OPTIONAL -> unmarshalMaybeFunc qualifier typename name 
      FEL.LABEL_REPEATED -> unmarshalListFunc qualifier typename name 

genNativeUnmarshal :: Scope -> DescriptorProto -> Text
genNativeUnmarshal scope descriptor = 
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
      let fieldTypename = fullyQualifyElmType scope $ T.concat [typename, "_oneof_", name]
      return (name, fieldTypename)

    oneofTypes :: [(OneofDescriptorProto, [FieldDescriptorProto])]
    oneofTypes =
      let
        groupedOneofFields = map (map snd) $ groupByKey fst oneofFields
      in zip oneofDescriptors groupedOneofFields

    fields :: [(Text, Text)]
    fields = (genElmField scope <$> standaloneFields) ++ oneofRecordFields

    scope :: Scope
    scope = []

  in error "genNativeUnmarshal"


getDependencyScope :: Text -> [Text] -> Scope
getDependencyScope prefix dependencyModuleNames = 
  let
    makePackageRef x = (x, PackageReference . FQN $ T.concat [ prefix, T.pack ".", x ])
  in [M.fromList . fmap makePackageRef $ fmap toTitlePreserving dependencyModuleNames]

genNativeModule :: Text -> Text -> FilePath -> ByteString -> FileDescriptorProto -> [Text] -> ByteString
genNativeModule outputPrefix protoModulename filename protoContents fileDescriptor dependencyModuleNames =
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
      typename <- toTextE "Didn't find a name for the descriptor" . D.name <$> descriptors
      let typename = toTextE "Didn't find a name for the descriptor" . D.name $ descriptor
      valueBuilder <- [genNativeMarshal, genNativeUnmarshal]
      return $ valueBuilder [M.empty] descriptor

    values :: Text
    values = T.append encodeValues marshalValues

    exports :: Text
    exports = T.concat $ do
      typename <- typenames
      prefix <- ["encode", "decode", "marshal", "unmarshal"]
      return . nativeModuleExport $ T.concat [T.pack prefix, typename]

    protoSource :: Text
    protoSource = toStrict . decodeUtf8 $ protoContents
  in encodeUtf8 . fromStrict $ nativeModule outputPrefix protoModulename (T.pack filename) packagename modulename protoSource values exports

genPrimitiveTypeName :: FET.Type -> Text
genPrimitiveTypeName t =
  case t of
    FET.TYPE_DOUBLE   -> "Float"
    FET.TYPE_UINT64   -> "Int"
    FET.TYPE_INT32    -> "Int"
    FET.TYPE_STRING   -> "String"
    _                 -> error $ "type not implemented - pull request - " ++ show t

newtype FullyQualifiedName = FQN { getFQN :: Text } deriving (Show)

data Namespace
  = PackageReference FullyQualifiedName
  | NestedScope Scope
  deriving (Show)

type Scope = [Map Text Namespace]
    
-- Given a scope and a type name , generate the fully qualified Elm type name
fullyQualifyElmType :: Scope -> Text -> Text
fullyQualifyElmType scope typename = 
  error "join"

splitTypePath :: Text -> ([Text],Text)
splitTypePath typename =
  case T.split (=='.') typename of
    [] -> error "empty type name"
    fullPath -> 
      let
        typename = last fullPath
        startPath = reverse . tail . reverse $ fullPath
      in (startPath, typename)

getElmTypeQualifier :: Scope -> Text -> [Text]
getElmTypeQualifier scope typename =
  case scope of
    [] -> [] --typename
    (local : ancestors) -> 
      case splitTypePath typename of
        ([], _) -> [] --typename
        ((qualified : typepath), _)-> 
          case M.lookup (toTitlePreserving qualified) local of
            Nothing -> getElmTypeQualifier ancestors typename
            Just (PackageReference name) -> getFQN name : typepath
            Just (NestedScope scope) -> error "todo nested scope"

-- TODO there's a thing here around nested types and membership in a oneof
genElmField :: Scope -> FieldDescriptorProto -> (Text, Text)
genElmField scope fieldDescriptor =
  let
    name :: Text
    name = toTextE "Didn't find a name for the field descriptor" . FE.name $ fieldDescriptor

    typename :: Text
    typename = 
      case (FE.type' fieldDescriptor, FE.type_name fieldDescriptor) of
        (Just t, _) -> genPrimitiveTypeName t
        (_, Just tn) ->
          let
            basetype = fullyQualifyElmType scope $ case toText <$> FE.extendee fieldDescriptor of
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

genOneofType :: Scope -> Text -> OneofDescriptorProto -> [FieldDescriptorProto] -> Text
genOneofType scope baseTypename oneofDescriptor fieldDescriptors =
  let
    oneofSubTypeName = toTextE "Didn't find a name for the oneof" . O.name $ oneofDescriptor
    typename = T.concat [baseTypename, "_oneof_", oneofSubTypeName]
    oneofFields = genElmField scope <$> fieldDescriptors
    oneofPrefixes = T.pack "=" : repeat (T.pack "|")
    fields =
      case oneofFields of
        [] -> error "Sum type with no fields?"
        _ -> T.concat $ zipWith (elmSumField typename) oneofPrefixes oneofFields
  in elmSumTypeDef typename fields

genElmTypeDefs :: Scope -> DescriptorProto -> Text
genElmTypeDefs scope descriptor =
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
      let fieldTypename = fullyQualifyElmType scope $ T.concat [typename, "_oneof_", name]
      return (name, fieldTypename)

    oneofTypes :: [(OneofDescriptorProto, [FieldDescriptorProto])]
    oneofTypes =
      let
        groupedOneofFields = map (map snd) $ groupByKey fst oneofFields
      in zip oneofDescriptors groupedOneofFields

    oneofTypeDefs :: Text
    oneofTypeDefs = T.concat $ map (uncurry $ genOneofType scope typename) oneofTypes

    elmFields :: [(Text, Text)]
    elmFields = (genElmField scope <$> standaloneFields) ++ oneofRecordFields

    recordPrefixes :: [Text]
    recordPrefixes = T.pack "{" : repeat (T.pack ",")

    fields :: Text
    fields =
      case elmFields of
        [] -> T.pack "{"
        _ -> T.concat $ zipWith elmRecordField recordPrefixes elmFields
  in elmRecordTypeDef typename oneofTypeDefs fields

genElmModule :: Text -> FileDescriptorProto -> [Text] -> ByteString
genElmModule outputPrefix fileDescriptor dependencyModulenames =
  let
    packagename :: Text
    packagename = toTextE "Did not find a package name in the .proto file" . F.package $ fileDescriptor

    dependencyScope :: Scope
    dependencyScope = getDependencyScope outputPrefix dependencyModulenames

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

    -- TODO all of the messages represent new scopes as well
    scope :: Scope
    scope = dependencyScope

    typeDefs :: Text
    typeDefs = T.concat $ genElmTypeDefs scope <$> descriptors

    contractTypes :: Text
    contractTypes = T.concat $ elmContractTypeDef <$> typenames

    values :: Text
    values = T.concat $ do
      typename <- typenames
      valueBuilder <- [elmEncode, elmDecode, elmMarshal, elmUnmarshal]
      return $ valueBuilder outputPrefix modulename typename

    imports :: Text
    imports = T.concat $ elmDependencyImport . (T.append $ T.concat [outputPrefix, T.pack "."]) <$> dependencyModulenames

    comment :: Text
    comment = T.pack . show $ scope

  in encodeUtf8 . fromStrict $ elmModule outputPrefix modulename exports imports typeDefs contractTypes values comment
