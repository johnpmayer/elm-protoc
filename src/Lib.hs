{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Utils

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

parseProtoFile :: String -> String -> String -> FilePath -> FilePath -> IO ()
parseProtoFile owner project outputPrefix filename outputDir =
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
    putStrLn $ "Writing " ++ nativeFile
    writeFile nativeFile $ genNativeModule (T.pack owner) (T.pack project) (T.pack outputPrefix) (T.pack outputPrefix) (takeFileName filename) protoContents fileDescriptor dependencyModulenames
    putStrLn $ "Writing " ++ elmFile
    writeFile elmFile $ genElmModule (T.pack outputPrefix) fileDescriptor dependencyModulenames
    Prelude.writeFile specFile . groom $ fileDescriptor

toText :: Utf8 -> Text
toText = toStrict . decodeUtf8 . utf8

toTextE :: String -> Maybe Utf8 -> Text
toTextE errorMsg = toText . fromMaybe (error errorMsg)

genNativeFieldMarshal :: Scope -> FieldDescriptorProto -> (Text, Text -> Text)
genNativeFieldMarshal scope fieldDescriptor =
  let
    name :: Text
    name = toTextE "Didn't find a name for the field descriptor" . FE.name $ fieldDescriptor

    label = case FE.label fieldDescriptor of
      Nothing -> error "what's the default - required/optional/repeated"
      Just (l) -> l
  in
    case (FE.type' fieldDescriptor, FE.type_name fieldDescriptor) of
      (Just t, _) ->
        case label of
          FEL.LABEL_REQUIRED -> (name, marshalPrimitive name)
          FEL.LABEL_OPTIONAL -> (name, marshalMaybePrimitive name)
          FEL.LABEL_REPEATED -> (name, marshalListPrimitive name)
      (_, Just tn) ->
        let
          (_,typename) = splitTypePath . toText $ tn
          qualifierList = getElmTypeQualifier scope $ toText tn
          qualifier = T.intercalate (T.pack ".") qualifierList
        in
          case label of
            FEL.LABEL_REQUIRED -> (name, marshalFunc qualifier typename name)
            FEL.LABEL_OPTIONAL -> (name, marshalMaybeFunc qualifier typename name)
            FEL.LABEL_REPEATED -> (name, marshalListFunc qualifier typename name)


genNativeMarshal :: Text -> Scope -> DescriptorProto -> Text
genNativeMarshal packagename scope descriptor =
  let
    typename :: Text
    typename = toTextE "Didn't find a name for the descriptor" . D.name $ descriptor

    fieldDescriptors :: [FieldDescriptorProto]
    fieldDescriptors = toList $ D.field descriptor

    standaloneFields :: [FieldDescriptorProto]
    standaloneFields = filter (\fd -> FE.oneof_index fd == Nothing) fieldDescriptors

    standaloneFieldMarshalers :: [(Text, Text -> Text)]
    standaloneFieldMarshalers = genNativeFieldMarshal scope <$> standaloneFields

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

    oneofTypes :: [(OneofDescriptorProto, [FieldDescriptorProto])]
    oneofTypes =
      let
        groupedOneofFields = map (map snd) $ groupByKey fst oneofFields
      in zip oneofDescriptors groupedOneofFields

    oneofData :: [(Text, [(Text, Text)])]
    oneofData = do
      oneofType <- oneofTypes
      let oneofRecordFieldName = toTextE "Didn't find name for the oneof" . O.name . fst $ oneofType
      let oneofCases =
            do
              fieldDescriptor <- snd oneofType
              let fieldName = toTextE "Didn't find a name for the field descriptor" . FE.name $ fieldDescriptor
                  typename =
                    case (FE.type' fieldDescriptor, FE.type_name fieldDescriptor) of
                      (Just t, _) -> genPrimitiveTypeName t
                      (_, Just tn) -> toText tn
              return (fieldName, typename)
      return $ (oneofRecordFieldName, oneofCases)

    oneofFieldMarshalers :: Text
    oneofFieldMarshalers =
      let
        marshalers :: [Text]
        marshalers = do
          oneofData1 <- oneofData
          let oneofRecordFieldName = fst oneofData1
          let oneofCaseMarshalers = T.concat $
                do
                  (fieldName, fieldTypeName) <- snd oneofData1
                  let (qualifier, justFieldTypename) = splitTypePath fieldTypeName
                  let qualifierPrefix :: Text = T.concat (fmap (\s -> T.toTitle s `T.append` (T.pack ".")) qualifier)
                  return $ nativeOneofCaseMarshal qualifierPrefix typename oneofRecordFieldName fieldName justFieldTypename
          return $ nativeOneofMarshal typename oneofRecordFieldName oneofCaseMarshalers
      in T.concat marshalers

    -- TODO check if the typename has some other qualifier
  in nativeRecordMarshal packagename typename standaloneFieldMarshalers oneofFieldMarshalers

genNativeFieldUnmarshal :: Scope -> FieldDescriptorProto -> (Text, Text -> Text)
genNativeFieldUnmarshal scope fieldDescriptor =
  let
    name :: Text
    name = toTextE "Didn't find a name for the field descriptor" . FE.name $ fieldDescriptor

    label = case FE.label fieldDescriptor of
      Nothing -> error "what's the default - required/optional/repeated"
      Just (l) -> l
  in
    case (FE.type' fieldDescriptor, FE.type_name fieldDescriptor) of
      (Just t, _) ->
        case label of
          FEL.LABEL_REQUIRED -> (name, unmarshalPrimitive name)
          FEL.LABEL_OPTIONAL -> (name, unmarshalMaybePrimitive name)
          FEL.LABEL_REPEATED -> (name, unmarshalListPrimitive name)
      (_, Just tn) ->
        let
          (_,typename) = splitTypePath . toText $ tn
          qualifierList = getElmTypeQualifier scope $ toText tn
          --qualifier = T.intercalate (T.pack ".") qualifierList
          qualifierPrefix :: Text = T.concat (fmap (\s -> T.toTitle s `T.append` (T.pack ".")) qualifierList)
        in
          case label of
            FEL.LABEL_REQUIRED -> (name, unmarshalFunc qualifierPrefix typename name)
            FEL.LABEL_OPTIONAL -> (name, unmarshalMaybeFunc qualifierPrefix typename name)
            FEL.LABEL_REPEATED -> (name, unmarshalListFunc qualifierPrefix typename name)

genNativeUnmarshal :: Scope -> DescriptorProto -> Text
genNativeUnmarshal scope descriptor =
  let
    typename :: Text
    typename = toTextE "Didn't find a name for the descriptor" . D.name $ descriptor

    fieldDescriptors :: [FieldDescriptorProto]
    fieldDescriptors = toList $ D.field descriptor

    standaloneFields :: [FieldDescriptorProto]
    standaloneFields = filter (\fd -> FE.oneof_index fd == Nothing) fieldDescriptors

    standaloneFieldUnmarshalers :: [(Text, Text -> Text)]
    standaloneFieldUnmarshalers = genNativeFieldUnmarshal scope <$> standaloneFields

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

    oneofTypes :: [(OneofDescriptorProto, [FieldDescriptorProto])]
    oneofTypes =
      let
        groupedOneofFields = map (map snd) $ groupByKey fst oneofFields
      in zip oneofDescriptors groupedOneofFields

    oneofData :: [(Text, [(Text, Int32, Text)])]
    oneofData = do
      oneofType <- oneofTypes
      let oneofRecordFieldName = toTextE "Didn't find name for the oneof" . O.name . fst $ oneofType
      let oneofCases =
            do
              fieldDescriptor <- snd oneofType
              let fieldName = toTextE "Didn't find a name for the field descriptor" . FE.name $ fieldDescriptor
                  fieldNumber = fromMaybe (error "Didn't find a number for the field descriptor") . FE.number $ fieldDescriptor
                  typename =
                    case (FE.type' fieldDescriptor, FE.type_name fieldDescriptor) of
                      (Just t, _) -> genPrimitiveTypeName t
                      (_, Just tn) -> toText tn
              return (fieldName, fieldNumber, typename)
      return $ (oneofRecordFieldName, oneofCases)

    oneofFieldUnmarshalers :: [(Text, Text -> Text)]
    oneofFieldUnmarshalers = do
      oneofData1 <- oneofData
      let oneofRecordFieldName = fst oneofData1
      let oneofCaseUnmarshalers = T.concat $
            do
              (fieldName, fieldNumber, fieldTypeName) <- snd oneofData1
              let
                (qualifier, justFieldTypename) = splitTypePath fieldTypeName
                qualifierPrefix :: Text = T.concat (fmap (\s -> T.toTitle s `T.append` (T.pack ".")) qualifier)
              return $ nativeOneofCaseUnmarshal qualifierPrefix typename oneofRecordFieldName fieldName (T.pack . show $ fieldNumber) justFieldTypename
      return $ (oneofRecordFieldName, nativeOneofUnmarshal typename oneofRecordFieldName oneofCaseUnmarshalers)

  in nativeMessageUnmarshal typename (standaloneFieldUnmarshalers ++ oneofFieldUnmarshalers)


getDependencyScope :: Text -> [Text] -> Scope
getDependencyScope prefix dependencyModuleNames =
  let
    makePackageRef x = (x, PackageReference . FQN $ T.concat [ prefix, T.pack ".", x ])
  in [M.fromList . fmap makePackageRef $ fmap toTitlePreserving dependencyModuleNames]

genNativeModule :: Text -> Text -> Text -> Text -> FilePath -> ByteString -> FileDescriptorProto -> [Text] -> ByteString
genNativeModule owner project outputPrefix protoModulename filename protoContents fileDescriptor dependencyModuleNames =
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
      valueBuilder <- [nativeEncode, nativeDecode packagename]
      return $ valueBuilder typename

    marshalValues :: Text
    marshalValues = T.concat $ do
      descriptor <- descriptors
      let typename = toTextE "Didn't find a name for the descriptor" . D.name $ descriptor
      valueBuilder <- [genNativeMarshal packagename, genNativeUnmarshal]
      return $ valueBuilder [] descriptor

    values :: Text
    values = T.append encodeValues marshalValues

    exports :: Text
    exports = T.concat $ do
      typename <- typenames
      prefix <- ["encode", "decode", "marshal", "unmarshal"]
      return . nativeModuleExport $ T.concat [T.pack prefix, typename]

    imports :: Text
    imports = T.concat $ do
      dependency <- dependencyModuleNames
      return $ nativeModuleImport owner project outputPrefix dependency

    protoSource :: Text
    protoSource = toStrict . decodeUtf8 $ protoContents
  in encodeUtf8 . fromStrict $ nativeModule owner project outputPrefix protoModulename (T.pack filename) packagename modulename protoSource values imports exports

genPrimitiveTypeName :: FET.Type -> Text
genPrimitiveTypeName t =
  case t of
    FET.TYPE_DOUBLE   -> "Float"
    FET.TYPE_UINT64   -> "Int"
    FET.TYPE_INT32    -> "Int"
    FET.TYPE_STRING   -> "String"
    FET.TYPE_BOOL     -> "Bool"
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
  let
    (_,justTypename) = splitTypePath typename
    justQualifier = getElmTypeQualifier scope typename
  in
    T.intercalate "." $ justQualifier ++ [justTypename]

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
          let
            basetype = case (FE.type' fieldDescriptor, FE.type_name fieldDescriptor) of
                        (Just t, _) -> genPrimitiveTypeName t
                        (_, Just tn) ->
                          fullyQualifyElmType scope $ case toText <$> FE.extendee fieldDescriptor of
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

genOneofType :: Scope -> Text -> OneofDescriptorProto -> [FieldDescriptorProto] -> (Text, Text)
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
  in (typename, elmSumTypeDef typename fields)

genElmTypeDefs :: Scope -> DescriptorProto -> ([Text], Text)
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

    --(oneofTypeNames, oneofTypeDefs) :: ([Text], Text)
    (oneofTypeNames, oneofTypeDefs) =
      let
        oneofTypeNameAndDefs = map (uncurry $ genOneofType scope typename) oneofTypes
        names = map fst oneofTypeNameAndDefs
        defs = T.concat $ map snd oneofTypeNameAndDefs
      in (names, defs)

    elmFields :: [(Text, Text)]
    elmFields = (genElmField scope <$> standaloneFields) ++ oneofRecordFields

    recordPrefixes :: [Text]
    recordPrefixes = T.pack "{" : repeat (T.pack ",")

    fields :: Text
    fields =
      case elmFields of
        [] -> T.pack "{"
        _ -> T.concat $ zipWith elmRecordField recordPrefixes elmFields
  in (oneofTypeNames, elmRecordTypeDef typename oneofTypeDefs fields)

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
    exports = T.concat . zipWith elmExport exportPrefixes $ concat [typenames, oneofTypenameExports, contractTypeExports, valueExports]

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

    --(oneofTypenames, typeDefs) :: Text
    (oneofTypenames, typeDefs) =
      let
        typeOneofNamesAndDefs = genElmTypeDefs scope <$> descriptors
        allOneofNames = concat $ map fst typeOneofNamesAndDefs
        defs = T.concat $ map snd typeOneofNamesAndDefs
      in (allOneofNames, defs)

    oneofTypenameExports :: [Text]
    oneofTypenameExports = (`T.append` (T.pack "(..)")) <$> oneofTypenames

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
