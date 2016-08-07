{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Templates where

import qualified Data.Text as T

import Data.Int                 (Int32)
import Data.Text                (Text)
import NeatInterpolation        (text)

import Utils

nativeModule owner project prefix protoModulename filename packagename modulename protoSource moduleValues moduleImports moduleExports =
  let dollar = T.pack "$"
  in [text|
    var _${owner}${dollar}${project}${dollar}Native_${prefix}_${modulename} = function() {

      // reference to the protobuf generated JavaScript, wrapped as a "module"
      var Proto = _${owner}${dollar}${project}${dollar}Native_${protoModulename}_Internal_Proto;

      ${moduleImports}

      ${moduleValues}

      return {
        ${moduleExports}
      }
    }();
  |]

nativeModuleImport owner project prefix dependency =
  let dollar = T.pack "$"
  in [text|
    var ${dependency} = _${owner}${dollar}${project}${dollar}Native_${prefix}_${dependency}
  |]

nativeModuleExport valuename =
  [text|
    ${valuename}: ${valuename},
  |]

nativeDecode packagename typename =
  [text|
    var decode${typename} = function(blob) {
      return Proto.${packagename}.${typename}.deserializeBinary(blob);
    }
  |]

nativeEncode typename =
  [text|
    var encode${typename} = function(message_${typename}) {
      return message_${typename}.serializeBinary()
    }
  |]

marshalPrimitive :: Text -> (Text -> Text)
marshalPrimitive fieldName = \typename ->
  let
    setMethod = T.concat [T.pack "set", T.toTitle fieldName]
  in
    [text|
      contract_${typename}.${setMethod}(value_${typename}.${fieldName});
    |]

marshalMaybePrimitive :: Text -> (Text -> Text)
marshalMaybePrimitive fieldName = \typename ->
  let
    setMethod = T.concat [T.pack "set", toTitlePreserving fieldName]
  in
    [text|
      throw "Not implemented - optional field marshaling (${fieldName})";
      contract_${typename}.${setMethod}(value_${typename}.${fieldName});
    |]

marshalListPrimitive :: Text -> (Text -> Text)
marshalListPrimitive fieldName = \typename ->
  let
    setMethod = T.concat [T.pack "set", toTitlePreserving fieldName]
  in
    [text|
      throw "Not implemented - repeated field marshaling (${fieldName})";
      contract_${typename}.${setMethod}(value_${typename}.${fieldName});
    |]

marshalFunc :: Text -> Text -> Text -> (Text -> Text)
marshalFunc qualifier typename fieldName = \typename ->
  let
    setMethod = T.concat [T.pack "set", toTitlePreserving fieldName]
  in
    [text|
      var tmp_${fieldName} = ${qualifier}marshal${typename}(value_${typename}.${fieldName});
      contract_${typename}.${setMethod}(tmp_${fieldName});
    |]

marshalMaybeFunc :: Text -> Text -> Text -> (Text -> Text)
marshalMaybeFunc qualifier typename fieldName = \typename ->
  let
    setMethod = T.concat [T.pack "set", toTitlePreserving fieldName]
  in
    [text|
      throw "Not implemented - optional field marshaling (${fieldName})";
      var tmp_${fieldName} = ${qualifier}marshal${typename}(value_${typename}.${fieldName});
      contract_${typename}.${setMethod}(tmp_${fieldName});
    |]

marshalListFunc :: Text -> Text -> Text -> (Text -> Text)
marshalListFunc qualifier typename fieldName = \typename ->
  let
    setMethod = T.concat [T.pack "set", toTitlePreserving fieldName]
  in
    [text|
      throw "Not implemented - repeated field marshaling (${fieldName})";
      var tmp_${fieldName} = ${qualifier}marshal${typename}(value_${typename}.${fieldName});
      contract_${typename}.${setMethod}(tmp_${fieldName});
    |]

nativeOneofCaseMarshal :: Text -> Text -> Text -> Text -> Text -> Text
nativeOneofCaseMarshal qualifier typename oneofRecordFieldName fieldName fieldTypeName =
  let
    elmOneofCaseName = [text|${typename}_oneof_${oneofRecordFieldName}_${fieldName}|]
    setMethod = T.concat [T.pack "set", T.toTitle fieldName]
  in
    [text|
      case "${elmOneofCaseName}":
        var tmp_${fieldName} = ${qualifier}marshal${fieldTypeName}(value_${typename}.${oneofRecordFieldName}._0);
        contract_${typename}.${setMethod}(tmp_${fieldName});
        break;
    |]

nativeOneofMarshal :: Text -> Text -> Text -> Text
nativeOneofMarshal typename oneofRecordFieldName oneofCaseMarshalers =
  let
    a = 1
  in
    [text|
      switch (value_${typename}.${oneofRecordFieldName}.ctor) {
      ${oneofCaseMarshalers}
      default:
        throw new Error("Default case - marshal oneof (${typename}.${oneofRecordFieldName})");
      }
    |]

nativeRecordMarshal :: Text -> Text -> [(Text, Text -> Text)] -> Text -> Text
nativeRecordMarshal packagename typename fields oneofFieldMarshalers =
  let
    makeFieldMarshalStatement field = snd field $ typename
    fieldMarshalStatements = T.unlines $ fmap makeFieldMarshalStatement fields
    constructorArguments = T.concat $ fmap (((T.pack ", ") `T.append`) . fst) fields
    applyN = T.concat [ T.pack "A", T.pack . show . length $ fields ]
  in
    [text|
      var marshal${typename} = function(value_${typename}) {
        var contract_${typename} = new Proto.${packagename}.${typename}();
        ${fieldMarshalStatements}
        ${oneofFieldMarshalers}
        return contract_${typename};
      }
    |]

unmarshalPrimitive :: Text -> (Text -> Text)
unmarshalPrimitive fieldName = \contractValueName ->
  let
    getter = T.concat [T.pack "get", T.toTitle fieldName]
  in
    [text|
      var ${fieldName} = ${contractValueName}.${getter}();
    |]

unmarshalMaybePrimitive :: Text -> (Text -> Text)
unmarshalMaybePrimitive fieldName = \contractValueName ->
  let
    getter = T.concat [T.pack "get", T.toTitle fieldName]
    dollar = T.pack "$"
  in
    [text|
      var ${fieldName};
      var tmp_${fieldName} = ${contractValueName}.${getter}();
      if (tmp_${fieldName}) {
        ${fieldName} = _elm_lang${dollar}core${dollar}Maybe${dollar}Just(tmp_${fieldName});
      } else {
        ${fieldName} = _elm_lang${dollar}core${dollar}Maybe${dollar}Nothing;
      }
    |]

unmarshalListPrimitive :: Text -> (Text -> Text)
unmarshalListPrimitive fieldName = \contractValueName ->
  let
    listGetter = T.concat [T.pack "get", T.toTitle fieldName, T.pack "List"]
    fromArray = "_elm_lang$core$Native_List.fromArray"
  in
    [text|
      var ${fieldName}_list = ${contractValueName}.${listGetter}();
      var ${fieldName} = ${fromArray}(${fieldName}_list);
    |]

unmarshalFunc :: Text -> Text -> Text -> (Text -> Text)
unmarshalFunc qualifier typename fieldName = \contractValueName ->
  let
    getter = T.concat [T.pack "get", T.toTitle fieldName]
  in
    [text|
      var ${fieldName} = ${qualifier}unmarshal${typename}(${contractValueName}.${getter}());
    |]

unmarshalMaybeFunc :: Text -> Text -> Text -> (Text -> Text)
unmarshalMaybeFunc qualifier typename fieldName = \contractValueName ->
  let
    getter = T.concat [T.pack "get", T.toTitle fieldName]
    nothing = "_elm_lang$core$Maybe$Nothing"
    just = "_elm_lang$core$Maybe$Just"
  in
    [text|
      var tmp_${fieldName} = ${qualifier}unmarshal${typename}(${contractValueName}.${getter}());
      var ${fieldName};
      if (tmp_${fieldName}) {
        ${fieldName} = ${just}(tmp_${fieldName});
      } else {
        ${fieldName} = $nothing;
      }
    |]

unmarshalListFunc :: Text -> Text -> Text -> (Text -> Text)
unmarshalListFunc qualifier typename fieldName = \contractValueName ->
  let
    listGetter = T.concat [T.pack "get", T.toTitle fieldName, T.pack "List"]
    fromArray = "_elm_lang$core$Native_List.fromArray"
  in
    [text|
      var ${fieldName}_contracts_list = ${contractValueName}.${listGetter}();
      var ${fieldName}_list = ${fieldName}_contracts_list.map(x => ${qualifier}unmarshal${typename}(x));
      var ${fieldName} = ${fromArray}(${fieldName}_list);
    |]

nativeOneofCaseUnmarshal :: Text -> Text -> Text -> Text -> Text -> Text -> Text
nativeOneofCaseUnmarshal qualifier typename oneofRecordFieldName fieldName fieldNumber fieldTypeName =
  let
    elmOneofCaseName = [text|${typename}_oneof_${oneofRecordFieldName}_${fieldName}|]
    getter = T.concat [T.pack "get", T.toTitle fieldName]
  in
    [text|
      case ${fieldNumber}:
        var tmp_${fieldTypeName} = ${qualifier}unmarshal${fieldTypeName}(${typename}_contract.${getter}());
        ${oneofRecordFieldName} = {ctor: '${elmOneofCaseName}', _0: tmp_${fieldTypeName}};
        break;
    |]

nativeOneofUnmarshal :: Text -> Text -> Text -> (Text -> Text)
nativeOneofUnmarshal typename oneofRecordFieldName oneofCaseUnmarshalers = \contractValueName ->
  let
    getter = T.concat [T.pack "get", toTitlePreserving oneofRecordFieldName, T.pack "Case"]
  in
    [text|
      var ${oneofRecordFieldName};
      switch (${contractValueName}.${getter}()) {
        ${oneofCaseUnmarshalers}
        default:
          throw new Error("Default case - unmarshal oneof (${typename}.${oneofRecordFieldName})");
      }
    |]

nativeMessageUnmarshal :: Text -> [(Text, Text -> Text)] -> Text
nativeMessageUnmarshal typename fields =
  let
    contractValueName = T.concat [ typename, T.pack "_contract" ]
    makeFieldUnmarshalStatement field = snd field $ contractValueName
    fieldUnmarshalStatements = T.unlines $ fmap makeFieldUnmarshalStatement fields
    constructorArguments = T.intercalate ", " . fmap fst $ fields
    applyN = T.concat [ T.pack "A", T.pack . show . length $ fields ]
    makeFieldProperty field = T.concat [ field, T.pack ": ", field ]
    fieldProperties = T.intercalate ", " . fmap (makeFieldProperty . fst) $ fields
  in
    [text|
      var unmarshal${typename} = function(${contractValueName}) {
        ${fieldUnmarshalStatements}
        return {
          ${fieldProperties}
        }
      }
    |]

elmModule prefix modulename moduleExports dependencyImports types contractTypeDefs modulevalues comment =
  [text|

    {-
     - ${comment}
     -}

    module ${prefix}.${modulename} exposing
      ${moduleExports} )

    import Binary.ArrayBuffer exposing (ArrayBuffer)

    ${dependencyImports}

    import Native.${prefix}.Internal.Proto as Proto -- for compiler include only
    import Native.${prefix}.${modulename}

    ${types}

    -- Opaque Type definitions
    ${contractTypeDefs}

    ${modulevalues}
  |]

elmDependencyImport modulename =
  [text|
    import ${modulename}
  |]

elmExport prefix name =
  [text|
    ${prefix} ${name}
  |]

elmRecordField prefix (name, typename) =
  [text|
    ${prefix} ${name} : ${typename}
  |]

elmRecordTypeDef typename oneofTypeDefs recordFields =
  [text|
    ${oneofTypeDefs}
    type alias ${typename} =
      ${recordFields} }
  |]

elmSumField sumTypename prefix (name, typename) =
  [text|
    ${prefix} ${sumTypename}_${name} ${typename}
  |]

elmSumTypeDef typename sumFields =
  [text|
    type ${typename}
      ${sumFields}
  |]

elmContractTypeDef typename =
  [text|
    type ${typename}Contract = Opaque_${typename}Contract
  |]

elmDecode prefix modulename typename =
  [text|
    decode${typename} : ArrayBuffer -> ${typename}Contract
    decode${typename} = Native.${prefix}.${modulename}.decode${typename}
  |]

elmEncode prefix modulename typename =
  [text|
    encode${typename} : ${typename}Contract -> ArrayBuffer
    encode${typename} = Native.${prefix}.${modulename}.encode${typename}
  |]

elmUnmarshal prefix modulename typename =
  [text|
    unmarshal${typename} : ${typename}Contract -> ${typename}
    unmarshal${typename} = Native.${prefix}.${modulename}.unmarshal${typename}
  |]

elmMarshal prefix modulename typename =
  [text|
    marshal${typename} : ${typename} -> ${typename}Contract
    marshal${typename} = Native.${prefix}.${modulename}.marshal${typename}
  |]
