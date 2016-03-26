{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Templates where

import qualified Data.Text as T

import Data.Int                 (Int32)
import Data.Text                (Text)
import NeatInterpolation        (text)

import Utils

nativeModule prefix protoModulename filename packagename modulename protoSource moduleValues moduleExports =
  [text|
    Elm.Native.${prefix} = Elm.Native.${prefix} || {};
    Elm.Native.${prefix}.${modulename} = Elm.Native.${prefix}.${modulename} || {};
    Elm.Native.${prefix}.${modulename}.make = function(_elm) {
      "use strict";
      _elm.Native.${prefix} = _elm.Native.${prefix} || {};
      _elm.Native.${prefix}.${modulename} = _elm.Native.${prefix}.${modulename} || {};
      if (_elm.Native.${prefix}.${modulename}.values) {
        return _elm.Native.${prefix}.${modulename}.values;
      }

      // reference to the elm module - don't build (circular dep)
      // var ${modulename} = Elm.${prefix}.${modulename}.make(_elm);
      var _elm_module = _elm.${prefix}.${modulename};

      // reference to the protobuf generated JavaScript
      var Proto = Elm.Native.${protoModulename}.Internal.Proto.make(_elm);

      ${moduleValues}

      return _elm.Native.${prefix}.${modulename}.values = {
        ${moduleExports}
      }
    }
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
    setMethod = T.concat [T.pack "set", toTitlePreserving fieldName]
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
    setMethod = T.concat [T.pack "set", toTitlePreserving fieldName]
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
    getter = T.concat [T.pack "get", toTitlePreserving fieldName]
  in
    [text|
      var ${fieldName} = ${contractValueName}.${getter}();
    |]

unmarshalMaybePrimitive :: Text -> (Text -> Text)
unmarshalMaybePrimitive fieldName = \contractValueName ->
  let
    getter = T.concat [T.pack "get", toTitlePreserving fieldName]
  in
    [text|
      throw "Not implemented - optional field unmarshaling (${fieldName})";
      var ${fieldName} = ${contractValueName}.${getter}();
    |]

unmarshalListPrimitive :: Text -> (Text -> Text)
unmarshalListPrimitive fieldName = \contractValueName ->
  let
    getter = T.concat [T.pack "get", toTitlePreserving fieldName]
  in
    [text|
      throw "Not implemented - repeated field unmarshaling (${fieldName})";
      var ${fieldName} = ${contractValueName}.${getter}();
    |]

unmarshalFunc :: Text -> Text -> Text -> (Text -> Text)
unmarshalFunc qualifier typename fieldName = \contractValueName ->
  let
    getter = T.concat [T.pack "get", toTitlePreserving fieldName]
  in
    [text|
      var ${fieldName} = ${qualifier}unmarshal${typename}(${contractValueName}.${getter}());
    |]

unmarshalMaybeFunc :: Text -> Text -> Text -> (Text -> Text)
unmarshalMaybeFunc qualifier typename fieldName = \contractValueName ->
  let
    getter = T.concat [T.pack "get", toTitlePreserving fieldName]
  in
    [text|
      throw "Not implemented - optional field unmarshaling (${fieldName})";
      var ${fieldName} = ${qualifier}unmarshal${typename}(${contractValueName}.${getter}());
    |]

unmarshalListFunc :: Text -> Text -> Text -> (Text -> Text)
unmarshalListFunc qualifier typename fieldName = \contractValueName ->
  let
    getter = T.concat [T.pack "get", toTitlePreserving fieldName]
  in
    [text|
      throw "Not implemented - repeated field unmarshaling (${fieldName})";
      var ${fieldName} = ${qualifier}unmarshal${typename}(${contractValueName}.${getter}());
    |]

nativeOneofUnmarshal :: Text -> (Text -> Text)
nativeOneofUnmarshal oneofFieldName = \contractValueName ->
  [text|
    // something with ${contractValueName}.${oneofFieldName} ???
    throw new Error("Not implemented - oneof field unmarshaling (${oneofFieldName})");
  |]

nativeMessageUnmarshal :: Text -> [(Text, Text -> Text)] -> Text
nativeMessageUnmarshal typename fields =
  let
    contractValueName = T.concat [ typename, T.pack "_contract" ]
    makeFieldUnmarshalStatement field = snd field $ contractValueName
    fieldUnmarshalStatements = T.unlines $ fmap makeFieldUnmarshalStatement fields
    constructorArguments = T.concat $ fmap (((T.pack ", ") `T.append`) . fst) fields
    applyN = T.concat [ T.pack "A", T.pack . show . length $ fields ]
  in
    [text|
      var unmarshal${typename} = function(${contractValueName}) {
        ${fieldUnmarshalStatements}
        return ${applyN}(_elm_module.values.${typename}${constructorArguments});
      }
    |]

elmModule prefix modulename moduleExports dependencyImports types contractTypeDefs modulevalues comment =
  [text|

    {-
     - ${comment}
     -}

    module ${prefix}.${modulename}
      ${moduleExports} ) where

    import Opaque exposing (Buffer)

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
    decode${typename} : Buffer -> ${typename}Contract
    decode${typename} = Native.${prefix}.${modulename}.decode${typename}
  |]

elmEncode prefix modulename typename =
  [text|
    encode${typename} : ${typename}Contract -> Buffer
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
