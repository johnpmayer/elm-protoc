{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Templates where

import qualified Data.Text as T

import Data.Text                (Text)
import NeatInterpolation        (text)

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

      var Proto = Elm.Native.${protoModulename}.make(_elm);

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

nativeDecode protoModulename typename = 
  [text|
    var decode${typename} = function(blob) {
      return ${protoModulename}.${typename}.deserializeBinary(blob);
    }
  |]

nativeEncode protoModulename typename = 
  [text|
    var encode${typename} = function(message_${typename}) {
      return message_${typename}.serializeBinary()
    }
  |]

nativeOneofMarshal = undefined

nativeRecordMarshal protoModulename typename = 
  [text|
    var marshal${typename} = function(value_${typename}) {
      throw "Not implemented";
    }
  |]

nativeOneofUnmarshal = undefined

nativeFieldUnmarshalStatement contractValueName = undefined

nativeMessageUnmarshal :: Text -> Text -> [(Text, Text -> Text)] -> Text
nativeMessageUnmarshal protoModulename typename fields = 
  let 
    contractValueName = T.concat [ typename, T.pack "_contract" ]
    makeFieldUnmarshalStatement field = [text|
      var ${fst field} = ${snd field $ contractValueName};
    |]
    fieldUnmarshalStatements = T.unlines $ fmap makeFieldUnmarshalStatement fields
    constructorArguments = T.concat $ fmap (((T.pack ", ") `T.append`) . fst) fields
    applyN = T.concat [ T.pack "A", T.pack . show . length $ fields ]
  in [text|
    var unmarshal${typename} = function(${contractValueName}) {
      ${fieldUnmarshalStatements}
      return ${applyN}(${typename}${constructorArguments});
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
