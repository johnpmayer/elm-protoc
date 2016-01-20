{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Templates where

import Data.Text                (Text)
import NeatInterpolation        (text)

nativeModule packagename modulename protoSource moduleValues moduleExports =
  [text|
    Elm.Native.${modulename} = Elm.Native.${modulename} || {}
    Elm.Native.${modulename} = function(_elm) {
      "use strict";
      _elm.Native.${modulename} = _elm.Native.${modulename} || {}
      if (_elm.Native.${modulename}.values) {
        return _elm.Native.${modulename}.values
      }
      
      // .proto source
      var protoSource = `${protoSource}`;
      
      var ProtoBuilder = dcodeIO.ProtoBuf.loadProto(protoSource);
      var Proto = ProtoBuilder.build("${packagename}");

      ${moduleValues}

      return _elm.Native.${modulename}.values = {
        ${moduleExports}
      }
    }
  |]

nativeModuleExport valuename =
  [text|
    ${valuename}: ${valuename},
  |]

nativeDecode typename = 
  [text|
    var decode${typename} = function(blob) {
      return Proto.${typename}.decode(blob);
    }
  |]

nativeEncode typename = 
  [text|
    var encode${typename} = function(message_${typename}} {
      return message_${typename}.toArrayBuffer();
    }
  |]

nativeMarshal typename = 
  [text|
    var marshal${typename} = function(value_${typename}} {
    }
  |]
  
nativeUnmarshal typename = 
  [text|
    var unmarshal${typename} = function(message_${typename}} {
    }
  |]
  
elmModule modulename moduleExports types contractTypeDefs modulevalues =
  [text|
    module ${modulename} 
      ${moduleExports} ) where
    
    import Native.${modulename}
    
    ${types}
    
    -- Opaque Type definitions
    ${contractTypeDefs}
    
    ${modulevalues}
  |]

elmExport prefix name = 
  [text|
    ${prefix} ${name}
  |]

elmRecordField prefix (name, typename) =
  [text|
    ${prefix} ${name} : ${typename}
  |]
  
elmRecordTypeDef typename recordFields =
  [text|
    type alias ${typename} = 
      ${recordFields} }
  |]
  
elmContractTypeDef typename =
  [text|
    type ${typename}Contract = Opaque_${typename}Contract
  |]

elmDecode modulename typename = 
  [text|
    decode${typename} : Buffer -> ${typename}Contract
    decode${typename} = Native.${modulename}.decode${typename}
  |]

elmEncode modulename typename =
  [text|
    encode${typename} : ${typename}Contract -> Buffer
    encode${typename} = Native.${modulename}.encode${typename}
  |]

elmUnmarshal modulename typename =
  [text|
    unmarshal${typename} : ${typename}Contract -> ${typename}
    unmarshal${typename} = Native.${modulename}.unmarshal${typename}
  |]

elmMarshal modulename typename = 
  [text|
    marshal${typename} : ${typename} -> ${typename}Contract
    marshal${typename} = Native.${modulename}.marshal${typename}
  |]