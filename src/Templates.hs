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

elmModule modulename moduleExports contractTypeDefs modulevalues =
  [text|
    module ${modulename} 
      ${moduleExports} ) where
    
    import Native.${modulename}
    
    -- Opaque Type definitions
    ${contractTypeDefs}
    
    ${modulevalues}
  |]

elmExport prefix name = 
  [text|
      ${prefix} ${name}
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

