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

decodeNative typename = 
  [text|
    var decode${typename} = function(blob) {
      return Proto.${typename}.decode(blob);
    }
  |]

decodeElm modulename typename = 
  [text|
    decode${typename} : Buffer -> ${typename}Contract
    decode${typename} = Native.${modulename}.decode${typename}
  |]

encodeNative typename = 
  [text|
    var encode${typename} = function(message_${typename}} {
      return message_${typename}.toArrayBuffer();
    }
  |]

encodeElm modulename typename =
  [text|
    encode${typename} : ${typename}Contract -> Buffer
    encode${typename} = Native.${modulename}.encode${typename}
  |]

