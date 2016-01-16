{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Templates where

import Data.Text                (Text)
import NeatInterpolation        (text)

-- TODO - should really support a list 
nativeModule :: Text -> Text -> Text -> Text
nativeModule packagename modulename protoSource =
  [text|
    Elm.Native.$modulename = Elm.Native.$modulename || {}
    Elm.Native.$modulename = function(_elm) {
      "use strict";
      _elm.Native.$modulename = _elm.Native.$modulename || {}
      if (_elm.Native.$modulename.values) {
        return _elm.Native.$modulename.values
      }
      
      // .proto source
      var protoSource = `$protoSource`;
      
      var ProtoBuilder = dcodeIO.ProtoBuf.loadProto(protoSource);
      var Proto = ProtoBuilder.build("$packagename");
    }
  |]
  