Elm.Native.World = Elm.Native.World || {};
Elm.Native.World.make = function(_elm) {
  "use strict";
  _elm.Native.World = _elm.Native.World || {};
  if (_elm.Native.World.values) {
    return _elm.Native.World.values;
  }

  var Proto = Elm.Native.ElmProto.make(_elm);

  var encodeSnapshot = function(message_Snapshot) {
    return message_Snapshot.serializeBinary()
  }
  var decodeSnapshot = function(blob) {
    return ElmProto.Snapshot.deserializeBinary(blob);
  }
  var encodeGameUpdate = function(message_GameUpdate) {
    return message_GameUpdate.serializeBinary()
  }
  var decodeGameUpdate = function(blob) {
    return ElmProto.GameUpdate.deserializeBinary(blob);
  }

  return _elm.Native.World.values = {
    encodeSnapshot: encodeSnapshot,
    decodeSnapshot: decodeSnapshot,
    marshalSnapshot: marshalSnapshot,
    unmarshalSnapshot: unmarshalSnapshot,
    encodeGameUpdate: encodeGameUpdate,
    decodeGameUpdate: decodeGameUpdate,
    marshalGameUpdate: marshalGameUpdate,
    unmarshalGameUpdate: unmarshalGameUpdate,
  }
}
