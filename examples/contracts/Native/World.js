Elm.Native.World = Elm.Native.World || {};
Elm.Native.World.make = function(_elm) {
  "use strict";
  _elm.Native.World = _elm.Native.World || {};
  if (_elm.Native.World.values) {
    return _elm.Native.World.values;
  }

  var Proto = Elm.Native.ElmProto.make(_elm);

  var encodeSnapshot = function(message_Snapshot) {
    return message_Snapshot.toArrayBuffer();
  }
  var decodeSnapshot = function(blob) {
    return Proto.Snapshot.decode(blob);
  }
  var encodeGameUpdate = function(message_GameUpdate) {
    return message_GameUpdate.toArrayBuffer();
  }
  var decodeGameUpdate = function(blob) {
    return Proto.GameUpdate.decode(blob);
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
