Elm.Native.Actions = Elm.Native.Actions || {};
Elm.Native.Actions.make = function(_elm) {
  "use strict";
  _elm.Native.Actions = _elm.Native.Actions || {};
  if (_elm.Native.Actions.values) {
    return _elm.Native.Actions.values;
  }

  var Proto = Elm.Native.ElmProto.make(_elm);

  var encodeUnit = function(message_Unit) {
    return message_Unit.serializeBinary()
  }
  var decodeUnit = function(blob) {
    return ElmProto.Unit.deserializeBinary(blob);
  }
  var encodeActive = function(message_Active) {
    return message_Active.serializeBinary()
  }
  var decodeActive = function(blob) {
    return ElmProto.Active.deserializeBinary(blob);
  }
  var encodeControls = function(message_Controls) {
    return message_Controls.serializeBinary()
  }
  var decodeControls = function(blob) {
    return ElmProto.Controls.deserializeBinary(blob);
  }
  var encodeBuild = function(message_Build) {
    return message_Build.serializeBinary()
  }
  var decodeBuild = function(blob) {
    return ElmProto.Build.deserializeBinary(blob);
  }
  var encodeAction = function(message_Action) {
    return message_Action.serializeBinary()
  }
  var decodeAction = function(blob) {
    return ElmProto.Action.deserializeBinary(blob);
  }

  return _elm.Native.Actions.values = {
    encodeUnit: encodeUnit,
    decodeUnit: decodeUnit,
    marshalUnit: marshalUnit,
    unmarshalUnit: unmarshalUnit,
    encodeActive: encodeActive,
    decodeActive: decodeActive,
    marshalActive: marshalActive,
    unmarshalActive: unmarshalActive,
    encodeControls: encodeControls,
    decodeControls: decodeControls,
    marshalControls: marshalControls,
    unmarshalControls: unmarshalControls,
    encodeBuild: encodeBuild,
    decodeBuild: decodeBuild,
    marshalBuild: marshalBuild,
    unmarshalBuild: unmarshalBuild,
    encodeAction: encodeAction,
    decodeAction: decodeAction,
    marshalAction: marshalAction,
    unmarshalAction: unmarshalAction,
  }
}
