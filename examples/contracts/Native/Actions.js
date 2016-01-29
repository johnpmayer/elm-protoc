Elm.Native.Actions = Elm.Native.Actions || {};
Elm.Native.Actions.make = function(_elm) {
  "use strict";
  _elm.Native.Actions = _elm.Native.Actions || {};
  if (_elm.Native.Actions.values) {
    return _elm.Native.Actions.values;
  }

  var Proto = Elm.Native.ElmProto.make(_elm);

  var encodeUnit = function(message_Unit) {
    return message_Unit.toArrayBuffer();
  }
  var decodeUnit = function(blob) {
    return Proto.Unit.decode(blob);
  }
  var encodeActive = function(message_Active) {
    return message_Active.toArrayBuffer();
  }
  var decodeActive = function(blob) {
    return Proto.Active.decode(blob);
  }
  var encodeControls = function(message_Controls) {
    return message_Controls.toArrayBuffer();
  }
  var decodeControls = function(blob) {
    return Proto.Controls.decode(blob);
  }
  var encodeBuild = function(message_Build) {
    return message_Build.toArrayBuffer();
  }
  var decodeBuild = function(blob) {
    return Proto.Build.decode(blob);
  }
  var encodeAction = function(message_Action) {
    return message_Action.toArrayBuffer();
  }
  var decodeAction = function(blob) {
    return Proto.Action.decode(blob);
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
