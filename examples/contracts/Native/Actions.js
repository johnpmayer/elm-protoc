Elm.Native.Actions = Elm.Native.Actions || {}
Elm.Native.Actions.make = function(_elm) {
  "use strict";
  _elm.Native.Actions = _elm.Native.Actions || {}
  if (_elm.Native.Actions.values) {
    return _elm.Native.Actions.values
  }
  
  // .proto source
  var protoSource = `
  syntax = "proto2";
  
  package actions;
  
  message Unit {};
  
  message Active {
    repeated int32 groups = 1;
  }
  
  message Controls {
    oneof controls {
      Unit brakes = 1;
      Active active = 2;
    }
  }
  
  message Build {
    required int32 foo = 1;
  }
  
  message Action {
    oneof action {
      Controls controls = 1;
      Build build = 2;
    }
  }`;
  
  var ProtoBuilder = dcodeIO.ProtoBuf.loadProto(protoSource, "actions.proto");
  var Proto = ProtoBuilder.build("actions");

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
