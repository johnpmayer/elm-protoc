Elm.Native.Actions = Elm.Native.Actions || {}
Elm.Native.Actions = function(_elm) {
  "use strict";
  _elm.Native.Actions = _elm.Native.Actions || {}
  if (_elm.Native.Actions.values) {
    return _elm.Native.Actions.values
  }
  
  // .proto source
  var protoSource = `
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
  }`;
  
  var ProtoBuilder = dcodeIO.ProtoBuf.loadProto(protoSource);
  var Proto = ProtoBuilder.build("actions");

  var decodeUnit = function(blob) {
    return Proto.Unit.decode(blob);
  }
  var decodeActive = function(blob) {
    return Proto.Active.decode(blob);
  }
  var decodeControls = function(blob) {
    return Proto.Controls.decode(blob);
  }
  var decodeBuild = function(blob) {
    return Proto.Build.decode(blob);
  }
  var decodeAction = function(blob) {
    return Proto.Action.decode(blob);
  }
  var encodeUnit = function(message_Unit} {
    return message_Unit.toArrayBuffer();
  }
  var encodeActive = function(message_Active} {
    return message_Active.toArrayBuffer();
  }
  var encodeControls = function(message_Controls} {
    return message_Controls.toArrayBuffer();
  }
  var encodeBuild = function(message_Build} {
    return message_Build.toArrayBuffer();
  }
  var encodeAction = function(message_Action} {
    return message_Action.toArrayBuffer();
  }

  return _elm.Native.Actions.values = {
    encodeUnit: encodeUnit,
    decodeUnit: decodeUnit,
    encodeActive: encodeActive,
    decodeActive: decodeActive,
    encodeControls: encodeControls,
    decodeControls: decodeControls,
    encodeBuild: encodeBuild,
    decodeBuild: decodeBuild,
    encodeAction: encodeAction,
    decodeAction: decodeAction,
  }
}
