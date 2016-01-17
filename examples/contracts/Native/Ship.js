Elm.Native.Ship = Elm.Native.Ship || {}
Elm.Native.Ship = function(_elm) {
  "use strict";
  _elm.Native.Ship = _elm.Native.Ship || {}
  if (_elm.Native.Ship.values) {
    return _elm.Native.Ship.values
  }
  
  // .proto source
  var protoSource = `
  package ship;
  
  import "actions.proto";
  
  message Vessel {
    required double width = 1;
    required double length = 2;
  };
  
  message FuelTank {
    required double radius = 1;
    required double length = 2;
  };
  
  message Engine {
    required double radius = 1;
    required double length = 2;
    required int32 group = 3;
  };
  
  message Part {
    oneof part {
      Vessel vessel = 1;
      FuelTank fuelTank = 2;
      Engine engine = 3;
    }
  }
  
  message Beam {
    required double length = 1;
  }
  
  message Root {};
  
  message Attach {
    required double location = 1;
    required double rotation = 2;
  }
  
  message StructureNode {
    oneof node {
      Beam beam = 1;
      Part part = 2;
    }
  }
  
  message StructureLink {
    oneof link {
      Root root = 1;
      Attach attach = 2;
    }
  }
  
  message StructureTree {
    required StructureNode node = 1;
    required StructureLink link = 2;
  }
  
  message EndMarker {};
  
  message StructureData {
    oneof structure {
      EndMarker marker = 1;
      StructureTree tree = 2; 
    }
  }
  
  message Structure { 
    repeated StructureData attachments = 1;
  }
  
  message PhysicsState {
    required double x = 1;
    required double y = 2;
    required double theta = 3;
    required double vx = 4;
    required double vy = 5;
    required double omega = 6;
  }
  
  message Ship {
    required uint64 entityId = 1;
    required Structure structure = 2;
    required PhysicsState physicsState = 3;
    required actions.Active active = 4;
  }`;
  
  var ProtoBuilder = dcodeIO.ProtoBuf.loadProto(protoSource);
  var Proto = ProtoBuilder.build("ship");

  var decodeVessel = function(blob) {
    return Proto.Vessel.decode(blob);
  }
  var decodeFuelTank = function(blob) {
    return Proto.FuelTank.decode(blob);
  }
  var decodeEngine = function(blob) {
    return Proto.Engine.decode(blob);
  }
  var decodePart = function(blob) {
    return Proto.Part.decode(blob);
  }
  var decodeBeam = function(blob) {
    return Proto.Beam.decode(blob);
  }
  var decodeRoot = function(blob) {
    return Proto.Root.decode(blob);
  }
  var decodeAttach = function(blob) {
    return Proto.Attach.decode(blob);
  }
  var decodeStructureNode = function(blob) {
    return Proto.StructureNode.decode(blob);
  }
  var decodeStructureLink = function(blob) {
    return Proto.StructureLink.decode(blob);
  }
  var decodeStructureTree = function(blob) {
    return Proto.StructureTree.decode(blob);
  }
  var decodeEndMarker = function(blob) {
    return Proto.EndMarker.decode(blob);
  }
  var decodeStructureData = function(blob) {
    return Proto.StructureData.decode(blob);
  }
  var decodeStructure = function(blob) {
    return Proto.Structure.decode(blob);
  }
  var decodePhysicsState = function(blob) {
    return Proto.PhysicsState.decode(blob);
  }
  var decodeShip = function(blob) {
    return Proto.Ship.decode(blob);
  }
  var encodeVessel = function(message_Vessel} {
    return message_Vessel.toArrayBuffer();
  }
  var encodeFuelTank = function(message_FuelTank} {
    return message_FuelTank.toArrayBuffer();
  }
  var encodeEngine = function(message_Engine} {
    return message_Engine.toArrayBuffer();
  }
  var encodePart = function(message_Part} {
    return message_Part.toArrayBuffer();
  }
  var encodeBeam = function(message_Beam} {
    return message_Beam.toArrayBuffer();
  }
  var encodeRoot = function(message_Root} {
    return message_Root.toArrayBuffer();
  }
  var encodeAttach = function(message_Attach} {
    return message_Attach.toArrayBuffer();
  }
  var encodeStructureNode = function(message_StructureNode} {
    return message_StructureNode.toArrayBuffer();
  }
  var encodeStructureLink = function(message_StructureLink} {
    return message_StructureLink.toArrayBuffer();
  }
  var encodeStructureTree = function(message_StructureTree} {
    return message_StructureTree.toArrayBuffer();
  }
  var encodeEndMarker = function(message_EndMarker} {
    return message_EndMarker.toArrayBuffer();
  }
  var encodeStructureData = function(message_StructureData} {
    return message_StructureData.toArrayBuffer();
  }
  var encodeStructure = function(message_Structure} {
    return message_Structure.toArrayBuffer();
  }
  var encodePhysicsState = function(message_PhysicsState} {
    return message_PhysicsState.toArrayBuffer();
  }
  var encodeShip = function(message_Ship} {
    return message_Ship.toArrayBuffer();
  }

  return _elm.Native.Ship.values = {
    encodeVessel: encodeVessel,
    decodeVessel: decodeVessel,
    encodeFuelTank: encodeFuelTank,
    decodeFuelTank: decodeFuelTank,
    encodeEngine: encodeEngine,
    decodeEngine: decodeEngine,
    encodePart: encodePart,
    decodePart: decodePart,
    encodeBeam: encodeBeam,
    decodeBeam: decodeBeam,
    encodeRoot: encodeRoot,
    decodeRoot: decodeRoot,
    encodeAttach: encodeAttach,
    decodeAttach: decodeAttach,
    encodeStructureNode: encodeStructureNode,
    decodeStructureNode: decodeStructureNode,
    encodeStructureLink: encodeStructureLink,
    decodeStructureLink: decodeStructureLink,
    encodeStructureTree: encodeStructureTree,
    decodeStructureTree: decodeStructureTree,
    encodeEndMarker: encodeEndMarker,
    decodeEndMarker: decodeEndMarker,
    encodeStructureData: encodeStructureData,
    decodeStructureData: decodeStructureData,
    encodeStructure: encodeStructure,
    decodeStructure: decodeStructure,
    encodePhysicsState: encodePhysicsState,
    decodePhysicsState: decodePhysicsState,
    encodeShip: encodeShip,
    decodeShip: decodeShip,
  }
}
