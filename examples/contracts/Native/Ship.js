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

  var encodeVessel = function(message_Vessel} {
    return message_Vessel.toArrayBuffer();
  }
  var decodeVessel = function(blob) {
    return Proto.Vessel.decode(blob);
  }
  var marshalVessel = function(value_Vessel} {
  }
  var unmarshalVessel = function(message_Vessel} {
  }
  var encodeFuelTank = function(message_FuelTank} {
    return message_FuelTank.toArrayBuffer();
  }
  var decodeFuelTank = function(blob) {
    return Proto.FuelTank.decode(blob);
  }
  var marshalFuelTank = function(value_FuelTank} {
  }
  var unmarshalFuelTank = function(message_FuelTank} {
  }
  var encodeEngine = function(message_Engine} {
    return message_Engine.toArrayBuffer();
  }
  var decodeEngine = function(blob) {
    return Proto.Engine.decode(blob);
  }
  var marshalEngine = function(value_Engine} {
  }
  var unmarshalEngine = function(message_Engine} {
  }
  var encodePart = function(message_Part} {
    return message_Part.toArrayBuffer();
  }
  var decodePart = function(blob) {
    return Proto.Part.decode(blob);
  }
  var marshalPart = function(value_Part} {
  }
  var unmarshalPart = function(message_Part} {
  }
  var encodeBeam = function(message_Beam} {
    return message_Beam.toArrayBuffer();
  }
  var decodeBeam = function(blob) {
    return Proto.Beam.decode(blob);
  }
  var marshalBeam = function(value_Beam} {
  }
  var unmarshalBeam = function(message_Beam} {
  }
  var encodeRoot = function(message_Root} {
    return message_Root.toArrayBuffer();
  }
  var decodeRoot = function(blob) {
    return Proto.Root.decode(blob);
  }
  var marshalRoot = function(value_Root} {
  }
  var unmarshalRoot = function(message_Root} {
  }
  var encodeAttach = function(message_Attach} {
    return message_Attach.toArrayBuffer();
  }
  var decodeAttach = function(blob) {
    return Proto.Attach.decode(blob);
  }
  var marshalAttach = function(value_Attach} {
  }
  var unmarshalAttach = function(message_Attach} {
  }
  var encodeStructureNode = function(message_StructureNode} {
    return message_StructureNode.toArrayBuffer();
  }
  var decodeStructureNode = function(blob) {
    return Proto.StructureNode.decode(blob);
  }
  var marshalStructureNode = function(value_StructureNode} {
  }
  var unmarshalStructureNode = function(message_StructureNode} {
  }
  var encodeStructureLink = function(message_StructureLink} {
    return message_StructureLink.toArrayBuffer();
  }
  var decodeStructureLink = function(blob) {
    return Proto.StructureLink.decode(blob);
  }
  var marshalStructureLink = function(value_StructureLink} {
  }
  var unmarshalStructureLink = function(message_StructureLink} {
  }
  var encodeStructureTree = function(message_StructureTree} {
    return message_StructureTree.toArrayBuffer();
  }
  var decodeStructureTree = function(blob) {
    return Proto.StructureTree.decode(blob);
  }
  var marshalStructureTree = function(value_StructureTree} {
  }
  var unmarshalStructureTree = function(message_StructureTree} {
  }
  var encodeEndMarker = function(message_EndMarker} {
    return message_EndMarker.toArrayBuffer();
  }
  var decodeEndMarker = function(blob) {
    return Proto.EndMarker.decode(blob);
  }
  var marshalEndMarker = function(value_EndMarker} {
  }
  var unmarshalEndMarker = function(message_EndMarker} {
  }
  var encodeStructureData = function(message_StructureData} {
    return message_StructureData.toArrayBuffer();
  }
  var decodeStructureData = function(blob) {
    return Proto.StructureData.decode(blob);
  }
  var marshalStructureData = function(value_StructureData} {
  }
  var unmarshalStructureData = function(message_StructureData} {
  }
  var encodeStructure = function(message_Structure} {
    return message_Structure.toArrayBuffer();
  }
  var decodeStructure = function(blob) {
    return Proto.Structure.decode(blob);
  }
  var marshalStructure = function(value_Structure} {
  }
  var unmarshalStructure = function(message_Structure} {
  }
  var encodePhysicsState = function(message_PhysicsState} {
    return message_PhysicsState.toArrayBuffer();
  }
  var decodePhysicsState = function(blob) {
    return Proto.PhysicsState.decode(blob);
  }
  var marshalPhysicsState = function(value_PhysicsState} {
  }
  var unmarshalPhysicsState = function(message_PhysicsState} {
  }
  var encodeShip = function(message_Ship} {
    return message_Ship.toArrayBuffer();
  }
  var decodeShip = function(blob) {
    return Proto.Ship.decode(blob);
  }
  var marshalShip = function(value_Ship} {
  }
  var unmarshalShip = function(message_Ship} {
  }

  return _elm.Native.Ship.values = {
    encodeVessel: encodeVessel,
    decodeVessel: decodeVessel,
    marshalVessel: marshalVessel,
    unmarshalVessel: unmarshalVessel,
    encodeFuelTank: encodeFuelTank,
    decodeFuelTank: decodeFuelTank,
    marshalFuelTank: marshalFuelTank,
    unmarshalFuelTank: unmarshalFuelTank,
    encodeEngine: encodeEngine,
    decodeEngine: decodeEngine,
    marshalEngine: marshalEngine,
    unmarshalEngine: unmarshalEngine,
    encodePart: encodePart,
    decodePart: decodePart,
    marshalPart: marshalPart,
    unmarshalPart: unmarshalPart,
    encodeBeam: encodeBeam,
    decodeBeam: decodeBeam,
    marshalBeam: marshalBeam,
    unmarshalBeam: unmarshalBeam,
    encodeRoot: encodeRoot,
    decodeRoot: decodeRoot,
    marshalRoot: marshalRoot,
    unmarshalRoot: unmarshalRoot,
    encodeAttach: encodeAttach,
    decodeAttach: decodeAttach,
    marshalAttach: marshalAttach,
    unmarshalAttach: unmarshalAttach,
    encodeStructureNode: encodeStructureNode,
    decodeStructureNode: decodeStructureNode,
    marshalStructureNode: marshalStructureNode,
    unmarshalStructureNode: unmarshalStructureNode,
    encodeStructureLink: encodeStructureLink,
    decodeStructureLink: decodeStructureLink,
    marshalStructureLink: marshalStructureLink,
    unmarshalStructureLink: unmarshalStructureLink,
    encodeStructureTree: encodeStructureTree,
    decodeStructureTree: decodeStructureTree,
    marshalStructureTree: marshalStructureTree,
    unmarshalStructureTree: unmarshalStructureTree,
    encodeEndMarker: encodeEndMarker,
    decodeEndMarker: decodeEndMarker,
    marshalEndMarker: marshalEndMarker,
    unmarshalEndMarker: unmarshalEndMarker,
    encodeStructureData: encodeStructureData,
    decodeStructureData: decodeStructureData,
    marshalStructureData: marshalStructureData,
    unmarshalStructureData: unmarshalStructureData,
    encodeStructure: encodeStructure,
    decodeStructure: decodeStructure,
    marshalStructure: marshalStructure,
    unmarshalStructure: unmarshalStructure,
    encodePhysicsState: encodePhysicsState,
    decodePhysicsState: decodePhysicsState,
    marshalPhysicsState: marshalPhysicsState,
    unmarshalPhysicsState: unmarshalPhysicsState,
    encodeShip: encodeShip,
    decodeShip: decodeShip,
    marshalShip: marshalShip,
    unmarshalShip: unmarshalShip,
  }
}
