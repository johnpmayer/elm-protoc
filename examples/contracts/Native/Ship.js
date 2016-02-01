Elm.Native.Ship = Elm.Native.Ship || {};
Elm.Native.Ship.make = function(_elm) {
  "use strict";
  _elm.Native.Ship = _elm.Native.Ship || {};
  if (_elm.Native.Ship.values) {
    return _elm.Native.Ship.values;
  }

  var Proto = Elm.Native.ElmProto.make(_elm);

  var encodeVessel = function(message_Vessel) {
    return message_Vessel.serializeBinary()
  }
  var decodeVessel = function(blob) {
    return ElmProto.Vessel.deserializeBinary(blob);
  }
  var encodeFuelTank = function(message_FuelTank) {
    return message_FuelTank.serializeBinary()
  }
  var decodeFuelTank = function(blob) {
    return ElmProto.FuelTank.deserializeBinary(blob);
  }
  var encodeEngine = function(message_Engine) {
    return message_Engine.serializeBinary()
  }
  var decodeEngine = function(blob) {
    return ElmProto.Engine.deserializeBinary(blob);
  }
  var encodePart = function(message_Part) {
    return message_Part.serializeBinary()
  }
  var decodePart = function(blob) {
    return ElmProto.Part.deserializeBinary(blob);
  }
  var encodeBeam = function(message_Beam) {
    return message_Beam.serializeBinary()
  }
  var decodeBeam = function(blob) {
    return ElmProto.Beam.deserializeBinary(blob);
  }
  var encodeRoot = function(message_Root) {
    return message_Root.serializeBinary()
  }
  var decodeRoot = function(blob) {
    return ElmProto.Root.deserializeBinary(blob);
  }
  var encodeAttach = function(message_Attach) {
    return message_Attach.serializeBinary()
  }
  var decodeAttach = function(blob) {
    return ElmProto.Attach.deserializeBinary(blob);
  }
  var encodeStructureNode = function(message_StructureNode) {
    return message_StructureNode.serializeBinary()
  }
  var decodeStructureNode = function(blob) {
    return ElmProto.StructureNode.deserializeBinary(blob);
  }
  var encodeStructureLink = function(message_StructureLink) {
    return message_StructureLink.serializeBinary()
  }
  var decodeStructureLink = function(blob) {
    return ElmProto.StructureLink.deserializeBinary(blob);
  }
  var encodeStructureTree = function(message_StructureTree) {
    return message_StructureTree.serializeBinary()
  }
  var decodeStructureTree = function(blob) {
    return ElmProto.StructureTree.deserializeBinary(blob);
  }
  var encodeEndMarker = function(message_EndMarker) {
    return message_EndMarker.serializeBinary()
  }
  var decodeEndMarker = function(blob) {
    return ElmProto.EndMarker.deserializeBinary(blob);
  }
  var encodeStructureData = function(message_StructureData) {
    return message_StructureData.serializeBinary()
  }
  var decodeStructureData = function(blob) {
    return ElmProto.StructureData.deserializeBinary(blob);
  }
  var encodeStructure = function(message_Structure) {
    return message_Structure.serializeBinary()
  }
  var decodeStructure = function(blob) {
    return ElmProto.Structure.deserializeBinary(blob);
  }
  var encodePhysicsState = function(message_PhysicsState) {
    return message_PhysicsState.serializeBinary()
  }
  var decodePhysicsState = function(blob) {
    return ElmProto.PhysicsState.deserializeBinary(blob);
  }
  var encodeShip = function(message_Ship) {
    return message_Ship.serializeBinary()
  }
  var decodeShip = function(blob) {
    return ElmProto.Ship.deserializeBinary(blob);
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
