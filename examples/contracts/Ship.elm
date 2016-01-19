module Ship 
  ( VesselContract
  , FuelTankContract
  , EngineContract
  , PartContract
  , BeamContract
  , RootContract
  , AttachContract
  , StructureNodeContract
  , StructureLinkContract
  , StructureTreeContract
  , EndMarkerContract
  , StructureDataContract
  , StructureContract
  , PhysicsStateContract
  , ShipContract
  , encodeVessel
  , decodeVessel
  , marshalVessel
  , unmarshalVessel
  , encodeFuelTank
  , decodeFuelTank
  , marshalFuelTank
  , unmarshalFuelTank
  , encodeEngine
  , decodeEngine
  , marshalEngine
  , unmarshalEngine
  , encodePart
  , decodePart
  , marshalPart
  , unmarshalPart
  , encodeBeam
  , decodeBeam
  , marshalBeam
  , unmarshalBeam
  , encodeRoot
  , decodeRoot
  , marshalRoot
  , unmarshalRoot
  , encodeAttach
  , decodeAttach
  , marshalAttach
  , unmarshalAttach
  , encodeStructureNode
  , decodeStructureNode
  , marshalStructureNode
  , unmarshalStructureNode
  , encodeStructureLink
  , decodeStructureLink
  , marshalStructureLink
  , unmarshalStructureLink
  , encodeStructureTree
  , decodeStructureTree
  , marshalStructureTree
  , unmarshalStructureTree
  , encodeEndMarker
  , decodeEndMarker
  , marshalEndMarker
  , unmarshalEndMarker
  , encodeStructureData
  , decodeStructureData
  , marshalStructureData
  , unmarshalStructureData
  , encodeStructure
  , decodeStructure
  , marshalStructure
  , unmarshalStructure
  , encodePhysicsState
  , decodePhysicsState
  , marshalPhysicsState
  , unmarshalPhysicsState
  , encodeShip
  , decodeShip
  , marshalShip
  , unmarshalShip ) where

import Native.Ship

-- Opaque Type definitions
type VesselContract = Opaque_VesselContract
type FuelTankContract = Opaque_FuelTankContract
type EngineContract = Opaque_EngineContract
type PartContract = Opaque_PartContract
type BeamContract = Opaque_BeamContract
type RootContract = Opaque_RootContract
type AttachContract = Opaque_AttachContract
type StructureNodeContract = Opaque_StructureNodeContract
type StructureLinkContract = Opaque_StructureLinkContract
type StructureTreeContract = Opaque_StructureTreeContract
type EndMarkerContract = Opaque_EndMarkerContract
type StructureDataContract = Opaque_StructureDataContract
type StructureContract = Opaque_StructureContract
type PhysicsStateContract = Opaque_PhysicsStateContract
type ShipContract = Opaque_ShipContract

encodeVessel : VesselContract -> Buffer
encodeVessel = Native.Ship.encodeVessel
decodeVessel : Buffer -> VesselContract
decodeVessel = Native.Ship.decodeVessel
marshalVessel : Vessel -> VesselContract
marshalVessel = Native.Ship.marshalVessel
unmarshalVessel : VesselContract -> Vessel
unmarshalVessel = Native.Ship.unmarshalVessel
encodeFuelTank : FuelTankContract -> Buffer
encodeFuelTank = Native.Ship.encodeFuelTank
decodeFuelTank : Buffer -> FuelTankContract
decodeFuelTank = Native.Ship.decodeFuelTank
marshalFuelTank : FuelTank -> FuelTankContract
marshalFuelTank = Native.Ship.marshalFuelTank
unmarshalFuelTank : FuelTankContract -> FuelTank
unmarshalFuelTank = Native.Ship.unmarshalFuelTank
encodeEngine : EngineContract -> Buffer
encodeEngine = Native.Ship.encodeEngine
decodeEngine : Buffer -> EngineContract
decodeEngine = Native.Ship.decodeEngine
marshalEngine : Engine -> EngineContract
marshalEngine = Native.Ship.marshalEngine
unmarshalEngine : EngineContract -> Engine
unmarshalEngine = Native.Ship.unmarshalEngine
encodePart : PartContract -> Buffer
encodePart = Native.Ship.encodePart
decodePart : Buffer -> PartContract
decodePart = Native.Ship.decodePart
marshalPart : Part -> PartContract
marshalPart = Native.Ship.marshalPart
unmarshalPart : PartContract -> Part
unmarshalPart = Native.Ship.unmarshalPart
encodeBeam : BeamContract -> Buffer
encodeBeam = Native.Ship.encodeBeam
decodeBeam : Buffer -> BeamContract
decodeBeam = Native.Ship.decodeBeam
marshalBeam : Beam -> BeamContract
marshalBeam = Native.Ship.marshalBeam
unmarshalBeam : BeamContract -> Beam
unmarshalBeam = Native.Ship.unmarshalBeam
encodeRoot : RootContract -> Buffer
encodeRoot = Native.Ship.encodeRoot
decodeRoot : Buffer -> RootContract
decodeRoot = Native.Ship.decodeRoot
marshalRoot : Root -> RootContract
marshalRoot = Native.Ship.marshalRoot
unmarshalRoot : RootContract -> Root
unmarshalRoot = Native.Ship.unmarshalRoot
encodeAttach : AttachContract -> Buffer
encodeAttach = Native.Ship.encodeAttach
decodeAttach : Buffer -> AttachContract
decodeAttach = Native.Ship.decodeAttach
marshalAttach : Attach -> AttachContract
marshalAttach = Native.Ship.marshalAttach
unmarshalAttach : AttachContract -> Attach
unmarshalAttach = Native.Ship.unmarshalAttach
encodeStructureNode : StructureNodeContract -> Buffer
encodeStructureNode = Native.Ship.encodeStructureNode
decodeStructureNode : Buffer -> StructureNodeContract
decodeStructureNode = Native.Ship.decodeStructureNode
marshalStructureNode : StructureNode -> StructureNodeContract
marshalStructureNode = Native.Ship.marshalStructureNode
unmarshalStructureNode : StructureNodeContract -> StructureNode
unmarshalStructureNode = Native.Ship.unmarshalStructureNode
encodeStructureLink : StructureLinkContract -> Buffer
encodeStructureLink = Native.Ship.encodeStructureLink
decodeStructureLink : Buffer -> StructureLinkContract
decodeStructureLink = Native.Ship.decodeStructureLink
marshalStructureLink : StructureLink -> StructureLinkContract
marshalStructureLink = Native.Ship.marshalStructureLink
unmarshalStructureLink : StructureLinkContract -> StructureLink
unmarshalStructureLink = Native.Ship.unmarshalStructureLink
encodeStructureTree : StructureTreeContract -> Buffer
encodeStructureTree = Native.Ship.encodeStructureTree
decodeStructureTree : Buffer -> StructureTreeContract
decodeStructureTree = Native.Ship.decodeStructureTree
marshalStructureTree : StructureTree -> StructureTreeContract
marshalStructureTree = Native.Ship.marshalStructureTree
unmarshalStructureTree : StructureTreeContract -> StructureTree
unmarshalStructureTree = Native.Ship.unmarshalStructureTree
encodeEndMarker : EndMarkerContract -> Buffer
encodeEndMarker = Native.Ship.encodeEndMarker
decodeEndMarker : Buffer -> EndMarkerContract
decodeEndMarker = Native.Ship.decodeEndMarker
marshalEndMarker : EndMarker -> EndMarkerContract
marshalEndMarker = Native.Ship.marshalEndMarker
unmarshalEndMarker : EndMarkerContract -> EndMarker
unmarshalEndMarker = Native.Ship.unmarshalEndMarker
encodeStructureData : StructureDataContract -> Buffer
encodeStructureData = Native.Ship.encodeStructureData
decodeStructureData : Buffer -> StructureDataContract
decodeStructureData = Native.Ship.decodeStructureData
marshalStructureData : StructureData -> StructureDataContract
marshalStructureData = Native.Ship.marshalStructureData
unmarshalStructureData : StructureDataContract -> StructureData
unmarshalStructureData = Native.Ship.unmarshalStructureData
encodeStructure : StructureContract -> Buffer
encodeStructure = Native.Ship.encodeStructure
decodeStructure : Buffer -> StructureContract
decodeStructure = Native.Ship.decodeStructure
marshalStructure : Structure -> StructureContract
marshalStructure = Native.Ship.marshalStructure
unmarshalStructure : StructureContract -> Structure
unmarshalStructure = Native.Ship.unmarshalStructure
encodePhysicsState : PhysicsStateContract -> Buffer
encodePhysicsState = Native.Ship.encodePhysicsState
decodePhysicsState : Buffer -> PhysicsStateContract
decodePhysicsState = Native.Ship.decodePhysicsState
marshalPhysicsState : PhysicsState -> PhysicsStateContract
marshalPhysicsState = Native.Ship.marshalPhysicsState
unmarshalPhysicsState : PhysicsStateContract -> PhysicsState
unmarshalPhysicsState = Native.Ship.unmarshalPhysicsState
encodeShip : ShipContract -> Buffer
encodeShip = Native.Ship.encodeShip
decodeShip : Buffer -> ShipContract
decodeShip = Native.Ship.decodeShip
marshalShip : Ship -> ShipContract
marshalShip = Native.Ship.marshalShip
unmarshalShip : ShipContract -> Ship
unmarshalShip = Native.Ship.unmarshalShip
