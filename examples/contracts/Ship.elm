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
  , encodeFuelTank
  , decodeFuelTank
  , encodeEngine
  , decodeEngine
  , encodePart
  , decodePart
  , encodeBeam
  , decodeBeam
  , encodeRoot
  , decodeRoot
  , encodeAttach
  , decodeAttach
  , encodeStructureNode
  , decodeStructureNode
  , encodeStructureLink
  , decodeStructureLink
  , encodeStructureTree
  , decodeStructureTree
  , encodeEndMarker
  , decodeEndMarker
  , encodeStructureData
  , decodeStructureData
  , encodeStructure
  , decodeStructure
  , encodePhysicsState
  , decodePhysicsState
  , encodeShip
  , decodeShip ) where

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

decodeVessel : Buffer -> VesselContract
decodeVessel = Native.Ship.decodeVessel
decodeFuelTank : Buffer -> FuelTankContract
decodeFuelTank = Native.Ship.decodeFuelTank
decodeEngine : Buffer -> EngineContract
decodeEngine = Native.Ship.decodeEngine
decodePart : Buffer -> PartContract
decodePart = Native.Ship.decodePart
decodeBeam : Buffer -> BeamContract
decodeBeam = Native.Ship.decodeBeam
decodeRoot : Buffer -> RootContract
decodeRoot = Native.Ship.decodeRoot
decodeAttach : Buffer -> AttachContract
decodeAttach = Native.Ship.decodeAttach
decodeStructureNode : Buffer -> StructureNodeContract
decodeStructureNode = Native.Ship.decodeStructureNode
decodeStructureLink : Buffer -> StructureLinkContract
decodeStructureLink = Native.Ship.decodeStructureLink
decodeStructureTree : Buffer -> StructureTreeContract
decodeStructureTree = Native.Ship.decodeStructureTree
decodeEndMarker : Buffer -> EndMarkerContract
decodeEndMarker = Native.Ship.decodeEndMarker
decodeStructureData : Buffer -> StructureDataContract
decodeStructureData = Native.Ship.decodeStructureData
decodeStructure : Buffer -> StructureContract
decodeStructure = Native.Ship.decodeStructure
decodePhysicsState : Buffer -> PhysicsStateContract
decodePhysicsState = Native.Ship.decodePhysicsState
decodeShip : Buffer -> ShipContract
decodeShip = Native.Ship.decodeShip
encodeVessel : VesselContract -> Buffer
encodeVessel = Native.Ship.encodeVessel
encodeFuelTank : FuelTankContract -> Buffer
encodeFuelTank = Native.Ship.encodeFuelTank
encodeEngine : EngineContract -> Buffer
encodeEngine = Native.Ship.encodeEngine
encodePart : PartContract -> Buffer
encodePart = Native.Ship.encodePart
encodeBeam : BeamContract -> Buffer
encodeBeam = Native.Ship.encodeBeam
encodeRoot : RootContract -> Buffer
encodeRoot = Native.Ship.encodeRoot
encodeAttach : AttachContract -> Buffer
encodeAttach = Native.Ship.encodeAttach
encodeStructureNode : StructureNodeContract -> Buffer
encodeStructureNode = Native.Ship.encodeStructureNode
encodeStructureLink : StructureLinkContract -> Buffer
encodeStructureLink = Native.Ship.encodeStructureLink
encodeStructureTree : StructureTreeContract -> Buffer
encodeStructureTree = Native.Ship.encodeStructureTree
encodeEndMarker : EndMarkerContract -> Buffer
encodeEndMarker = Native.Ship.encodeEndMarker
encodeStructureData : StructureDataContract -> Buffer
encodeStructureData = Native.Ship.encodeStructureData
encodeStructure : StructureContract -> Buffer
encodeStructure = Native.Ship.encodeStructure
encodePhysicsState : PhysicsStateContract -> Buffer
encodePhysicsState = Native.Ship.encodePhysicsState
encodeShip : ShipContract -> Buffer
encodeShip = Native.Ship.encodeShip
