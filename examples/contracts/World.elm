module World 
  ( Snapshot
  , GameUpdate
  , SnapshotContract
  , GameUpdateContract
  , encodeSnapshot
  , decodeSnapshot
  , marshalSnapshot
  , unmarshalSnapshot
  , encodeGameUpdate
  , decodeGameUpdate
  , marshalGameUpdate
  , unmarshalGameUpdate ) where

import Native.World


type alias Snapshot = 
  { ships : List (Ship.Ship) }
type GameUpdate_oneof_update =
  = focusEntityId : Int
  | snapshot : Maybe (Snapshot)
type alias GameUpdate = 
  { update : GameUpdate_oneof_update }

-- Opaque Type definitions
type SnapshotContract = Opaque_SnapshotContract
type GameUpdateContract = Opaque_GameUpdateContract

encodeSnapshot : SnapshotContract -> Buffer
encodeSnapshot = Native.World.encodeSnapshot
decodeSnapshot : Buffer -> SnapshotContract
decodeSnapshot = Native.World.decodeSnapshot
marshalSnapshot : Snapshot -> SnapshotContract
marshalSnapshot = Native.World.marshalSnapshot
unmarshalSnapshot : SnapshotContract -> Snapshot
unmarshalSnapshot = Native.World.unmarshalSnapshot
encodeGameUpdate : GameUpdateContract -> Buffer
encodeGameUpdate = Native.World.encodeGameUpdate
decodeGameUpdate : Buffer -> GameUpdateContract
decodeGameUpdate = Native.World.decodeGameUpdate
marshalGameUpdate : GameUpdate -> GameUpdateContract
marshalGameUpdate = Native.World.marshalGameUpdate
unmarshalGameUpdate : GameUpdateContract -> GameUpdate
unmarshalGameUpdate = Native.World.unmarshalGameUpdate
