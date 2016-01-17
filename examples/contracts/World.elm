module World 
  ( SnapshotContract
  , GameUpdateContract
  , encodeSnapshot
  , decodeSnapshot
  , encodeGameUpdate
  , decodeGameUpdate ) where

import Native.World

-- Opaque Type definitions
type SnapshotContract = Opaque_SnapshotContract
type GameUpdateContract = Opaque_GameUpdateContract

decodeSnapshot : Buffer -> SnapshotContract
decodeSnapshot = Native.World.decodeSnapshot
decodeGameUpdate : Buffer -> GameUpdateContract
decodeGameUpdate = Native.World.decodeGameUpdate
encodeSnapshot : SnapshotContract -> Buffer
encodeSnapshot = Native.World.encodeSnapshot
encodeGameUpdate : GameUpdateContract -> Buffer
encodeGameUpdate = Native.World.encodeGameUpdate
