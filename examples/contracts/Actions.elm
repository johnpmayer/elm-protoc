module Actions 
  ( UnitContract
  , ActiveContract
  , ControlsContract
  , BuildContract
  , ActionContract
  , encodeUnit
  , decodeUnit
  , encodeActive
  , decodeActive
  , encodeControls
  , decodeControls
  , encodeBuild
  , decodeBuild
  , encodeAction
  , decodeAction ) where

import Native.Actions

-- Opaque Type definitions
type UnitContract = Opaque_UnitContract
type ActiveContract = Opaque_ActiveContract
type ControlsContract = Opaque_ControlsContract
type BuildContract = Opaque_BuildContract
type ActionContract = Opaque_ActionContract

decodeUnit : Buffer -> UnitContract
decodeUnit = Native.Actions.decodeUnit
decodeActive : Buffer -> ActiveContract
decodeActive = Native.Actions.decodeActive
decodeControls : Buffer -> ControlsContract
decodeControls = Native.Actions.decodeControls
decodeBuild : Buffer -> BuildContract
decodeBuild = Native.Actions.decodeBuild
decodeAction : Buffer -> ActionContract
decodeAction = Native.Actions.decodeAction
encodeUnit : UnitContract -> Buffer
encodeUnit = Native.Actions.encodeUnit
encodeActive : ActiveContract -> Buffer
encodeActive = Native.Actions.encodeActive
encodeControls : ControlsContract -> Buffer
encodeControls = Native.Actions.encodeControls
encodeBuild : BuildContract -> Buffer
encodeBuild = Native.Actions.encodeBuild
encodeAction : ActionContract -> Buffer
encodeAction = Native.Actions.encodeAction
