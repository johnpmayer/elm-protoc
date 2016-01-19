module Actions 
  ( UnitContract
  , ActiveContract
  , ControlsContract
  , BuildContract
  , ActionContract
  , encodeUnit
  , decodeUnit
  , marshalUnit
  , unmarshalUnit
  , encodeActive
  , decodeActive
  , marshalActive
  , unmarshalActive
  , encodeControls
  , decodeControls
  , marshalControls
  , unmarshalControls
  , encodeBuild
  , decodeBuild
  , marshalBuild
  , unmarshalBuild
  , encodeAction
  , decodeAction
  , marshalAction
  , unmarshalAction ) where

import Native.Actions

-- Opaque Type definitions
type UnitContract = Opaque_UnitContract
type ActiveContract = Opaque_ActiveContract
type ControlsContract = Opaque_ControlsContract
type BuildContract = Opaque_BuildContract
type ActionContract = Opaque_ActionContract

encodeUnit : UnitContract -> Buffer
encodeUnit = Native.Actions.encodeUnit
decodeUnit : Buffer -> UnitContract
decodeUnit = Native.Actions.decodeUnit
marshalUnit : Unit -> UnitContract
marshalUnit = Native.Actions.marshalUnit
unmarshalUnit : UnitContract -> Unit
unmarshalUnit = Native.Actions.unmarshalUnit
encodeActive : ActiveContract -> Buffer
encodeActive = Native.Actions.encodeActive
decodeActive : Buffer -> ActiveContract
decodeActive = Native.Actions.decodeActive
marshalActive : Active -> ActiveContract
marshalActive = Native.Actions.marshalActive
unmarshalActive : ActiveContract -> Active
unmarshalActive = Native.Actions.unmarshalActive
encodeControls : ControlsContract -> Buffer
encodeControls = Native.Actions.encodeControls
decodeControls : Buffer -> ControlsContract
decodeControls = Native.Actions.decodeControls
marshalControls : Controls -> ControlsContract
marshalControls = Native.Actions.marshalControls
unmarshalControls : ControlsContract -> Controls
unmarshalControls = Native.Actions.unmarshalControls
encodeBuild : BuildContract -> Buffer
encodeBuild = Native.Actions.encodeBuild
decodeBuild : Buffer -> BuildContract
decodeBuild = Native.Actions.decodeBuild
marshalBuild : Build -> BuildContract
marshalBuild = Native.Actions.marshalBuild
unmarshalBuild : BuildContract -> Build
unmarshalBuild = Native.Actions.unmarshalBuild
encodeAction : ActionContract -> Buffer
encodeAction = Native.Actions.encodeAction
decodeAction : Buffer -> ActionContract
decodeAction = Native.Actions.decodeAction
marshalAction : Action -> ActionContract
marshalAction = Native.Actions.marshalAction
unmarshalAction : ActionContract -> Action
unmarshalAction = Native.Actions.unmarshalAction
