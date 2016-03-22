
module Main (main) where

{-|

Examples 

@docs main

-}

import Graphics.Element exposing (Element, show)

-- import Addressbook
-- Does not support NESTED TYPES

import ExampleProto.Actions
import ExampleProto.Ship
import ExampleProto.World

exampleVessel = 
  { width = 10
  , length = 20
  }

{-| Run the program -}
main : Element
main = 
  let 
    marshaledVessel = ExampleProto.Ship.marshalVessel exampleVessel
    wireVessel = ExampleProto.Ship.encodeVessel marshaledVessel
    decodedVessel = ExampleProto.Ship.decodeVessel wireVessel
    finalExampleVessel = ExampleProto.Ship.unmarshalVessel decodedVessel
  in show finalExampleVessel
