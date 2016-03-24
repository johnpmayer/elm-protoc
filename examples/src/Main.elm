
module Main (main) where

{-|

Examples 

@docs main

-}

import Graphics.Element exposing (Element, down, flow, show)

-- import Addressbook
-- Does not support NESTED TYPES

import ExampleProto.Actions
import ExampleProto.Ship
import ExampleProto.World

exampleVessel = 
  { width = 10
  , length = 20
  }

testVessel : Element
testVessel = 
  let 
    marshaledVessel = ExampleProto.Ship.marshalVessel exampleVessel
    wireVessel = ExampleProto.Ship.encodeVessel marshaledVessel
    decodedVessel = ExampleProto.Ship.decodeVessel wireVessel
    finalExampleVessel = ExampleProto.Ship.unmarshalVessel decodedVessel
  in show finalExampleVessel

examplePart = 
  { part = ExampleProto.Ship.Part_oneof_part_vessel exampleVessel }

testPart : Element
testPart = 
  let
    marshaledPart = ExampleProto.Ship.marshalPart examplePart
    wirePart = ExampleProto.Ship.encodePart marshaledPart
    decodedPart = ExampleProto.Ship.decodePart wirePart
    finalExamplePart = ExampleProto.Ship.unmarshalPart decodedPart
  in show finalExamplePart

{-| Display all the good stuff -}
main : Element
main = flow down [testVessel, testPart]
