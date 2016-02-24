
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

exampleSnapshot = { ships = [] }

{-| Run the program -}
main : Element
main = 
  let 
    marshaledSnapshot = World.marshalSnapshot exampleSnapshot
    wireSnapshot = World.encodeSnapshot marshaledSnapshot
    decodedSnapshot = World.decodeSnapshot wireSnapshot
    finalExampleSnapshot = World.unmarshalSnapshot decodedSnapshot
  in show finalExampleSnapshot
