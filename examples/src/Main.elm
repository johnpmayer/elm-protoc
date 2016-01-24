
module Main where

import Graphics.Element exposing (show)

-- import Addressbook
-- Does not support NESTED TYPES

import Actions
import Ship
import World

exampleSnapshot = { ships = [] }

main = 
  let 
    marshaledSnapshot = World.marshalSnapshot exampleSnapshot
    wireSnapshot = World.encodeSnapshot marshaledSnapshot
    decodedSnapshot = World.decodeSnapshot wireSnapshot
    finalExampleSnapshot = World.unmarshalSnapshot decodedSnapshot
  in show finalExampleSnapshot
