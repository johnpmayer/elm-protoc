
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
import ExampleProto.World exposing (..)

exampleSnapshot = { ships = [] }

{-| Run the program -}
main : Element
main = 
  let 
    marshaledSnapshot = marshalSnapshot exampleSnapshot
    wireSnapshot = encodeSnapshot marshaledSnapshot
    decodedSnapshot = decodeSnapshot wireSnapshot
    finalExampleSnapshot = unmarshalSnapshot decodedSnapshot
  in show finalExampleSnapshot
