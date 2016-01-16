{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Templates where

import NeatInterpolation

nativeModule :: String -> String
-- TODO - should really support a list 
--nativeModule [] = error "Empty fully qualified module path"
nativeModule modulename =
  [string|
    Elm.Native.$modulename = Elm.Native.$modulename || {}
    Elm.Native.$modulename = function(_elm) {
      "use strict";
      _elm.Native.$modulename = _elm.Native.$modulename || {}
      if (_elm.Native.$modulename.values) {
        return _elm.Native.$modulename.values
      }
  ]
  