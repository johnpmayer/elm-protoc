
module Utils where

import qualified  Data.Char                                         as C
import            Data.Text                                         (Text)
import qualified  Data.Text                                         as T

toTitlePreserving :: Text -> Text
toTitlePreserving =
  let
    walk (capitalize, rem) char =
      let
        capitalizeNext = not $ C.isAlpha char
        newRem = T.snoc rem (if capitalize then C.toUpper char else char)
      in (capitalizeNext, newRem)
  in snd . T.foldl walk (True, T.empty)
