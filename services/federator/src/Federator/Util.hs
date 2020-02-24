module Federator.Util
  ( wireJsonOptions,
  )
where

import Data.Aeson as Aeson
import Imports

dropPrefix :: String -> String -> Maybe String
dropPrefix pfx str =
  if length pfx > length str
    then Nothing
    else case splitAt (length pfx) str of
      (pfx', sfx) ->
        if pfx' /= pfx
          then Nothing
          else Just sfx

-- | This is a partial function; totality of all calls must be verified by roundtrip tests on
-- the aeson instances involved.
wireJsonOptions :: String -> Options
wireJsonOptions pfx = defaultOptions {fieldLabelModifier = fromJust . dropPrefix pfx . fmap toLower}
