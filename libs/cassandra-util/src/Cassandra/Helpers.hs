module Cassandra.Helpers (toOptionFieldName) where

import Data.Aeson.TH
import Imports

-- | Convenient helper to convert record field names to use as YAML fields.
-- NOTE: We typically use this for options in the configuration files!
-- If you are looking into converting record field name to JSON to be used
-- over the API, look for toJSONFieldName in the Data.Json.Util module.
-- It converts field names into snake_case
--
-- Example:
-- newtype TeamName = TeamName { teamName :: Text }
-- deriveJSON toJSONFieldName ''teamName
--
-- would generate {To/From}JSON instances where
-- the field name is "teamName"
toOptionFieldName :: Options
toOptionFieldName = defaultOptions {fieldLabelModifier = lowerFirst . dropPrefix}
  where
    lowerFirst :: String -> String
    lowerFirst (x : xs) = toLower x : xs
    lowerFirst [] = ""
    dropPrefix :: String -> String
    dropPrefix = dropWhile ('_' ==)
