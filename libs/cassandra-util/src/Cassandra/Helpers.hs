-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
