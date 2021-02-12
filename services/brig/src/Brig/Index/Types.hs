-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Index.Types
  ( CreateIndexSettings (..),
    SafeLegacyAuthId (..),
  )
where

import Cassandra.CQL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.String.Conversions (cs)
import qualified Database.Bloodhound as ES
import Imports
import Wire.API.User (LegacyAuthId)

data CreateIndexSettings = CreateIndexSettings
  { _cisIndexSettings :: [ES.UpdatableIndexSetting],
    _cisShardCount :: Int,
    _cisDeleteTemplate :: Maybe ES.TemplateName
  }
  deriving (Show)

-- | Without this type some UserSSOId value fail to be loaded from the DB
-- namely those with 'tenant', 'subject' values that are invalid XML.
-- While invalid values might be rare, bulk operations such as 'reindexAll' need to handle invalid values.
newtype SafeLegacyAuthId = SafeLegacyAuthId {fromSafeLegacyAuthId :: Either Aeson.Value LegacyAuthId}

instance Cql SafeLegacyAuthId where
  ctype = Tagged TextColumn

  fromCql (CqlText t) = case Aeson.eitherDecode $ cs t of
    Right json ->
      case Aeson.parseMaybe (Aeson.parseJSON @LegacyAuthId) json of
        Nothing -> Right $ SafeLegacyAuthId (Left json)
        Just legacyAuthId -> Right $ SafeLegacyAuthId (Right legacyAuthId)
    Left msg -> Left $ "fromCql: SafeLegacyAuthId: expected a JSON value" ++ msg
  fromCql _ = Left "fromCql: LegacyAuthId (was UserSSOId): CqlText expected"

  toCql =
    -- this is ok, same as in @instance Cql LegacyAuthId@.
    error "LegacyAuthId should never be written. Convert to AuthId first."
