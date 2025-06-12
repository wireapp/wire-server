-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.MLS.Group where

import Data.Aeson qualified as A
import Data.Default
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Servant
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

newtype GroupId = GroupId {unGroupId :: ByteString}
  deriving (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform GroupId)
  deriving (FromHttpApiData, ToHttpApiData, S.ToParamSchema) via Base64ByteString
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema GroupId)

instance IsString GroupId where
  fromString = GroupId . fromString

instance ParseMLS GroupId where
  parseMLS = GroupId <$> parseMLSBytes @VarInt

instance SerialiseMLS GroupId where
  serialiseMLS (GroupId gid) = serialiseMLSBytes @VarInt gid

instance ToSchema GroupId where
  schema =
    GroupId
      <$> unGroupId
        .= named "GroupId" (Base64ByteString .= fmap fromBase64ByteString (unnamed schema))

newtype GroupIdGen = GroupIdGen {unGroupIdGen :: Word32}
  deriving (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform GroupIdGen)

instance Default GroupIdGen where
  def = GroupIdGen 0
