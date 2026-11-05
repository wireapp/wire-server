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

import Cassandra qualified as C
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.Default
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Servant
import Wire.API.MLS.Credential (ClientIdentity)
import Wire.API.MLS.Serialisation
import Wire.API.PostgresMarshall
import Wire.Arbitrary

newtype GroupId = GroupId {unGroupId :: ByteString}
  deriving (Eq, Show, Generic, Ord)
  deriving newtype (PostgresUnmarshall ByteString, PostgresMarshall ByteString)
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

instance C.Cql GroupId where
  ctype = C.Tagged C.BlobColumn

  toCql = C.CqlBlob . LBS.fromStrict . unGroupId

  fromCql (C.CqlBlob b) = Right . GroupId . LBS.toStrict $ b
  fromCql _ = Left "group_id: blob expected"

newtype GroupIdGen = GroupIdGen {unGroupIdGen :: Word32}
  deriving (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform GroupIdGen)

instance Default GroupIdGen where
  def = GroupIdGen 0

-- | One @mls_group_member_client@ row of an MLS group as served by galley's
-- internal API.  Rows for clients with a pending removal are excluded (they
-- count as removed, see 'mkClientMap' in Wire.ConversationStore.MLS.Types).
data MLSMemberClient = MLSMemberClient
  { mmcClient :: ClientIdentity,
    mmcLeafIndex :: Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema MLSMemberClient)

instance ToSchema MLSMemberClient where
  schema =
    object $
      MLSMemberClient
        <$> mmcClient .= field "client" schema
        <*> mmcLeafIndex .= field "leaf_index" schema

newtype MLSMemberClientList = MLSMemberClientList {mmcsMemberClients :: [MLSMemberClient]}
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema MLSMemberClientList)

instance ToSchema MLSMemberClientList where
  schema =
    object $
      MLSMemberClientList
        <$> mmcsMemberClients .= field "member_clients" (array schema)
