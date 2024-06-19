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

module Wire.API.ServantProto where

import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.OpenApi
import Imports
import Network.HTTP.Media ((//))
import Servant

-- | Type to tell servant that it should unrender request body or render
-- response body with Protobuf
data Proto

-- | We do not use 'Data.ProtocolBuffers.Decode' so we get a little freedom in
-- defining separate data types which match one to one with the protobuf and the
-- data types which we actually use in business logic. Eventually we should
-- think of better ways of doing this, perhaps using mu-schema or proto-lens as
-- it is fairly difficult to keep our custom data type, e.g. in
-- Wire.API.Message.Proto in sync with the proto files.
class FromProto a where
  fromProto :: ByteString -> Either String a

class ToProto a where
  toProto :: a -> ByteString

instance Accept Proto where
  contentTypes _ = ("application" // "x-protobuf") :| []

instance (FromProto a) => MimeUnrender Proto a where
  mimeUnrender _ bs = fromProto (LBS.toStrict bs)

-- | This wrapper can be used to get the raw protobuf representation of a type.
-- It is used when the protobuf is supposed to be forwarded somewhere like a
-- federated remote, this saves us from having to re-encode it.
data RawProto a = RawProto
  { rpRaw :: ByteString,
    rpValue :: a
  }

instance (FromProto a) => FromProto (RawProto a) where
  fromProto x = fmap (RawProto x) (fromProto x)

instance (ToSchema a) => ToSchema (RawProto a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)
