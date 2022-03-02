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

module Wire.API.MLS.GroupId
  ( GroupId (..),
    parseConvId,

    -- * Convenience utilities
    convIdToGroupId,
    decodeConvId,
  )
where

import Control.Error.Util
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Either.Combinators
import Data.Id
import Data.Json.Util (Base64ByteString (Base64ByteString, fromBase64ByteString), base64Schema)
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text.Encoding as T
import Data.UUID
import Imports hiding (drop, length, take)
import Wire.API.Arbitrary

-- | An MLS group ID. It is represented as a byte sequence.
newtype GroupId = GroupId {unGroupId :: LBS.ByteString}
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema GroupId)

instance Arbitrary GroupId where
  arbitrary = fmap convIdToGroupId $ Qualified <$> arbitrary <*> mlsDomain

instance ToSchema GroupId where
  schema =
    GroupId
      <$> unGroupId
      .= named "GroupId" (Base64ByteString .= fmap fromBase64ByteString base64Schema)

-- | Parse a 'GroupId' byte sequence in the network order as a qualified
-- conversation ID.
parseConvId :: GroupId -> A.Parser (Qualified ConvId)
parseConvId ((<= 256) . LBS.length . unGroupId -> False) =
  fail "GroupId: it has to be up to 256 bytes in size"
parseConvId (GroupId bs) = do
  convId <-
    either fail (pure . Id)
      . note "Not a UUID"
      . fromByteString
      $ LBS.take l bs
  domain <-
    either fail pure $
      mkMLSDomain
        =<< ( mapLeft show
                . T.decodeUtf8'
                . LBS.toStrict
                $ LBS.drop l bs
            )
  pure $ Qualified convId domain
  where
    l = 16

-- | Currently the mapping from a group ID to a conversation ID is
-- straightforward, but in the future it might be more involved if the
-- representation is to change.
decodeConvId :: GroupId -> Either String (Qualified ConvId)
decodeConvId = A.parseEither parseConvId

-- | Serialise a 'Qualified ConvId' into a byte sequence in the network order
-- that is used as a group ID.
--
-- Currently the mapping from a conversation ID to a group ID is
-- straightforward, but in the future it might be more involved if the
-- representation is to change.
convIdToGroupId :: Qualified ConvId -> GroupId
convIdToGroupId (Qualified (Id cnvId) (Domain domain)) =
  GroupId $
    toByteString cnvId <> LBS.fromStrict (T.encodeUtf8 domain)
