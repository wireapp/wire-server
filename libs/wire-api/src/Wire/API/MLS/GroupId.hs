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
  ( GroupId,
    ParseErr,
    mkGroupId,
    serialise,
    testGId,
  )
where

import Control.Error.Util
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Domain
import Data.Either.Combinators
import Data.Id
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text.Encoding as T
import Data.UUID
import qualified Data.UUID as UUID (fromString)
import Imports hiding (drop, length, take)
import Wire.API.Arbitrary

-- | An MLS group ID
newtype GroupId = GroupId {unGroupId :: Qualified ConvId}
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GroupId)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema GroupId)

instance ToSchema GroupId where
  -- SchemaP NamedSwaggerDoc Value Value GroupId GroupId
  -- parsedText "Group ID" :: (Text -> Either String GroupId) -> SchemaP NamedSwaggerDoc Value Value Text GroupId
  -- schema = _ -- parsedText "Group ID" (mapLeft show . mkGroupId)
  schema =
    mkSchema
      undefined
      (A.withText "Group ID" f)
      (Just . A.String . T.decodeUtf8 . serialise)
    where
      f :: Text -> A.Parser GroupId
      f = either (fail . show) pure . mkGroupId . T.encodeUtf8

newtype ParseErr = ParseErr String
  deriving (Eq, Show)

-- | Parse a ''GroupId' from a byte sequence in the network order.
mkGroupId :: ByteString -> Either ParseErr GroupId
mkGroupId ((<= 256) . length -> False) = Left . ParseErr $ "GroupId: it has to be up to 256 bytes in size"
mkGroupId bs = do
  convId <-
    fmap Id
      . note (ParseErr "Not a UUID")
      . fromByteString
      . fromStrict
      $ take l bs
  domain <-
    mapLeft ParseErr $
      mkMLSDomain
        =<< ( mapLeft show
                . T.decodeUtf8'
                $ drop l bs
            )
  pure . GroupId $ Qualified convId domain
  where
    l = 16

-- | Serialise a 'GroupId' into a byte sequence in the network order.
serialise :: GroupId -> ByteString
serialise (unGroupId -> Qualified (Id cnvId) (Domain domain)) =
  toStrict (toByteString cnvId) <> T.encodeUtf8 domain

testGId :: GroupId
testGId =
  GroupId $
    Qualified
      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
      (Domain "to-be-parsed.com")
