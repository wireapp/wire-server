{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- for UUID instances

module Data.Id where

import Cassandra hiding (S)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import qualified Data.Aeson.Types as A
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.ByteString.Builder (byteString)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as L
import qualified Data.Char as Char
import Data.Default (Default (..))
import Data.Hashable (Hashable)
import Data.ProtocolBuffers.Internal
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import Data.Swagger.Internal.ParamSchema (ToParamSchema (..))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Test.QuickCheck
import Test.QuickCheck.Instances ()

data A

data C

data I

data U

data P

data S

data T

data STo

type AssetId = Id A

type InvitationId = Id I

-- | A local conversation ID
type ConvId = Id C

-- | A local user ID
type UserId = Id U

type ProviderId = Id P

type ServiceId = Id S

type TeamId = Id T

type ScimTokenId = Id STo

-- Id -------------------------------------------------------------------------

data NoId = NoId deriving (Eq, Show, Generic)

instance NFData NoId where rnf a = seq a ()

newtype Id a = Id
  { toUUID :: UUID
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, NFData, ToParamSchema)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema (Id a)

instance ToSchema (Id a) where
  schema = Id <$> toUUID .= uuid
    where
      uuid :: ValueSchema NamedSwaggerDoc UUID
      uuid =
        mkSchema
          (swaggerDoc @UUID)
          ( A.withText
              "UUID"
              ( maybe (fail "Invalid UUID") pure
                  . UUID.fromText
              )
          )
          (pure . A.toJSON . UUID.toText)

-- REFACTOR: non-derived, custom show instances break pretty-show and violate the law
-- that @show . read == id@.  can we derive Show here?
instance Show (Id a) where
  show = UUID.toString . toUUID

instance Read (Id a) where
  readsPrec n = map (\(a, x) -> (Id a, x)) . readsPrec n

instance FromByteString (Id a) where
  parser = do
    match <-
      -- we only want the matching part of the ByteString, so the parser doesn't
      -- consume additional input.
      -- This allows the parser to be composed.
      matching $ do
        void $ Atto.count 8 hexDigit <* Atto.char '-'
        void $ Atto.count 4 hexDigit <* Atto.char '-'
        void $ Atto.count 4 hexDigit <* Atto.char '-'
        void $ Atto.count 4 hexDigit <* Atto.char '-'
        void $ Atto.count 12 hexDigit
    case UUID.fromASCIIBytes match of
      Nothing -> fail "Invalid UUID"
      Just ui -> return (Id ui)
    where
      matching = fmap fst . Atto.match
      hexDigit = Atto.satisfy Char.isHexDigit <?> "hexadecimal digit"

instance ToByteString (Id a) where
  builder = byteString . UUID.toASCIIBytes . toUUID

instance FromHttpApiData (Id a) where
  parseUrlPiece = fmap Id . parseUrlPiece

instance ToHttpApiData (Id a) where
  toUrlPiece = toUrlPiece . show

instance A.ToJSONKey (Id a) where
  toJSONKey = A.ToJSONKeyText idToText (A.text . idToText)

instance A.FromJSONKey (Id a) where
  fromJSONKey = A.FromJSONKeyTextParser idFromText

randomId :: (Functor m, MonadIO m) => m (Id a)
randomId = Id <$> liftIO nextRandom

idFromText :: Text -> A.Parser (Id a)
idFromText = maybe (fail "UUID.fromText failed") (pure . Id) . UUID.fromText

idToText :: Id a -> Text
idToText = UUID.toText . toUUID

instance Cql (Id a) where
  ctype = retag (ctype :: Tagged UUID ColumnType)
  toCql = toCql . toUUID
  fromCql c = Id <$> fromCql c

instance EncodeWire (Id a) where
  encodeWire t = encodeWire t . toUUID

instance DecodeWire (Id a) where
  decodeWire = fmap Id . decodeWire

instance EncodeWire UUID where
  encodeWire t = encodeWire t . L.toStrict . UUID.toByteString

instance DecodeWire UUID where
  decodeWire (DelimitedField _ bs) =
    maybe (fail "Invalid UUID") pure . UUID.fromByteString . L.fromStrict $ bs
  decodeWire _ = fail "Invalid UUID"

instance Arbitrary (Id a) where
  arbitrary = Id <$> arbitrary

-- ConnId ----------------------------------------------------------------------

-- | Handle for a device.  Derived from the access token (see 'Data.ZAuth.Token.Access').  Unique
-- only together with a 'UserId'.  Historically, it is older than 'ClientId' and precedes end-to-end
-- encryption, but there are still situations in which 'ClientId' is not applicable (See also:
-- 'Presence').  Used by Cannon and Gundeck to identify a websocket connection, but also in other
-- places.
newtype ConnId = ConnId
  { fromConnId :: ByteString
  }
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      FromByteString,
      ToByteString,
      Hashable,
      NFData,
      Generic
    )

instance ToJSON ConnId where
  toJSON (ConnId c) = A.String (decodeUtf8 c)

instance FromJSON ConnId where
  parseJSON x = ConnId . encodeUtf8 <$> A.withText "ConnId" pure x

instance FromHttpApiData ConnId where
  parseUrlPiece = Right . ConnId . encodeUtf8

instance ToHttpApiData ConnId where
  toUrlPiece = decodeUtf8 . fromConnId

-- ClientId --------------------------------------------------------------------

-- | Handle for a device.  Corresponds to the device fingerprints exposed in the UI.  It is unique
-- only together with a 'UserId', stored in C*, and used as a handle for end-to-end encryption.  It
-- lives as long as the device is registered.  See also: 'ConnId'.
newtype ClientId = ClientId
  { client :: Text
  }
  deriving (Eq, Ord, Show, ToByteString, Hashable, NFData, A.ToJSONKey, Generic)
  deriving newtype (ToParamSchema, FromHttpApiData, ToHttpApiData)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ClientId

instance ToSchema ClientId where
  schema = client .= parsedText "ClientId" clientIdFromByteString

newClientId :: Word64 -> ClientId
newClientId = ClientId . toStrict . toLazyText . hexadecimal

clientIdFromByteString :: Text -> Either String ClientId
clientIdFromByteString txt =
  if T.length txt <= 20 && T.all isHexDigit txt
    then Right $ ClientId txt
    else Left "Invalid ClientId"

instance FromByteString ClientId where
  parser = do
    bs <- Atto.takeByteString
    either fail pure $ clientIdFromByteString (cs bs)

instance A.FromJSONKey ClientId where
  fromJSONKey = A.FromJSONKeyTextParser $ either fail pure . clientIdFromByteString

deriving instance Cql ClientId

instance Arbitrary ClientId where
  arbitrary = newClientId <$> arbitrary

instance EncodeWire ClientId where
  encodeWire t = encodeWire t . client

instance DecodeWire ClientId where
  decodeWire (DelimitedField _ x) = either fail return (runParser parser x)
  decodeWire _ = fail "Invalid ClientId"

-- BotId -----------------------------------------------------------------------

newtype BotId = BotId
  {botUserId :: UserId}
  deriving
    ( Eq,
      Ord,
      FromByteString,
      ToByteString,
      Hashable,
      NFData,
      FromJSON,
      ToJSON,
      Generic
    )

instance Show BotId where
  show = show . botUserId

instance Read BotId where
  readsPrec n = map (\(a, x) -> (BotId a, x)) . readsPrec n

deriving instance Cql BotId

instance Arbitrary BotId where
  arbitrary = BotId <$> arbitrary

-- RequestId -------------------------------------------------------------------

newtype RequestId = RequestId
  { unRequestId :: ByteString
  }
  deriving
    ( Eq,
      Show,
      Read,
      FromByteString,
      ToByteString,
      Hashable,
      NFData,
      Generic
    )

instance ToSchema RequestId where
  schema =
    RequestId . encodeUtf8
      <$> (decodeUtf8 . unRequestId) .= text "RequestId"

-- | Returns "N/A"
instance Default RequestId where
  def = RequestId "N/A"

instance ToJSON RequestId where
  toJSON (RequestId r) = A.String (decodeUtf8 r)

instance FromJSON RequestId where
  parseJSON = A.withText "RequestId" (pure . RequestId . encodeUtf8)

instance EncodeWire RequestId where
  encodeWire t = encodeWire t . unRequestId

instance DecodeWire RequestId where
  decodeWire = fmap RequestId . decodeWire

-- Rendering Id values in JSON objects -----------------------------------------

newtype IdObject a = IdObject {fromIdObject :: a}
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema (IdObject a)

instance ToSchema a => ToSchema (IdObject a) where
  schema =
    object "Id" $
      IdObject
        <$> fromIdObject .= field "id" schema
