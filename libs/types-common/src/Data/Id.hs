{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- for UUID instances

module Data.Id
  ( -- * Tagged IDs
    Id (..),
    IdTag,
    KnownIdTag (..),
    idTagName,
    randomId,
    AssetId,
    InvitationId,
    ConvId,
    UserId,
    ProviderId,
    ServiceId,
    TeamId,
    ScimTokenId,
    parseIdFromText,
    idToText,
    idObjectSchema,
    IdObject (..),

    -- * Client IDs
    ClientId (..),
    clientToText,

    -- * Other IDs
    ConnId (..),
    RequestId (..),
    defRequestId,
    BotId (..),
    NoId,
    OAuthClientId,
    OAuthRefreshTokenId,

    -- * Utils
    uuidSchema,
  )
where

import Cassandra hiding (S)
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Encoding qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as A
import Data.Attoparsec.ByteString ((<?>))
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Bifunctor (first)
import Data.Binary
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as L
import Data.Char qualified as Char
import Data.Hashable (Hashable)
import Data.OpenApi qualified as S
import Data.OpenApi.Internal.ParamSchema (ToParamSchema (..))
import Data.ProtocolBuffers.Internal
import Data.Proxy
import Data.Schema
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import System.Logger (ToBytes)
import Test.QuickCheck
import Test.QuickCheck.Instances ()

data IdTag
  = Asset
  | Conversation
  | Invitation
  | User
  | Provider
  | Service
  | Team
  | ScimToken
  | OAuthClient
  | OAuthRefreshToken

idTagName :: IdTag -> Text
idTagName Asset = "Asset"
idTagName Conversation = "Conv"
idTagName Invitation = "Invitation"
idTagName User = "User"
idTagName Provider = "Provider"
idTagName Service = "Service"
idTagName Team = "Team"
idTagName ScimToken = "ScimToken"
idTagName OAuthClient = "OAuthClient"
idTagName OAuthRefreshToken = "OAuthRefreshToken"

class KnownIdTag (t :: IdTag) where
  idTagValue :: IdTag

instance KnownIdTag 'Asset where idTagValue = Asset

instance KnownIdTag 'Conversation where idTagValue = Conversation

instance KnownIdTag 'Invitation where idTagValue = Invitation

instance KnownIdTag 'User where idTagValue = User

instance KnownIdTag 'Provider where idTagValue = Provider

instance KnownIdTag 'Service where idTagValue = Service

instance KnownIdTag 'Team where idTagValue = Team

instance KnownIdTag 'ScimToken where idTagValue = ScimToken

instance KnownIdTag 'OAuthClient where idTagValue = OAuthClient

instance KnownIdTag 'OAuthRefreshToken where idTagValue = OAuthRefreshToken

type AssetId = Id 'Asset

type InvitationId = Id 'Invitation

-- | A local conversation ID
type ConvId = Id 'Conversation

-- | A local user ID
type UserId = Id 'User

type ProviderId = Id 'Provider

type ServiceId = Id 'Service

type TeamId = Id 'Team

type ScimTokenId = Id 'ScimToken

type OAuthClientId = Id 'OAuthClient

type OAuthRefreshTokenId = Id 'OAuthRefreshToken

-- Id -------------------------------------------------------------------------

data NoId = NoId deriving (Eq, Show, Generic)

instance NFData NoId where rnf a = seq a ()

newtype Id a = Id
  { toUUID :: UUID
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, NFData, ToParamSchema, Binary)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema (Id a)

instance ToSchema (Id a) where
  schema = Id <$> toUUID .= uuidSchema

uuidSchema :: ValueSchema NamedSwaggerDoc UUID
uuidSchema =
  mkSchema
    (addExample (swaggerDoc @UUID))
    ( A.withText
        "UUID"
        ( maybe (fail "Invalid UUID") pure
            . UUID.fromText
        )
    )
    (pure . A.toJSON . UUID.toText)
  where
    addExample =
      S.schema . S.example
        ?~ toJSON ("99db9768-04e3-4b5d-9268-831b6a25c4ab" :: Text)

-- REFACTOR: non-derived, custom show instances break pretty-show and violate the law
-- that @show . read == id@.  can we derive Show here?
instance Show (Id a) where
  show = UUID.toString . toUUID

instance Read (Id a) where
  readsPrec n = map (first Id) . readsPrec n

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
      Just ui -> pure (Id ui)
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
  toJSONKey = A.ToJSONKeyText (Key.fromText . idToText) (A.text . idToText)

instance A.FromJSONKey (Id a) where
  fromJSONKey = A.FromJSONKeyTextParser idFromText

randomId :: (MonadIO m) => m (Id a)
randomId = Id <$> liftIO nextRandom

idFromText :: Text -> A.Parser (Id a)
idFromText = either fail pure . parseIdFromText

parseIdFromText :: Text -> Either String (Id a)
parseIdFromText = maybe (Left "UUID.fromText failed") (Right . Id) . UUID.fromText

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
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConnId

instance ToSchema ConnId where
  schema = (decodeUtf8 . fromConnId) .= fmap (ConnId . encodeUtf8) (text "ConnId")

instance S.ToParamSchema ConnId where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance FromHttpApiData ConnId where
  parseUrlPiece = Right . ConnId . encodeUtf8

instance Arbitrary ConnId where
  arbitrary = ConnId . B8.pack <$> resize 10 (listOf arbitraryPrintableChar)

-- ClientId --------------------------------------------------------------------

-- | Handle for a device.  Corresponds to the device fingerprints exposed in the UI.  It is unique
-- only together with a 'UserId', stored in C*, and used as a handle for end-to-end encryption.  It
-- lives as long as the device is registered.  See also: 'ConnId'.
newtype ClientId = ClientId
  { clientToWord64 :: Word64
  }
  deriving (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ClientId

instance ToParamSchema ClientId where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance FromHttpApiData ClientId where
  parseUrlPiece = first T.pack . runParser parser . encodeUtf8

instance ToHttpApiData ClientId where
  toUrlPiece = clientToText

clientToText :: ClientId -> Text
clientToText = toStrict . toLazyText . hexadecimal . clientToWord64

instance ToSchema ClientId where
  schema = withParser s parseClientId
    where
      s :: ValueSchemaP NamedSwaggerDoc ClientId Text
      s =
        clientToText .= schema
          & doc . S.description
            ?~ "A 64-bit unsigned integer, represented as a hexadecimal numeral. \
               \Any valid hexadecimal numeral is accepted, but the backend will only \
               \produce representations with lowercase digits and no leading zeros"

parseClientId :: Text -> A.Parser ClientId
parseClientId = either fail pure . runParser parser . encodeUtf8

instance FromByteString ClientId where
  parser = do
    num :: Integer <- Atto.hexadecimal
    guard $ num <= fromIntegral (maxBound :: Word64)
    pure (ClientId (fromIntegral num))

instance ToByteString ClientId where
  builder = Builder.fromByteString . encodeUtf8 . clientToText

instance A.FromJSONKey ClientId where
  fromJSONKey = A.FromJSONKeyTextParser parseClientId

instance A.ToJSONKey ClientId where
  toJSONKey = A.toJSONKeyText clientToText

instance Cql ClientId where
  ctype = Tagged TextColumn
  toCql = CqlText . clientToText
  fromCql (CqlText t) = runParser parser (encodeUtf8 t)
  fromCql _ = Left "ClientId: expected CqlText"

instance Arbitrary ClientId where
  arbitrary = ClientId <$> arbitrary

instance EncodeWire ClientId where
  encodeWire t = encodeWire t . clientToText

instance DecodeWire ClientId where
  decodeWire (DelimitedField _ x) = either fail pure (runParser parser x)
  decodeWire _ = fail "Invalid ClientId"

-- BotId -----------------------------------------------------------------------

newtype BotId = BotId
  {botUserId :: UserId}
  deriving
    ( Eq,
      Ord,
      FromByteString,
      ToByteString,
      FromHttpApiData,
      Hashable,
      NFData,
      Generic,
      ToParamSchema
    )
  deriving newtype (ToSchema)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema BotId

instance Show BotId where
  show = show . botUserId

instance Read BotId where
  readsPrec n = map (first BotId) . readsPrec n

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
      Generic,
      ToBytes
    )

defRequestId :: (IsString s) => s
defRequestId = "N/A"

instance ToSchema RequestId where
  schema =
    RequestId . encodeUtf8
      <$> (decodeUtf8 . unRequestId) .= text "RequestId"

instance ToJSON RequestId where
  toJSON (RequestId r) = A.String (decodeUtf8 r)

instance FromJSON RequestId where
  parseJSON = A.withText "RequestId" (pure . RequestId . encodeUtf8)

instance EncodeWire RequestId where
  encodeWire t = encodeWire t . unRequestId

instance DecodeWire RequestId where
  decodeWire = fmap RequestId . decodeWire

instance FromHttpApiData RequestId where
  parseUrlPiece = Right . RequestId . encodeUtf8

-- Rendering Id values in JSON objects -----------------------------------------

newtype IdObject a = IdObject {fromIdObject :: a}
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema (IdObject a)

instance (ToSchema a) => ToSchema (IdObject a) where
  schema = idObjectSchema (IdObject <$> fromIdObject .= schema)

idObjectSchema :: ValueSchemaP NamedSwaggerDoc a b -> ValueSchemaP NamedSwaggerDoc a b
idObjectSchema sch = object "Id" (field "id" sch)
