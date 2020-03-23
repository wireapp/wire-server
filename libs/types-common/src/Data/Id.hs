{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- for UUID instances

module Data.Id where

import Cassandra hiding (S)
import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Aeson.Types (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.ByteString.Builder (byteString)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as L
import qualified Data.Char as Char
import Data.Default (Default (..))
import Data.Hashable (Hashable)
import Data.ProtocolBuffers.Internal
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4
import Imports
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

data Mapped a

data Opaque a

type AssetId = Id A

type InvitationId = Id I

-- | A local conversation ID
type ConvId = Id C

-- | A UUID local to this backend, for which we know a mapping to a
-- remote qualified conversation ID exists.
-- These IDs should never leak to other backends or their clients.
type MappedConvId = Id (Mapped C)

-- | A UUID local to this backend, which can either be a local or a mapped conversation ID.
-- Which one it is can be found out by checking whether there exists a corresponding
-- local conversation or mapping in the database.
-- This is how clients refer to conversations, they don't need to know about the mapping.
type OpaqueConvId = Id (Opaque C)

-- | A local user ID
type UserId = Id U

-- | A UUID local to this backend, for which we know a mapping to a
-- remote qualified user ID exists.
-- These IDs should never leak to other backends or their clients.
type MappedUserId = Id (Mapped U)

-- | A UUID local to this backend, which can either be a local or a mapped user ID.
-- Which one it is can be found out by checking whether there exists a corresponding
-- local user or mapping in the database.
-- This is how clients refer to users, they don't need to know about the mapping.
type OpaqueUserId = Id (Opaque U)

makeIdOpaque :: Id a -> Id (Opaque a)
makeIdOpaque (Id userId) = Id userId

makeMappedIdOpaque :: Id (Mapped a) -> Id (Opaque a)
makeMappedIdOpaque (Id userId) = Id userId

type ProviderId = Id P

type ServiceId = Id S

type TeamId = Id T

type ScimTokenId = Id STo

-- Id -------------------------------------------------------------------------

data NoId = NoId deriving (Eq, Show, Generic)

instance NFData NoId where rnf a = seq a ()

newtype Id a
  = Id
      { toUUID :: UUID
      }
  deriving (Eq, Ord, NFData, Hashable, Generic)

-- REFACTOR: non-derived, custom show instances break pretty-show and violate the law
-- that @show . read == id@.  can we derive Show here?
instance Show (Id a) where
  show = UUID.toString . toUUID

instance Read (Id a) where
  readsPrec n = map (\(a, x) -> (Id a, x)) . readsPrec n

instance FromByteString (Id a) where
  parser = do
    x <-
      -- we only want the matching ByteString
      matching $ do
        () <$ Atto.count 8 hexDigit <* Atto.char '-'
        () <$ Atto.count 4 hexDigit <* Atto.char '-'
        () <$ Atto.count 4 hexDigit <* Atto.char '-'
        () <$ Atto.count 4 hexDigit <* Atto.char '-'
        () <$ Atto.count 12 hexDigit
    case UUID.fromASCIIBytes x of
      Nothing -> fail "Invalid UUID"
      Just ui -> return (Id ui)
    where
      matching = fmap fst . Atto.match
      hexDigit = Atto.satisfy Char.isHexDigit

instance ToByteString (Id a) where
  builder = byteString . UUID.toASCIIBytes . toUUID

randomId :: (Functor m, MonadIO m) => m (Id a)
randomId = Id <$> liftIO nextRandom

instance ToJSON (Id a) where
  toJSON (Id uuid) = toJSON $ UUID.toText uuid

instance FromJSON (Id a) where
  parseJSON = withText "Id a" idFromText

instance ToJSONKey (Id a) where
  toJSONKey = ToJSONKeyText idToText (text . idToText)

instance FromJSONKey (Id a) where
  fromJSONKey = FromJSONKeyTextParser idFromText

idFromText :: Text -> Parser (Id a)
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
newtype ConnId
  = ConnId
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
  toJSON (ConnId c) = String (decodeUtf8 c)

instance FromJSON ConnId where
  parseJSON x = ConnId . encodeUtf8 <$> withText "ConnId" pure x

-- ClientId --------------------------------------------------------------------

-- | Handle for a device.  Corresponds to the device fingerprints exposed in the UI.  It is unique
-- only together with a 'UserId', stored in C*, and used as a handle for end-to-end encryption.  It
-- lives as long as the device is registered.  See also: 'ConnId'.
newtype ClientId
  = ClientId
      { client :: Text
      }
  deriving (Eq, Ord, Show, ToByteString, Hashable, NFData, ToJSON, ToJSONKey, Generic)

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

instance FromJSON ClientId where
  parseJSON = withText "ClientId" $ either fail pure . clientIdFromByteString

instance FromJSONKey ClientId where
  fromJSONKey = FromJSONKeyTextParser $ either fail pure . clientIdFromByteString

deriving instance Cql ClientId

instance Arbitrary ClientId where
  arbitrary = newClientId <$> arbitrary

instance EncodeWire ClientId where
  encodeWire t = encodeWire t . client

instance DecodeWire ClientId where
  decodeWire (DelimitedField _ x) = either fail return (runParser parser x)
  decodeWire _ = fail "Invalid ClientId"

-- BotId -----------------------------------------------------------------------

newtype BotId
  = BotId
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

newtype RequestId
  = RequestId
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

-- | Returns "N/A"
instance Default RequestId where
  def = RequestId "N/A"

instance ToJSON RequestId where
  toJSON (RequestId r) = String (decodeUtf8 r)

instance FromJSON RequestId where
  parseJSON = withText "RequestId" (pure . RequestId . encodeUtf8)

instance EncodeWire RequestId where
  encodeWire t = encodeWire t . unRequestId

instance DecodeWire RequestId where
  decodeWire = fmap RequestId . decodeWire

-- Rendering Id values in JSON objects -----------------------------------------

newtype IdObject a = IdObject {fromIdObject :: a}
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (IdObject a) where
  parseJSON = withObject "Id" $ \o -> IdObject <$> (o .: "id")

instance ToJSON a => ToJSON (IdObject a) where
  toJSON (IdObject a) = object ["id" .= a]
