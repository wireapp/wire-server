{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-} -- for UUID instances

module Data.Id where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import Data.ByteString.Conversion
import Data.Char (isHexDigit)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.UUID
import Data.UUID.V4
#ifdef WITH_CQL
import Database.CQL.Protocol hiding (S)
#endif
import Data.Word
import GHC.Generics (Generic)
#ifdef WITH_ARBITRARY
import Test.QuickCheck
#endif
#ifdef WITH_PROTOBUF
import qualified Data.ByteString.Lazy as L
import Data.ProtocolBuffers.Internal
#endif

import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Data.UUID             as UUID

data A
data C
data I
data U
data P
data S
data T

type AssetId      = Id A
type ConvId       = Id C
type InvitationId = Id I
type UserId       = Id U
type ProviderId   = Id P
type ServiceId    = Id S
type TeamId       = Id T

-- Id -------------------------------------------------------------------------

data NoId = NoId deriving (Eq, Show)

instance NFData NoId where rnf a = seq a ()

newtype Id a = Id
    { toUUID :: UUID
    } deriving (Eq, Ord, Generic, NFData)

instance Show (Id a) where
    show = toString . toUUID

instance Read (Id a) where
    readsPrec n = map (\(a, x) -> (Id a, x)) . readsPrec n

instance FromByteString (Id a) where
    parser = do
        x <- takeByteString
        case fromASCIIBytes x of
            Nothing -> fail "Invalid UUID"
            Just ui -> return (Id ui)

instance ToByteString (Id a) where
    builder = byteString . toASCIIBytes . toUUID

instance Hashable (Id a)

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

#ifdef WITH_CQL
instance Cql (Id a) where
    ctype = retag (ctype :: Tagged UUID ColumnType)
    toCql = toCql . toUUID
    fromCql c = Id <$> fromCql c
#endif

#ifdef WITH_PROTOBUF
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
#endif

#ifdef WITH_ARBITRARY
instance Arbitrary (Id a) where
    arbitrary = Id <$> arbitrary

instance Arbitrary UUID where
    arbitrary = choose (nil, nil)
#endif

-- ConnId ----------------------------------------------------------------------

newtype ConnId = ConnId
    { fromConnId :: ByteString
    } deriving ( Eq
               , Generic
               , Ord
               , Read
               , Show
               , FromByteString
               , ToByteString
               , Hashable
               , NFData
               )

instance ToJSON ConnId where
    toJSON (ConnId c) = String (decodeUtf8 c)

instance FromJSON ConnId where
    parseJSON x = ConnId . encodeUtf8 <$> withText "ConnId" pure x

-- ClientId --------------------------------------------------------------------

newtype ClientId = ClientId
    { client :: Text
    } deriving (Eq, Ord, Show, ToByteString, Hashable, NFData, ToJSON)

instance FromByteString ClientId where
    parser = do
        x <- takeByteString
        unless (B.length x <= 20 && B.all isHexDigit x) $
            fail "Invalid client ID"
        either fail (return . ClientId) (runParser parser x)

newClientId :: Word64 -> ClientId
newClientId = ClientId . toStrict . toLazyText . hexadecimal

instance FromJSON ClientId where
    parseJSON = withText "ClientId" $ \x -> do
        unless (T.length x <= 20 && T.all isHexDigit x) $
            fail "Invalid ClientId"
        return (ClientId x)

#ifdef WITH_CQL
deriving instance Cql ClientId
#endif

#ifdef WITH_ARBITRARY
instance Arbitrary ClientId where
    arbitrary = newClientId <$> arbitrary
#endif

#ifdef WITH_PROTOBUF
instance EncodeWire ClientId where
    encodeWire t = encodeWire t . client

instance DecodeWire ClientId where
    decodeWire (DelimitedField _ x) = either fail return (runParser parser x)
    decodeWire _                    = fail "Invalid ClientId"
#endif

-- BotId -----------------------------------------------------------------------

newtype BotId = BotId
    { botUserId :: UserId }
    deriving ( Eq
             , Ord
             , Generic
             , FromByteString
             , ToByteString
             , Hashable
             , NFData
             , FromJSON
             , ToJSON
             )

instance Show BotId where
    show = show . botUserId

instance Read BotId where
    readsPrec n = map (\(a, x) -> (BotId a, x)) . readsPrec n

#ifdef WITH_CQL
deriving instance Cql BotId
#endif

#ifdef WITH_ARBITRARY
instance Arbitrary BotId where
    arbitrary = BotId <$> arbitrary
#endif

-- RequestId -------------------------------------------------------------------

newtype RequestId = RequestId
    { unRequestId :: ByteString
    } deriving ( Eq
               , Show
               , Read
               , FromByteString
               , ToByteString
               , Hashable
               , NFData
               )

instance Monoid RequestId where
    mempty = RequestId "N/A"
    mappend a b
        | a == mempty = b
        | otherwise   = a

instance ToJSON RequestId where
    toJSON (RequestId r) = String (decodeUtf8 r)

instance FromJSON RequestId where
    parseJSON = withText "RequestId" (pure . RequestId . encodeUtf8)

#ifdef WITH_PROTOBUF
instance EncodeWire RequestId where
    encodeWire t = encodeWire t . unRequestId

instance DecodeWire RequestId where
    decodeWire = fmap RequestId . decodeWire
#endif
