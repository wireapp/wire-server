{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.ZAuth.Token
    ( -- * Token
      Token
    , signature
    , header
    , body
    , mkToken

    -- * Header
    , Header
    , version
    , key
    , time
    , typ
    , tag
    , mkHeader

    , Type  (..)
    , Tag   (..)

    -- * Access body
    , Access
    , userId
    , connection
    , mkAccess

    -- * User body
    , User
    , user
    , rand
    , mkUser

    -- * Bot body
    , Bot
    , prov
    , bot
    , conv
    , mkBot

    -- * Provider body
    , Provider
    , provider
    , mkProvider

    , writeData
    ) where

import Imports hiding (break, drop)
import Control.Error
import Control.Lens
import Data.Attoparsec.ByteString (takeLazyByteString)
import Data.ByteString.Builder (Builder, byteString, char8)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Base64.URL
import Data.ByteString.Conversion
import Data.ByteString.Lazy.Char8 (split, break, drop)
import Data.UUID
import Sodium.Crypto.Sign (Signature (..))

data Type = A | U | B | P deriving (Eq, Show)
data Tag  = S deriving (Eq, Show)

data Token a = Token
    { _signature :: !Signature
    , _header    :: !Header
    , _body      :: !a
    } deriving (Eq, Show)

data Header = Header
    { _version   :: !Int
    , _key       :: !Int
    , _time      :: !Integer
    , _typ       :: !Type
    , _tag       :: Maybe Tag
    } deriving (Eq, Show)

data Access = Access
    { _userId     :: !UUID
    , _connection :: !Word64  -- ^ 'ConnId' is derived from this.
    } deriving (Eq, Show)

data User = User
    { _user :: !UUID
    , _rand :: !Word32
    } deriving (Eq, Show)

data Bot = Bot
    { _prov :: !UUID
    , _bot  :: !UUID
    , _conv :: !UUID
    } deriving (Eq, Show)

newtype Provider = Provider
    { _provider :: UUID
    } deriving (Eq, Show)

type Properties = [(LByteString, LByteString)]

signature :: Getter (Token a) Signature
signature = to _signature

header :: Getter (Token a) Header
header = to _header

body :: Getter (Token a) a
body = to _body

makeLenses ''Header
makeLenses ''Access
makeLenses ''User
makeLenses ''Bot
makeLenses ''Provider

instance FromByteString (Token Access) where
    parser = takeLazyByteString >>= \b ->
        case readToken readAccessBody b of
            Nothing -> fail "Invalid access token"
            Just  t -> return t

instance FromByteString (Token User) where
    parser = takeLazyByteString >>= \b ->
        case readToken readUserBody b of
            Nothing -> fail "Invalid user token"
            Just  t -> return t

instance FromByteString (Token Bot) where
    parser = takeLazyByteString >>= \b ->
        case readToken readBotBody b of
            Nothing -> fail "Invalid bot token"
            Just  t -> return t

instance FromByteString (Token Provider) where
    parser = takeLazyByteString >>= \b ->
        case readToken readProviderBody b of
            Nothing -> fail "Invalid provider token"
            Just  t -> return t

instance ToByteString a => ToByteString (Token a) where
    builder = writeToken

-----------------------------------------------------------------------------
-- Constructing

mkToken :: Signature -> Header -> a -> Token a
mkToken = Token

mkHeader :: Int -> Int -> Integer -> Type -> Maybe Tag -> Header
mkHeader v k d t g = Header v k d t g

mkAccess :: UUID -> Word64 -> Access
mkAccess = Access

mkUser :: UUID -> Word32 -> User
mkUser = User

mkBot :: UUID -> UUID -> UUID -> Bot
mkBot = Bot

mkProvider :: UUID -> Provider
mkProvider = Provider

-----------------------------------------------------------------------------
-- Reading

readToken :: (Properties -> Maybe a) -> LByteString -> Maybe (Token a)
readToken f b = case split '.' b of
    (s:rest) ->
        let p = map pairwise rest in
        Token <$> hush (Signature <$> decode (toStrict s))
              <*> readHeader p
              <*> f p
    _ -> Nothing
  where
    pairwise :: LByteString -> (LByteString, LByteString)
    pairwise x = let (k, v) = break (== '=') x in (k, drop 1 v)

readHeader :: Properties -> Maybe Header
readHeader p = Header
    <$> (lookup "v" p >>= fromByteString')
    <*> (lookup "k" p >>= fromByteString')
    <*> (lookup "d" p >>= fromByteString')
    <*> (lookup "t" p >>= readType)
    <*> (readTag <$> lookup "l" p)
  where
    readType "a" = Just A
    readType "u" = Just U
    readType "b" = Just B
    readType "p" = Just P
    readType _   = Nothing

    readTag "s" = Just S
    readTag _   = Nothing

readAccessBody :: Properties -> Maybe Access
readAccessBody t = Access
    <$> (lookup "u" t >>= fromLazyASCIIBytes)
    <*> (lookup "c" t >>= fromByteString')

readUserBody :: Properties -> Maybe User
readUserBody t = User
    <$> (lookup "u" t >>= fromLazyASCIIBytes)
    <*> (lookup "r" t >>= fmap fromHex . fromByteString')

readBotBody :: Properties -> Maybe Bot
readBotBody t = Bot
    <$> (lookup "p" t >>= fromLazyASCIIBytes)
    <*> (lookup "b" t >>= fromLazyASCIIBytes)
    <*> (lookup "c" t >>= fromLazyASCIIBytes)

readProviderBody :: Properties -> Maybe Provider
readProviderBody t = Provider <$> (lookup "p" t >>= fromLazyASCIIBytes)

-----------------------------------------------------------------------------
-- Writing

writeToken :: ToByteString a => Token a -> Builder
writeToken t = byteString (encode (sigBytes (t^.signature)))
    <> dot
    <> writeData (t^.header) (t^.body)

writeData :: ToByteString a => Header -> a -> Builder
writeData h a = writeHeader h <> dot <> builder a

writeHeader :: Header -> Builder
writeHeader t =
    field "v" (t^.version) <> dot <>
    field "k" (t^.key)     <> dot <>
    field "d" (t^.time)    <> dot <>
    field "t" (t^.typ)     <> dot <>
    field "l" (maybe mempty builder (t^.tag))

instance ToByteString Access where
    builder t = field "u" (toLazyASCIIBytes $ t^.userId) <> dot <>
                field "c" (t^.connection)

instance ToByteString User where
    builder t = field "u" (toLazyASCIIBytes $ t^.user) <> dot <>
                field "r" (Hex (t^.rand))

instance ToByteString Bot where
    builder t = field "p" (toLazyASCIIBytes $ t^.prov) <> dot <>
                field "b" (toLazyASCIIBytes $ t^.bot) <> dot <>
                field "c" (toLazyASCIIBytes $ t^.conv)

instance ToByteString Provider where
    builder t = field "p" (toLazyASCIIBytes $ t^.provider)

instance ToByteString Type where
    builder A = char8 'a'
    builder U = char8 'u'
    builder B = char8 'b'
    builder P = char8 'p'

instance ToByteString Tag where
    builder S = char8 's'

field :: ToByteString a => LByteString -> a -> Builder
field k v = builder k <> eq <> builder v

dot, eq :: Builder
dot = char8 '.'
eq  = char8 '='
