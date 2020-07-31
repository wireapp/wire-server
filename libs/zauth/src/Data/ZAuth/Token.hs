{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Data.ZAuth.Token
  ( -- * Token
    Token,
    signature,
    header,
    body,
    mkToken,

    -- * Header
    Header,
    version,
    key,
    time,
    typ,
    tag,
    mkHeader,
    Type (..),
    Tag (..),

    -- * Access body
    Access,
    userId,
    connection,
    mkAccess,

    -- * User body
    User,
    user,
    rand,
    mkUser,

    -- * Bot body
    Bot,
    prov,
    bot,
    conv,
    mkBot,

    -- * Provider body
    Provider,
    provider,
    mkProvider,

    -- * LegalHold body
    LegalHoldUser,
    legalHoldUser,
    mkLegalHoldUser,
    LegalHoldAccess,
    legalHoldAccess,
    mkLegalHoldAccess,
    writeData,
  )
where

import Control.Error
import Control.Lens
import Data.Attoparsec.ByteString (takeLazyByteString)
import Data.ByteString.Base64.URL
import Data.ByteString.Builder (Builder, byteString, char8)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Char8 (break, drop, split)
import Data.UUID
import Imports hiding (break, drop)
import Sodium.Crypto.Sign (Signature (..))

data Type
  = -- | Access (Used as short-lived token for Users)
    A
  | -- | User (Used as a cookie for Users to refresh access tokens)
    U
  | -- | Bot
    B
  | -- | Provider
    P
  | -- | LegalHold Access (Used as short-lived token for LegalHold Service)
    LA
  | -- | LegalHold User (Used as a cookie for LegalHold Service to refresh access tokens)
    LU
  deriving (Eq, Show)

-- | Tag: Tokens for Users with no tag are refreshable themselves and called "UserToken"
-- Tokens for Users with the tag 'S' are non-refreshable themselves and called "SessionToken"
-- FUTUREWORK: rename 'S' to 'SessionTag' for clarity
data Tag = S deriving (Eq, Show)

data Token a = Token
  { _signature :: !Signature,
    _header :: !Header,
    _body :: !a
  }
  deriving (Eq, Show)

-- FUTUREWORK: maybe refactor to
-- data Header (t :: Type) =
--      Header { ... everything except _typ ...} ?
data Header = Header
  { _version :: !Int,
    _key :: !Int,
    _time :: !Integer,
    _typ :: !Type,
    _tag :: Maybe Tag
  }
  deriving (Eq, Show)

data Access = Access
  { _userId :: !UUID,
    -- | 'ConnId' is derived from this.
    _connection :: !Word64
  }
  deriving (Eq, Show)

data User = User
  { _user :: !UUID,
    _rand :: !Word32
  }
  deriving (Eq, Show)

data Bot = Bot
  { _prov :: !UUID,
    _bot :: !UUID,
    _conv :: !UUID
  }
  deriving (Eq, Show)

newtype Provider = Provider
  { _provider :: UUID
  }
  deriving (Eq, Show)

newtype LegalHoldUser = LegalHoldUser
  { _legalHoldUser :: User
  }
  deriving (Eq, Show)

newtype LegalHoldAccess = LegalHoldAccess
  { _legalHoldAccess :: Access
  }
  deriving (Eq, Show)

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

makeLenses ''LegalHoldUser

makeLenses ''LegalHoldAccess

instance FromByteString (Token Access) where
  parser =
    takeLazyByteString >>= \b ->
      case readToken A readAccessBody b of
        Nothing -> fail "Invalid access token"
        Just t -> return t

instance FromByteString (Token User) where
  parser =
    takeLazyByteString >>= \b ->
      case readToken U readUserBody b of
        Nothing -> fail "Invalid user token"
        Just t -> return t

instance FromByteString (Token Bot) where
  parser =
    takeLazyByteString >>= \b ->
      case readToken B readBotBody b of
        Nothing -> fail "Invalid bot token"
        Just t -> return t

instance FromByteString (Token Provider) where
  parser =
    takeLazyByteString >>= \b ->
      case readToken P readProviderBody b of
        Nothing -> fail "Invalid provider token"
        Just t -> return t

instance FromByteString (Token LegalHoldAccess) where
  parser =
    takeLazyByteString >>= \b ->
      case readToken LA readLegalHoldAccessBody b of
        Nothing -> fail "Invalid access token"
        Just t -> return t

instance FromByteString (Token LegalHoldUser) where
  parser =
    takeLazyByteString >>= \b ->
      case readToken LU readLegalHoldUserBody b of
        Nothing -> fail "Invalid user token"
        Just t -> return t

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

mkLegalHoldAccess :: UUID -> Word64 -> LegalHoldAccess
mkLegalHoldAccess uid cid = LegalHoldAccess $ Access uid cid

mkLegalHoldUser :: UUID -> Word32 -> LegalHoldUser
mkLegalHoldUser uid r = LegalHoldUser $ User uid r

-----------------------------------------------------------------------------
-- Reading

readToken :: Type -> (Properties -> Maybe a) -> LByteString -> Maybe (Token a)
readToken t f b = case split '.' b of
  (s : rest) ->
    let p = map pairwise rest
     in Token <$> hush (Signature <$> decode (toStrict s))
          <*> readHeader t p
          <*> f p
  _ -> Nothing
  where
    pairwise :: LByteString -> (LByteString, LByteString)
    pairwise x = let (k, v) = break (== '=') x in (k, drop 1 v)

readHeader :: Type -> Properties -> Maybe Header
readHeader t p =
  Header
    <$> (lookup "v" p >>= fromByteString')
    <*> (lookup "k" p >>= fromByteString')
    <*> (lookup "d" p >>= fromByteString')
    <*> (lookup "t" p >>= readType t)
    <*> (readTag <$> lookup "l" p)
  where
    readType A "a" = Just A
    readType U "u" = Just U
    readType B "b" = Just B
    readType P "p" = Just P
    readType LA "la" = Just LA
    readType LU "lu" = Just LU
    readType _ _ = Nothing
    readTag "s" = Just S
    readTag _ = Nothing

readAccessBody :: Properties -> Maybe Access
readAccessBody t =
  Access
    <$> (lookup "u" t >>= fromLazyASCIIBytes)
    <*> (lookup "c" t >>= fromByteString')

readUserBody :: Properties -> Maybe User
readUserBody t =
  User
    <$> (lookup "u" t >>= fromLazyASCIIBytes)
    <*> (lookup "r" t >>= fmap fromHex . fromByteString')

readBotBody :: Properties -> Maybe Bot
readBotBody t =
  Bot
    <$> (lookup "p" t >>= fromLazyASCIIBytes)
    <*> (lookup "b" t >>= fromLazyASCIIBytes)
    <*> (lookup "c" t >>= fromLazyASCIIBytes)

readProviderBody :: Properties -> Maybe Provider
readProviderBody t = Provider <$> (lookup "p" t >>= fromLazyASCIIBytes)

readLegalHoldAccessBody :: Properties -> Maybe LegalHoldAccess
readLegalHoldAccessBody t = LegalHoldAccess <$> readAccessBody t

readLegalHoldUserBody :: Properties -> Maybe LegalHoldUser
readLegalHoldUserBody t = LegalHoldUser <$> readUserBody t

-----------------------------------------------------------------------------
-- Writing

writeToken :: ToByteString a => Token a -> Builder
writeToken t =
  byteString (encode (sigBytes (t ^. signature)))
    <> dot
    <> writeData (t ^. header) (t ^. body)

writeData :: ToByteString a => Header -> a -> Builder
writeData h a = writeHeader h <> dot <> builder a

writeHeader :: Header -> Builder
writeHeader t =
  field "v" (t ^. version) <> dot
    <> field "k" (t ^. key)
    <> dot
    <> field "d" (t ^. time)
    <> dot
    <> field "t" (t ^. typ)
    <> dot
    <> field "l" (maybe mempty builder (t ^. tag))

instance ToByteString Access where
  builder t =
    field "u" (toLazyASCIIBytes $ t ^. userId) <> dot
      <> field "c" (t ^. connection)

instance ToByteString User where
  builder t =
    field "u" (toLazyASCIIBytes $ t ^. user) <> dot
      <> field "r" (Hex (t ^. rand))

instance ToByteString Bot where
  builder t =
    field "p" (toLazyASCIIBytes $ t ^. prov) <> dot
      <> field "b" (toLazyASCIIBytes $ t ^. bot)
      <> dot
      <> field "c" (toLazyASCIIBytes $ t ^. conv)

instance ToByteString Provider where
  builder t = field "p" (toLazyASCIIBytes $ t ^. provider)

instance ToByteString LegalHoldAccess where
  builder t =
    field "u" (toLazyASCIIBytes $ t ^. legalHoldAccess . userId) <> dot
      <> field "c" (t ^. legalHoldAccess . connection)

instance ToByteString LegalHoldUser where
  builder t =
    field "u" (toLazyASCIIBytes $ t ^. legalHoldUser . user) <> dot
      <> field "r" (Hex (t ^. legalHoldUser . rand))

instance ToByteString Type where
  builder A = char8 'a'
  builder U = char8 'u'
  builder B = char8 'b'
  builder P = char8 'p'
  builder LA = char8 'l' <> char8 'a'
  builder LU = char8 'l' <> char8 'u'

instance ToByteString Tag where
  builder S = char8 's'

field :: ToByteString a => LByteString -> a -> Builder
field k v = builder k <> eq <> builder v

dot, eq :: Builder
dot = char8 '.'
eq = char8 '='
