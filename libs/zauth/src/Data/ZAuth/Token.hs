{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Data.ZAuth.Token
  ( -- * Token
    Token (..),
    SerializableToken,

    -- * Header
    Header (..),
    Type (..),
    KnownType (..),
    Tag (..),

    -- * Body
    Body,

    -- * Access body
    Access (..),

    -- * User body
    User (..),
    -- -- * UserTokenType
    -- UserTokenType (..),
    -- accessTokenType,
    -- userTokenType,
    -- KnownUserTokenType (..),

    -- * Bot body
    Bot (..),

    -- * Provider body
    Provider (..),

    -- * Serialization
    writeData,
  )
where

import Control.Error
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

class KnownType (t :: Type) where
  typeVal :: Type

instance KnownType A where
  typeVal = A

instance KnownType U where
  typeVal = U

instance KnownType B where
  typeVal = B

instance KnownType P where
  typeVal = P

instance KnownType LA where
  typeVal = LA

instance KnownType LU where
  typeVal = LU

-- | Tag: Tokens for Users with no tag are refreshable themselves and called "UserToken"
-- Tokens for Users with the tag 'S' are non-refreshable themselves and called "SessionToken"
-- FUTUREWORK: rename 'S' to 'SessionTag' for clarity
data Tag = S deriving (Eq, Show)

data Token (t :: Type) = Token
  { signature :: !Signature,
    header :: !(Header t),
    body :: !(Body t)
  }

deriving instance (Eq (Body t)) => Eq (Token t)

deriving instance (Show (Body t)) => Show (Token t)

data Header (t :: Type) = Header
  { version :: !Int,
    key :: !Int,
    time :: !Integer,
    tag :: Maybe Tag
  }
  deriving (Eq, Show)

type family Body (t :: Type) where
  Body A = Access
  Body U = User
  Body B = Bot
  Body P = Provider
  Body LA = Access
  Body LU = User

data Access = Access
  { userId :: !UUID,
    clientId :: Maybe Text,
    -- | 'ConnId' is derived from this.
    connection :: !Word64
  }
  deriving (Eq, Show)

data User = User
  { user :: !UUID,
    client :: Maybe Text,
    rand :: !Word32
  }
  deriving (Eq, Show)

data Bot = Bot
  { prov :: !UUID,
    bot :: !UUID,
    conv :: !UUID
  }
  deriving (Eq, Show)

newtype Provider = Provider
  { provider :: UUID
  }
  deriving (Eq, Show)

type Properties = [(LByteString, LByteString)]

instance (KnownType t, ReadProperties (Body t)) => FromByteString (Token t) where
  parser =
    takeLazyByteString >>= \bs ->
      case readToken bs of
        Nothing -> fail "Invalid token"
        Just t -> pure t

-- | Litte alias to help with reducing constraints. These constraints come
-- together in many places because the 'Header' and 'Body' need to be serialized
-- separately from the 'Token' for signing.
--
-- Perhaps it'd be better to just have a special type for an unsigned token.
type SerializableToken t = (ToByteString (Header t), ToByteString (Body t))

instance (SerializableToken t) => ToByteString (Token t) where
  builder = writeToken

-----------------------------------------------------------------------------
-- Reading

readToken :: (KnownType t, ReadProperties (Body t)) => LByteString -> Maybe (Token t)
readToken b = case split '.' b of
  (s : rest) ->
    let p = map pairwise rest
     in Token
          <$> hush (Signature <$> decode (toStrict s))
          <*> readProperties p
          <*> readProperties p
  _ -> Nothing
  where
    pairwise :: LByteString -> (LByteString, LByteString)
    pairwise x = let (k, v) = break (== '=') x in (k, drop 1 v)

class ReadProperties a where
  readProperties :: Properties -> Maybe a

instance (KnownType t) => ReadProperties (Header t) where
  readProperties p = do
    void $ lookup "t" p >>= readType (typeVal @t)
    Header
      <$> (lookup "v" p >>= fromByteString')
      <*> (lookup "k" p >>= fromByteString')
      <*> (lookup "d" p >>= fromByteString')
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

instance ReadProperties Access where
  readProperties t =
    Access
      <$> (lookup "u" t >>= fromLazyASCIIBytes)
      <*> pure (lookup "i" t >>= fromByteString')
      <*> (lookup "c" t >>= fromByteString')

instance ReadProperties User where
  readProperties t =
    User
      <$> (lookup "u" t >>= fromLazyASCIIBytes)
      <*> pure (lookup "i" t >>= fromByteString')
      <*> (lookup "r" t >>= fmap fromHex . fromByteString')

instance ReadProperties Bot where
  readProperties t =
    Bot
      <$> (lookup "p" t >>= fromLazyASCIIBytes)
      <*> (lookup "b" t >>= fromLazyASCIIBytes)
      <*> (lookup "c" t >>= fromLazyASCIIBytes)

instance ReadProperties Provider where
  readProperties t = Provider <$> (lookup "p" t >>= fromLazyASCIIBytes)

-----------------------------------------------------------------------------
-- Writing

writeToken :: (ToByteString (Body t), ToByteString (Header t)) => Token t -> Builder
writeToken t =
  byteString (encode (sigBytes (t.signature)))
    <> dot
    <> writeData (t.header) (t.body)

writeData :: (SerializableToken t) => Header t -> Body t -> Builder
writeData h a = builder h <> dot <> builder a

instance (KnownType t) => ToByteString (Header t) where
  builder h =
    field "v" h.version
      <> dot
      <> field "k" h.key
      <> dot
      <> field "d" h.time
      <> dot
      <> field "t" (typeVal @t)
      <> dot
      <> field "l" (foldMap builder (h.tag))

instance ToByteString Access where
  builder t =
    field "u" (toLazyASCIIBytes $ t.userId)
      <> foldMap (\c -> dot <> field "i" c) (t.clientId)
      <> dot
      <> field "c" (t.connection)

instance ToByteString User where
  builder t =
    field "u" (toLazyASCIIBytes $ t.user)
      <> dot
      <> field "r" (Hex (t.rand))
      <> foldMap (\c -> dot <> field "i" c) (t.client)

instance ToByteString Bot where
  builder t =
    field "p" (toLazyASCIIBytes $ t.prov)
      <> dot
      <> field "b" (toLazyASCIIBytes $ t.bot)
      <> dot
      <> field "c" (toLazyASCIIBytes $ t.conv)

instance ToByteString Provider where
  builder t = field "p" (toLazyASCIIBytes $ t.provider)

instance ToByteString Type where
  builder A = char8 'a'
  builder U = char8 'u'
  builder B = char8 'b'
  builder P = char8 'p'
  builder LA = char8 'l' <> char8 'a'
  builder LU = char8 'l' <> char8 'u'

instance ToByteString Tag where
  builder S = char8 's'

field :: (ToByteString a) => LByteString -> a -> Builder
field k v = builder k <> eq <> builder v

dot, eq :: Builder
dot = char8 '.'
eq = char8 '='
