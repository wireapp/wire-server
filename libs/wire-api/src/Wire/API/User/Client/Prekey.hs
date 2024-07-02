{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.Client.Prekey
  ( PrekeyId (..),
    Prekey (..),
    clientIdFromPrekey,
    LastPrekey,
    lastPrekey,
    unpackLastPrekey,
    fakeLastPrekey,
    lastPrekeyId,
    PrekeyBundle (..),
    ClientPrekey (..),
  )
where

import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bits
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.Id
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text.Encoding (encodeUtf8)
import Imports
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

newtype PrekeyId = PrekeyId {keyId :: Word16}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, Arbitrary, S.ToSchema, ToSchema)

--------------------------------------------------------------------------------
-- Prekey

data Prekey = Prekey
  { prekeyId :: PrekeyId,
    prekeyKey :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Prekey)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Prekey

instance ToSchema Prekey where
  schema =
    object "Prekey" $
      Prekey
        <$> prekeyId .= field "id" schema
        <*> prekeyKey .= field "key" schema

-- | Construct a new client ID from a prekey.
--
-- This works by taking the SHA256 hash of the prekey, truncating it to its
-- first 8 bytes, and interpreting the resulting bytestring as a big endian
-- Word64.
clientIdFromPrekey :: Prekey -> ClientId
clientIdFromPrekey =
  ClientId
    . foldl' (\w d -> (w `shiftL` 8) .|. fromIntegral d) 0
    . BS.unpack
    . BS.take 8
    . convert
    . hash @ByteString @SHA256
    . encodeUtf8
    . prekeyKey

--------------------------------------------------------------------------------
-- LastPrekey

newtype LastPrekey = LastPrekey
  {unpackLastPrekey :: Prekey}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema LastPrekey

instance ToSchema LastPrekey where
  schema = LastPrekey <$> unpackLastPrekey .= schema `withParser` check
    where
      check x =
        x
          <$ guard (prekeyId x == lastPrekeyId)
            <|> fail "Invalid last prekey ID"

instance Arbitrary LastPrekey where
  arbitrary = lastPrekey <$> arbitrary

lastPrekeyId :: PrekeyId
lastPrekeyId = PrekeyId maxBound

lastPrekey :: Text -> LastPrekey
lastPrekey = LastPrekey . Prekey lastPrekeyId

-- for tests only
-- This fake last prekey has the wrong prekeyId
fakeLastPrekey :: LastPrekey
fakeLastPrekey = LastPrekey $ Prekey (PrekeyId 7) "pQABAQcCoQBYIDXdN8VlKb5lbgPmoDPLPyqNIEyShG4oT/DlW0peRRZUA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="

--------------------------------------------------------------------------------
-- PrekeyBundle

data PrekeyBundle = PrekeyBundle
  { prekeyUser :: UserId,
    prekeyClients :: [ClientPrekey]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PrekeyBundle)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema PrekeyBundle

instance ToSchema PrekeyBundle where
  schema =
    object "PrekeyBundle" $
      PrekeyBundle
        <$> prekeyUser .= field "user" schema
        <*> prekeyClients .= field "clients" (array schema)

--------------------------------------------------------------------------------
-- ClientPrekey

data ClientPrekey = ClientPrekey
  { prekeyClient :: ClientId,
    prekeyData :: Prekey
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientPrekey)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ClientPrekey

instance ToSchema ClientPrekey where
  schema =
    object "ClientPrekey" $
      ClientPrekey
        <$> prekeyClient .= field "client" schema
        <*> prekeyData .= field "prekey" schema
