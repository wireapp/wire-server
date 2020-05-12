{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.Client.Prekey
  ( PrekeyId (..),
    Prekey (..),
    clientIdFromPrekey,
    LastPrekey,
    lastPrekey,
    unpackLastPrekey,
    lastPrekeyId,
    PrekeyBundle (..),
    ClientPrekey (..),

    -- * Swagger
    modelPrekeyBundle,
    modelClientPrekey,
    modelPrekey,
  )
where

import Data.Aeson
import Data.Hashable (hash)
import Data.Id
import qualified Data.Swagger.Build.Api as Doc
import Imports
import qualified Test.QuickCheck as QC
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

newtype PrekeyId = PrekeyId {keyId :: Word16}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, Arbitrary)

--------------------------------------------------------------------------------
-- Prekey

data Prekey = Prekey
  { prekeyId :: PrekeyId,
    prekeyKey :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Prekey)

modelPrekey :: Doc.Model
modelPrekey = Doc.defineModel "Prekey" $ do
  Doc.description "Prekey"
  Doc.property "id" Doc.int32' $
    Doc.description "Prekey ID"
  Doc.property "key" Doc.bytes' $
    Doc.description "Prekey data"

instance ToJSON Prekey where
  toJSON k =
    object
      [ "id" .= prekeyId k,
        "key" .= prekeyKey k
      ]

instance FromJSON Prekey where
  parseJSON = withObject "Prekey" $ \o ->
    Prekey <$> o .: "id" <*> o .: "key"

clientIdFromPrekey :: Prekey -> ClientId
clientIdFromPrekey prekey =
  newClientId . fromIntegral . hash . prekeyKey $ prekey

--------------------------------------------------------------------------------
-- LastPrekey

newtype LastPrekey = LastPrekey
  {unpackLastPrekey :: Prekey}
  deriving stock (Eq, Show, Generic)

instance ToJSON LastPrekey where
  toJSON = toJSON . unpackLastPrekey

instance FromJSON LastPrekey where
  parseJSON =
    parseJSON
      >=> ( \k ->
              if prekeyId k == lastPrekeyId
                then return $ LastPrekey k
                else fail "Invalid last prekey ID."
          )

instance Arbitrary LastPrekey where
  arbitrary = lastPrekey <$> arbitrary

lastPrekeyId :: PrekeyId
lastPrekeyId = PrekeyId maxBound

lastPrekey :: Text -> LastPrekey
lastPrekey = LastPrekey . Prekey lastPrekeyId

--------------------------------------------------------------------------------
-- PrekeyBundle

data PrekeyBundle = PrekeyBundle
  { prekeyUser :: OpaqueUserId,
    prekeyClients :: [ClientPrekey]
  }
  deriving (Eq, Show, Generic)

modelPrekeyBundle :: Doc.Model
modelPrekeyBundle = Doc.defineModel "PrekeyBundle" $ do
  Doc.description "Prekeys of all clients of a single user"
  Doc.property "user" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "clients" (Doc.array (Doc.ref modelClientPrekey)) $
    Doc.description "Prekeys of all clients"

instance ToJSON PrekeyBundle where
  toJSON k =
    object
      [ "user" .= prekeyUser k,
        "clients" .= prekeyClients k
      ]

instance FromJSON PrekeyBundle where
  parseJSON = withObject "PrekeyBundle" $ \o ->
    PrekeyBundle <$> o .: "user" <*> o .: "clients"

--------------------------------------------------------------------------------
-- ClientPrekey

data ClientPrekey = ClientPrekey
  { prekeyClient :: ClientId,
    prekeyData :: Prekey
  }
  deriving (Eq, Show, Generic)

modelClientPrekey :: Doc.Model
modelClientPrekey = Doc.defineModel "ClientPrekey" $ do
  Doc.description "Prekey of a single client"
  Doc.property "client" Doc.bytes' $
    Doc.description "Client Id"
  Doc.property "prekey" (Doc.ref modelPrekey) $
    Doc.description "Prekey"

instance ToJSON ClientPrekey where
  toJSON k =
    object
      [ "client" .= prekeyClient k,
        "prekey" .= prekeyData k
      ]

instance FromJSON ClientPrekey where
  parseJSON = withObject "ClientPrekey" $ \o ->
    ClientPrekey <$> o .: "client" <*> o .: "prekey"
