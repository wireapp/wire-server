{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Wire.API.User.Client
  ( module Wire.API.User.Client,
    module P,

    -- * UserClients
    UserClientMap (..),
    UserClients (..),
    filterClients,
  )
where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util
import qualified Data.Map.Strict as Map
import Data.Misc (Location, PlainTextPassword (..))
import qualified Data.Text.Encoding as Text.E
import Data.UUID (toASCIIBytes)
import Imports
import Wire.API.User.Auth (CookieLabel)
import Wire.API.User.Client.Prekey as P
import Wire.API.User.Profile ()

newtype UserClientMap a = UserClientMap
  { userClientMap :: Map OpaqueUserId (Map ClientId a)
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

instance ToJSON a => ToJSON (UserClientMap a) where
  toJSON = toJSON . Map.foldrWithKey' f Map.empty . userClientMap
    where
      f (Id u) clients m =
        let key = Text.E.decodeLatin1 (toASCIIBytes u)
            val = Map.foldrWithKey' g Map.empty clients
         in Map.insert key val m
      g (ClientId c) a = Map.insert c (toJSON a)

instance FromJSON a => FromJSON (UserClientMap a) where
  parseJSON = withObject "user-client-map" $ \o ->
    UserClientMap <$> foldrM f Map.empty (HashMap.toList o)
    where
      f (k, v) m = do
        u <- parseJSON (String k)
        flip (withObject "client-value-map") v $ \c -> do
          e <- foldrM g Map.empty (HashMap.toList c)
          return (Map.insert u e m)
      g (k, v) m = do
        c <- parseJSON (String k)
        t <- parseJSON v
        return (Map.insert c t m)

newtype UserClients = UserClients
  { userClients :: Map OpaqueUserId (Set ClientId)
  }
  deriving (Eq, Show, Semigroup, Monoid, Generic)

instance ToJSON UserClients where
  toJSON =
    toJSON . Map.foldrWithKey' fn Map.empty . userClients
    where
      fn u c m =
        let k = Text.E.decodeLatin1 (toASCIIBytes (toUUID u))
         in Map.insert k c m

instance FromJSON UserClients where
  parseJSON =
    withObject "UserClients" (fmap UserClients . foldrM fn Map.empty . HashMap.toList)
    where
      fn (k, v) m = Map.insert <$> parseJSON (String k) <*> parseJSON v <*> pure m

-- TODO: internal?
filterClients :: (Set ClientId -> Bool) -> UserClients -> UserClients
filterClients p (UserClients c) = UserClients $ Map.filter p c

-- [Note: LegalHold]
--
-- Short feature description:
-- LegalHold is an enterprise feature, enabled on a per-team basis, and within a
-- team on a per-user basis

-- * A LegalHoldClient is a client outside that user's control (but under the
--   control of that team's business)

-- * Users need to click "accept" before a LegalHoldClient is added to their
--   account.

-- * Any user interacting with a user which has a LegalHoldClient will upon
--   first interaction receive a warning, have the option of cancelling the
--   interation, and on an ongoing basis see a visual indication in all
--   conversations where such a device is active.

data ClientType
  = TemporaryClientType
  | PermanentClientType
  | LegalHoldClientType -- see Note [LegalHold]
  deriving (Eq, Ord, Show)

data ClientClass
  = PhoneClient
  | TabletClient
  | DesktopClient
  | LegalHoldClient -- see Note [LegalHold]
  deriving (Eq, Ord, Show)

data NewClient = NewClient
  { newClientPrekeys :: [Prekey],
    newClientLastKey :: !LastPrekey,
    newClientType :: !ClientType,
    newClientLabel :: !(Maybe Text),
    newClientClass :: !(Maybe ClientClass),
    newClientCookie :: !(Maybe CookieLabel),
    newClientPassword :: !(Maybe PlainTextPassword),
    newClientModel :: !(Maybe Text)
  }

newClient :: ClientType -> LastPrekey -> NewClient
newClient t k =
  NewClient
    { newClientPrekeys = [],
      newClientLastKey = k,
      newClientType = t,
      newClientLabel = Nothing,
      newClientClass = if t == LegalHoldClientType then Just LegalHoldClient else Nothing,
      newClientCookie = Nothing,
      newClientPassword = Nothing,
      newClientModel = Nothing
    }

data Client = Client
  { clientId :: !ClientId,
    clientType :: !ClientType,
    clientTime :: !UTCTimeMillis,
    clientClass :: !(Maybe ClientClass),
    clientLabel :: !(Maybe Text),
    clientCookie :: !(Maybe CookieLabel),
    clientLocation :: !(Maybe Location),
    clientModel :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

data PubClient = PubClient
  { pubClientId :: !ClientId,
    pubClientClass :: !(Maybe ClientClass)
  }
  deriving (Eq, Show, Generic)

newtype RmClient = RmClient
  { rmPassword :: Maybe PlainTextPassword
  }
  deriving (Generic)

data UpdateClient = UpdateClient
  { updateClientPrekeys :: ![Prekey],
    updateClientLastKey :: !(Maybe LastPrekey),
    updateClientLabel :: !(Maybe Text)
  }
  deriving (Generic)

-- * JSON instances:

instance ToJSON Client where
  toJSON c =
    object $
      "id" .= clientId c
        # "type" .= clientType c
        # "label" .= clientLabel c
        # "class" .= clientClass c
        # "time" .= clientTime c
        # "cookie" .= clientCookie c
        # "location" .= clientLocation c
        # "model" .= clientModel c
        # []

instance FromJSON Client where
  parseJSON = withObject "Client" $ \o ->
    Client <$> o .: "id"
      <*> o .: "type"
      <*> o .: "time"
      <*> o .:? "class"
      <*> o .:? "label"
      <*> o .:? "cookie"
      <*> o .:? "location"
      <*> o .:? "model"

instance ToJSON PubClient where
  toJSON c =
    object $
      "id" .= pubClientId c
        # "class" .= pubClientClass c
        # []

instance FromJSON PubClient where
  parseJSON = withObject "PubClient" $ \o ->
    PubClient <$> o .: "id"
      <*> o .:? "class"

instance ToJSON ClientType where
  toJSON TemporaryClientType = String "temporary"
  toJSON PermanentClientType = String "permanent"
  toJSON LegalHoldClientType = String "legalhold"

instance FromJSON ClientType where
  parseJSON = withText "ClientType" $ \txt -> case txt of
    "temporary" -> return TemporaryClientType
    "permanent" -> return PermanentClientType
    "legalhold" -> return LegalHoldClientType
    _ -> fail "Must be one of {'temporary', 'permanent', 'legalhold'}."

instance ToJSON ClientClass where
  toJSON PhoneClient = String "phone"
  toJSON TabletClient = String "tablet"
  toJSON DesktopClient = String "desktop"
  toJSON LegalHoldClient = String "legalhold"

instance FromJSON ClientClass where
  parseJSON = withText "ClientClass" $ \txt -> case txt of
    "phone" -> return PhoneClient
    "tablet" -> return TabletClient
    "desktop" -> return DesktopClient
    "legalhold" -> return LegalHoldClient
    _ -> fail "Must be one of {'phone', 'tablet', 'desktop', 'legalhold'}."

instance ToJSON NewClient where
  toJSON c =
    object $
      "type" .= newClientType c
        # "prekeys" .= newClientPrekeys c
        # "lastkey" .= newClientLastKey c
        # "label" .= newClientLabel c
        # "class" .= newClientClass c
        # "cookie" .= newClientCookie c
        # "password" .= newClientPassword c
        # "model" .= newClientModel c
        # []

instance FromJSON NewClient where
  parseJSON = withObject "NewClient" $ \o ->
    NewClient <$> o .: "prekeys"
      <*> o .: "lastkey"
      <*> o .: "type"
      <*> o .:? "label"
      <*> o .:? "class"
      <*> o .:? "cookie"
      <*> o .:? "password"
      <*> o .:? "model"

instance ToJSON RmClient where
  toJSON (RmClient pw) = object ["password" .= pw]

instance FromJSON RmClient where
  parseJSON = withObject "RmClient" $ \o ->
    RmClient <$> o .:? "password"

instance ToJSON UpdateClient where
  toJSON c =
    object $
      "prekeys" .= updateClientPrekeys c
        # "lastkey" .= updateClientLastKey c
        # "label" .= updateClientLabel c
        # []

instance FromJSON UpdateClient where
  parseJSON = withObject "RefreshClient" $ \o ->
    UpdateClient <$> o .:? "prekeys" .!= []
      <*> o .:? "lastkey"
      <*> o .:? "label"
