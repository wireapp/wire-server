{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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

-- | Types for the (internal) provider API.
--
-- FUTUREWORK: Deduplicate with 'Wire.API.User'?
module Wire.API.Provider
  ( -- * Provider
    Provider (..),
    ProviderProfile (..),

    -- * NewProvider
    NewProvider (..),
    NewProviderResponse (..),

    -- * UpdateProvider
    UpdateProvider (..),

    -- * ProviderActivationResponse
    ProviderActivationResponse (..),
    ProviderLogin (..),
    DeleteProvider (..),

    -- * Password Change/Reset
    PasswordReset (..),
    CompletePasswordReset (..),
    PasswordChange (..),
    EmailUpdate (..),

    -- * Bot
    AddBot (..),
    AddBotResponse (..),
    RemoveBotResponse (..),
    UpdateBotPrekeys (..),

    -- * Re-exports
    HttpsUrl (..),
    ServiceToken (..),
    ServiceTag (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Id
import Data.Json.Util
import Data.Misc (HttpsUrl (..), PlainTextPassword (..))
import Data.Range
import Imports
import Wire.API.Conversation.Code as Code
import Wire.API.Conversation.Event (Event)
import Wire.API.Provider.Tag (ServiceTag (..))
import Wire.API.Service (ServiceToken (..))
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile

--------------------------------------------------------------------------------
-- Provider

-- | Full provider definition as seen by a verified provider itself.
data Provider = Provider
  { providerId :: !ProviderId,
    providerName :: !Name,
    providerEmail :: !Email,
    providerUrl :: !HttpsUrl,
    providerDescr :: !Text
  }
  deriving (Eq, Show)

instance ToJSON Provider where
  toJSON p =
    object $
      "id" .= providerId p
        # "name" .= providerName p
        # "email" .= providerEmail p
        # "url" .= providerUrl p
        # "description" .= providerDescr p
        # []

instance FromJSON Provider where
  parseJSON = withObject "Provider" $ \o ->
    Provider <$> o .: "id"
      <*> o .: "name"
      <*> o .: "email"
      <*> o .: "url"
      <*> o .: "description"

-- | A provider profile as seen by regular users.
-- Note: This is a placeholder that may evolve to contain only a subset of
-- the full provider information.
newtype ProviderProfile = ProviderProfile Provider
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- NewProvider

-- | Input data for registering a new provider.
data NewProvider = NewProvider
  { newProviderName :: !Name,
    newProviderEmail :: !Email,
    newProviderUrl :: !HttpsUrl,
    newProviderDescr :: !(Range 1 1024 Text),
    -- | If none provided, a password is generated.
    newProviderPassword :: !(Maybe PlainTextPassword)
  }

instance ToJSON NewProvider where
  toJSON p =
    object $
      "name" .= newProviderName p
        # "email" .= newProviderEmail p
        # "url" .= newProviderUrl p
        # "description" .= newProviderDescr p
        # "password" .= newProviderPassword p
        # []

instance FromJSON NewProvider where
  parseJSON = withObject "NewProvider" $ \o ->
    NewProvider <$> o .: "name"
      <*> o .: "email"
      <*> o .: "url"
      <*> o .: "description"
      <*> o .:? "password"

-- | Response data upon registering a new provider.
data NewProviderResponse = NewProviderResponse
  { rsNewProviderId :: !ProviderId,
    -- | The generated password, if none was provided
    -- in the 'NewProvider' request.
    rsNewProviderPassword :: !(Maybe PlainTextPassword)
  }

instance ToJSON NewProviderResponse where
  toJSON r =
    object $
      "id" .= rsNewProviderId r
        # "password" .= rsNewProviderPassword r
        # []

instance FromJSON NewProviderResponse where
  parseJSON = withObject "NewProviderResponse" $ \o ->
    NewProviderResponse <$> o .: "id"
      <*> o .:? "password"

--------------------------------------------------------------------------------
-- UpdateProvider

-- | Input data for updating general provider information.
data UpdateProvider = UpdateProvider
  { updateProviderName :: !(Maybe Name),
    updateProviderUrl :: !(Maybe HttpsUrl),
    updateProviderDescr :: !(Maybe Text)
  }
  deriving (Eq, Show)

instance ToJSON UpdateProvider where
  toJSON p =
    object $
      "name" .= updateProviderName p
        # "url" .= updateProviderUrl p
        # "description" .= updateProviderDescr p
        # []

instance FromJSON UpdateProvider where
  parseJSON = withObject "UpdateProvider" $ \o ->
    UpdateProvider <$> o .:? "name"
      <*> o .:? "url"
      <*> o .:? "description"

--------------------------------------------------------------------------------
-- ProviderActivationResponse

-- | Successful response upon activating an email address (or possibly phone
-- number in the future) of a provider.
newtype ProviderActivationResponse = ProviderActivationResponse
  {activatedProviderIdentity :: Email}
  deriving (Eq, Show)

instance ToJSON ProviderActivationResponse where
  toJSON (ProviderActivationResponse e) =
    object ["email" .= e]

instance FromJSON ProviderActivationResponse where
  parseJSON = withObject "ProviderActivationResponse" $ \o ->
    ProviderActivationResponse <$> o .: "email"

--------------------------------------------------------------------------------
-- ProviderLogin

-- | Input data for a provider login request.
data ProviderLogin = ProviderLogin
  { providerLoginEmail :: !Email,
    providerLoginPassword :: !PlainTextPassword
  }

instance ToJSON ProviderLogin where
  toJSON l =
    object
      [ "email" .= providerLoginEmail l,
        "password" .= providerLoginPassword l
      ]

instance FromJSON ProviderLogin where
  parseJSON = withObject "ProviderLogin" $ \o ->
    ProviderLogin <$> o .: "email"
      <*> o .: "password"

--------------------------------------------------------------------------------
-- DeleteProvider

-- | Input data for a provider deletion request.
newtype DeleteProvider = DeleteProvider
  {deleteProviderPassword :: PlainTextPassword}

instance ToJSON DeleteProvider where
  toJSON d =
    object
      [ "password" .= deleteProviderPassword d
      ]

instance FromJSON DeleteProvider where
  parseJSON = withObject "DeleteProvider" $ \o ->
    DeleteProvider <$> o .: "password"

--------------------------------------------------------------------------------
-- Password Change/Reset

-- | The payload for initiating a password reset.
newtype PasswordReset = PasswordReset {nprEmail :: Email}

deriveJSON toJSONFieldName ''PasswordReset

-- | The payload for completing a password reset.
data CompletePasswordReset = CompletePasswordReset
  { cpwrKey :: !Code.Key,
    cpwrCode :: !Code.Value,
    cpwrPassword :: !PlainTextPassword
  }

deriveJSON toJSONFieldName ''CompletePasswordReset

-- | The payload for changing a password.
data PasswordChange = PasswordChange
  { cpOldPassword :: !PlainTextPassword,
    cpNewPassword :: !PlainTextPassword
  }

deriveJSON toJSONFieldName ''PasswordChange

-- | The payload for updating an email address
newtype EmailUpdate = EmailUpdate {euEmail :: Email}

deriveJSON toJSONFieldName ''EmailUpdate

--------------------------------------------------------------------------------
-- AddBot

-- | Input data for adding a bot to a conversation.
data AddBot = AddBot
  { addBotProvider :: !ProviderId,
    addBotService :: !ServiceId,
    addBotLocale :: !(Maybe Locale)
  }

instance ToJSON AddBot where
  toJSON n =
    object $
      "provider" .= addBotProvider n
        # "service" .= addBotService n
        # "locale" .= addBotLocale n
        # []

instance FromJSON AddBot where
  parseJSON = withObject "NewBot" $ \o ->
    AddBot <$> o .: "provider"
      <*> o .: "service"
      <*> o .:? "locale"

data AddBotResponse = AddBotResponse
  { rsAddBotId :: !BotId,
    rsAddBotClient :: !ClientId,
    rsAddBotName :: !Name,
    rsAddBotColour :: !ColourId,
    rsAddBotAssets :: ![Asset],
    rsAddBotEvent :: !Event
  }

instance ToJSON AddBotResponse where
  toJSON r =
    object
      [ "id" .= rsAddBotId r,
        "client" .= rsAddBotClient r,
        "name" .= rsAddBotName r,
        "accent_id" .= rsAddBotColour r,
        "assets" .= rsAddBotAssets r,
        "event" .= rsAddBotEvent r
      ]

instance FromJSON AddBotResponse where
  parseJSON = withObject "AddBotResponse" $ \o ->
    AddBotResponse <$> o .: "id"
      <*> o .: "client"
      <*> o .: "name"
      <*> o .: "accent_id"
      <*> o .: "assets"
      <*> o .: "event"

--------------------------------------------------------------------------------
-- RemoveBot

-- (There is no request payload for bot removal)

newtype RemoveBotResponse = RemoveBotResponse
  { rsRemoveBotEvent :: Event
  }

instance ToJSON RemoveBotResponse where
  toJSON r =
    object
      [ "event" .= rsRemoveBotEvent r
      ]

instance FromJSON RemoveBotResponse where
  parseJSON = withObject "RemoveBotResponse" $ \o ->
    RemoveBotResponse <$> o .: "event"

--------------------------------------------------------------------------------
-- UpdateBotPrekeys

newtype UpdateBotPrekeys = UpdateBotPrekeys
  { updateBotPrekeyList :: [Prekey]
  }

instance ToJSON UpdateBotPrekeys where
  toJSON u =
    object
      [ "prekeys" .= updateBotPrekeyList u
      ]

instance FromJSON UpdateBotPrekeys where
  parseJSON = withObject "UpdateBotPrekeys" $ \o ->
    UpdateBotPrekeys <$> o .: "prekeys"
