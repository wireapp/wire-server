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

    -- * Re-exports
    HttpsUrl (..),
    ServiceToken (..),
    ServiceTag (..),
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Misc (HttpsUrl (..), PlainTextPassword (..))
import Data.Range
import Data.Schema
import qualified Data.Schema as Schema (object)
import qualified Data.Swagger as S
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Conversation.Code as Code
import Wire.API.Provider.Service (ServiceToken (..))
import Wire.API.Provider.Service.Tag (ServiceTag (..))
import Wire.API.User.Identity (Email)
import Wire.API.User.Profile (Name)

--------------------------------------------------------------------------------
-- Provider

-- | Full provider definition as seen by a verified provider itself.
data Provider = Provider
  { providerId :: ProviderId,
    providerName :: Name,
    providerEmail :: Email,
    providerUrl :: HttpsUrl,
    providerDescr :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Provider)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Provider

instance ToSchema Provider where
  schema =
    Schema.object "Provider" $
      Provider
        <$> providerId .= field "id" schema
        <*> providerName .= field "name" schema
        <*> providerEmail .= field "email" schema
        <*> providerUrl .= field "url" schema
        <*> providerDescr .= field "description" schema

-- | A provider profile as seen by regular users.
-- Note: This is a placeholder that may evolve to contain only a subset of
-- the full provider information.
newtype ProviderProfile = ProviderProfile Provider
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, S.ToSchema, Arbitrary)

--------------------------------------------------------------------------------
-- NewProvider

-- | Input data for registering a new provider.
data NewProvider = NewProvider
  { newProviderName :: Name,
    newProviderEmail :: Email,
    newProviderUrl :: HttpsUrl,
    newProviderDescr :: Range 1 1024 Text,
    -- | If none provided, a password is generated.
    newProviderPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewProvider)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema NewProvider

instance ToSchema NewProvider where
  schema =
    Schema.object "NewProvider" $
      NewProvider
        <$> newProviderName .= field "name" schema
        <*> newProviderEmail .= field "email" schema
        <*> newProviderUrl .= field "url" schema
        <*> newProviderDescr .= field "description" schema
        <*> newProviderPassword .= maybe_ (optField "password" schema)

-- | Response data upon registering a new provider.
data NewProviderResponse = NewProviderResponse
  { rsNewProviderId :: ProviderId,
    -- | The generated password, if none was provided
    -- in the 'NewProvider' request.
    rsNewProviderPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewProviderResponse)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema NewProviderResponse

instance ToSchema NewProviderResponse where
  schema =
    Schema.object "NewProviderResponse" $
      NewProviderResponse
        <$> rsNewProviderId .= field "id" schema
        <*> rsNewProviderPassword .= maybe_ (optFieldWithDocModifier "password" (S.description ?~ "The generated password, if none was provided in the request.") schema)

--------------------------------------------------------------------------------
-- UpdateProvider

-- | Input data for updating general provider information.
data UpdateProvider = UpdateProvider
  { updateProviderName :: Maybe Name,
    updateProviderUrl :: Maybe HttpsUrl,
    updateProviderDescr :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateProvider)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema UpdateProvider

instance ToSchema UpdateProvider where
  schema =
    Schema.object "UpdateProvider" $
      UpdateProvider
        <$> updateProviderName .= maybe_ (optField "name" schema)
        <*> updateProviderUrl .= maybe_ (optField "url" schema)
        <*> updateProviderDescr .= maybe_ (optField "description" schema)

--------------------------------------------------------------------------------
-- ProviderActivationResponse

-- | Successful response upon activating an email address (or possibly phone
-- number in the future) of a provider.
newtype ProviderActivationResponse = ProviderActivationResponse
  {activatedProviderIdentity :: Email}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ProviderActivationResponse

instance ToSchema ProviderActivationResponse where
  schema =
    Schema.object "ProviderActivationResponse" $
      ProviderActivationResponse
        <$> activatedProviderIdentity .= field "email" schema

--------------------------------------------------------------------------------
-- ProviderLogin

-- | Input data for a provider login request.
data ProviderLogin = ProviderLogin
  { providerLoginEmail :: Email,
    providerLoginPassword :: PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ProviderLogin)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ProviderLogin

instance ToSchema ProviderLogin where
  schema =
    Schema.object "ProviderLogin" $
      ProviderLogin
        <$> providerLoginEmail .= field "email" schema
        <*> providerLoginPassword .= field "password" schema

--------------------------------------------------------------------------------
-- DeleteProvider

-- | Input data for a provider deletion request.
newtype DeleteProvider = DeleteProvider
  {deleteProviderPassword :: PlainTextPassword}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema DeleteProvider

instance ToSchema DeleteProvider where
  schema =
    Schema.object "DeleteProvider" $
      DeleteProvider <$> deleteProviderPassword .= field "password" schema

--------------------------------------------------------------------------------
-- Password Change/Reset

-- | The payload for initiating a password reset.
newtype PasswordReset = PasswordReset {nprEmail :: Email}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema PasswordReset

instance ToSchema PasswordReset where
  schema =
    Schema.object "PasswordReset" $
      PasswordReset
        <$> nprEmail .= field "email" schema

-- | The payload for completing a password reset.
data CompletePasswordReset = CompletePasswordReset
  { cpwrKey :: Code.Key,
    cpwrCode :: Code.Value,
    cpwrPassword :: PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CompletePasswordReset)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema CompletePasswordReset

instance ToSchema CompletePasswordReset where
  schema =
    Schema.object "CompletePasswordReset" $
      CompletePasswordReset
        <$> cpwrKey .= field "key" schema
        <*> cpwrCode .= field "code" schema
        <*> cpwrPassword .= field "password" schema

-- | The payload for changing a password.
data PasswordChange = PasswordChange
  { cpOldPassword :: PlainTextPassword,
    cpNewPassword :: PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PasswordChange)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema PasswordChange

instance ToSchema PasswordChange where
  schema =
    Schema.object "PasswordChange" $
      PasswordChange
        <$> cpOldPassword .= field "old_password" schema
        <*> cpNewPassword .= field "new_password" schema

-- | The payload for updating an email address
newtype EmailUpdate = EmailUpdate {euEmail :: Email}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema EmailUpdate

instance ToSchema EmailUpdate where
  schema =
    Schema.object "EmailUpdate" $
      EmailUpdate
        <$> euEmail .= field "email" schema
