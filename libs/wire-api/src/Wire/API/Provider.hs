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

import Data.Aeson qualified as A
import Data.Id
import Data.Misc (HttpsUrl (..), PlainTextPassword6, PlainTextPassword8)
import Data.OpenApi qualified as S
import Data.Range
import Data.Schema
import Imports
import Wire.API.Conversation.Code as Code
import Wire.API.Provider.Service (ServiceToken (..))
import Wire.API.Provider.Service.Tag (ServiceTag (..))
import Wire.API.User.EmailAddress
import Wire.API.User.Profile (Name)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- Provider

-- | Full provider definition as seen by a verified provider itself.
data Provider = Provider
  { providerId :: ProviderId,
    providerName :: Name,
    providerEmail :: EmailAddress,
    providerUrl :: HttpsUrl,
    providerDescr :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Provider)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema Provider

instance ToSchema Provider where
  schema =
    object "Provider" $
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
  deriving newtype (A.FromJSON, A.ToJSON, Arbitrary, S.ToSchema)

--------------------------------------------------------------------------------
-- NewProvider

-- | Input data for registering a new provider.
data NewProvider = NewProvider
  { newProviderName :: Name,
    newProviderEmail :: EmailAddress,
    newProviderUrl :: HttpsUrl,
    newProviderDescr :: Range 1 1024 Text,
    -- | If none provided, a password is generated.
    newProviderPassword :: Maybe PlainTextPassword6
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewProvider)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema NewProvider

instance ToSchema NewProvider where
  schema =
    object "NewProvider" $
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
    rsNewProviderPassword :: Maybe PlainTextPassword8
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewProviderResponse)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema NewProviderResponse

instance ToSchema NewProviderResponse where
  schema =
    object "NewProviderResponse" $
      NewProviderResponse
        <$> rsNewProviderId .= field "id" schema
        <*> rsNewProviderPassword .= maybe_ (optField "password" schema)

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
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema UpdateProvider

instance ToSchema UpdateProvider where
  schema =
    object "UpdateProvider" $
      UpdateProvider
        <$> updateProviderName .= maybe_ (optField "name" schema)
        <*> updateProviderUrl .= maybe_ (optField "url" schema)
        <*> updateProviderDescr .= maybe_ (optField "description" schema)

--------------------------------------------------------------------------------
-- ProviderActivationResponse

-- | Successful response upon activating an email address (or possibly phone
-- number in the future) of a provider.
newtype ProviderActivationResponse = ProviderActivationResponse
  {activatedProviderIdentity :: EmailAddress}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema ProviderActivationResponse

instance ToSchema ProviderActivationResponse where
  schema =
    object "ProviderActivationResponse" $
      ProviderActivationResponse
        <$> activatedProviderIdentity .= field "email" schema

--------------------------------------------------------------------------------
-- ProviderLogin

-- | Input data for a provider login request.
data ProviderLogin = ProviderLogin
  { providerLoginEmail :: EmailAddress,
    providerLoginPassword :: PlainTextPassword6
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ProviderLogin)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema ProviderLogin

instance ToSchema ProviderLogin where
  schema =
    object "ProviderLogin" $
      ProviderLogin
        <$> providerLoginEmail .= field "email" schema
        <*> providerLoginPassword .= field "password" schema

--------------------------------------------------------------------------------
-- DeleteProvider

-- | Input data for a provider deletion request.
newtype DeleteProvider = DeleteProvider
  {deleteProviderPassword :: PlainTextPassword6}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema DeleteProvider

instance ToSchema DeleteProvider where
  schema =
    object "DeleteProvider" $
      DeleteProvider
        <$> deleteProviderPassword .= field "password" schema

--------------------------------------------------------------------------------
-- Password Change/Reset

-- | The payload for initiating a password reset.
newtype PasswordReset = PasswordReset {email :: EmailAddress}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema PasswordReset

instance ToSchema PasswordReset where
  schema =
    object "PasswordReset" $
      PasswordReset
        <$> (.email) .= field "email" schema

-- | The payload for completing a password reset.
data CompletePasswordReset = CompletePasswordReset
  { key :: Code.Key,
    code :: Code.Value,
    password :: PlainTextPassword6
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CompletePasswordReset)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema CompletePasswordReset

instance ToSchema CompletePasswordReset where
  schema =
    object "CompletePasswordReset" $
      CompletePasswordReset
        <$> key .= field "key" schema
        <*> (.code) .= field "code" schema
        <*> (.password) .= field "password" schema

-- | The payload for changing a password.
data PasswordChange = PasswordChange
  { oldPassword :: PlainTextPassword6,
    newPassword :: PlainTextPassword6
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PasswordChange)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema PasswordChange

instance ToSchema PasswordChange where
  schema =
    object "PasswordChange" $
      PasswordChange
        <$> oldPassword .= field "old_password" schema
        <*> newPassword .= field "new_password" schema

-- | The payload for updating an email address
newtype EmailUpdate = EmailUpdate {email :: EmailAddress}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema EmailUpdate

instance ToSchema EmailUpdate where
  schema =
    object "EmailUpdate" $
      EmailUpdate
        <$> (.email) .= field "email" schema
