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

-- | Swagger schemas for these types are in 'Galley.API.Swagger'.
module Wire.API.Team.LegalHold
  ( NewLegalHoldService (..),
    ViewLegalHoldService (..),
    ViewLegalHoldServiceInfo (..),
    UserLegalHoldStatusResponse (..),
    RemoveLegalHoldSettingsRequest (..),
    DisableLegalHoldForUserRequest (..),
    ApproveLegalHoldForUserRequest (..),
  )
where

import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Misc
import Imports
import Wire.API.Provider
import Wire.API.Provider.Service (ServiceKeyPEM)
import Wire.API.User.Client.Prekey

--------------------------------------------------------------------------------
-- NewLegalHoldService

-- | This type is analogous to 'NewService' for bots.
data NewLegalHoldService = NewLegalHoldService
  { newLegalHoldServiceUrl :: HttpsUrl,
    newLegalHoldServiceKey :: ServiceKeyPEM,
    newLegalHoldServiceToken :: ServiceToken
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON NewLegalHoldService where
  toJSON s =
    object $
      "base_url" .= newLegalHoldServiceUrl s
        # "public_key" .= newLegalHoldServiceKey s
        # "auth_token" .= newLegalHoldServiceToken s
        # []

instance FromJSON NewLegalHoldService where
  parseJSON = withObject "NewLegalHoldService" $ \o ->
    NewLegalHoldService
      <$> o .: "base_url"
      <*> o .: "public_key"
      <*> o .: "auth_token"

--------------------------------------------------------------------------------
-- ViewLegalHoldService

data ViewLegalHoldService
  = ViewLegalHoldService ViewLegalHoldServiceInfo
  | ViewLegalHoldServiceNotConfigured
  | ViewLegalHoldServiceDisabled
  deriving stock (Eq, Show, Generic)

instance ToJSON ViewLegalHoldService where
  toJSON s = case s of
    ViewLegalHoldService settings ->
      object $
        "status" .= String "configured"
          # "settings" .= settings
          # []
    ViewLegalHoldServiceNotConfigured ->
      object $
        "status" .= String "not_configured"
          # []
    ViewLegalHoldServiceDisabled ->
      object $
        "status" .= String "disabled"
          # []

instance FromJSON ViewLegalHoldService where
  parseJSON = withObject "LegalHoldService" $ \o -> do
    status :: Text <- o .: "status"
    case status of
      "configured" -> ViewLegalHoldService <$> (o .: "settings")
      "not_configured" -> pure ViewLegalHoldServiceNotConfigured
      "disabled" -> pure ViewLegalHoldServiceDisabled
      _ -> fail "status (one of configured, not_configured, disabled)"

data ViewLegalHoldServiceInfo = ViewLegalHoldServiceInfo
  { viewLegalHoldServiceTeam :: TeamId,
    viewLegalHoldServiceUrl :: HttpsUrl,
    viewLegalHoldServiceFingerprint :: Fingerprint Rsa,
    viewLegalHoldServiceAuthToken :: ServiceToken,
    viewLegalHoldServiceKey :: ServiceKeyPEM
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ViewLegalHoldServiceInfo where
  toJSON info =
    object $
      "team_id" .= viewLegalHoldServiceTeam info
        # "base_url" .= viewLegalHoldServiceUrl info
        # "fingerprint" .= viewLegalHoldServiceFingerprint info
        # "auth_token" .= viewLegalHoldServiceAuthToken info
        # "public_key" .= viewLegalHoldServiceKey info
        # []

instance FromJSON ViewLegalHoldServiceInfo where
  parseJSON = withObject "LegalHoldServiceInfo" $ \o ->
    ViewLegalHoldServiceInfo
      <$> o .: "team_id"
      <*> o .: "base_url"
      <*> o .: "fingerprint"
      <*> o .: "auth_token"
      <*> o .: "public_key"

--------------------------------------------------------------------------------
-- UserLegalHoldStatusResponse

data UserLegalHoldStatusResponse = UserLegalHoldStatusResponse
  { ulhsrStatus :: UserLegalHoldStatus,
    -- | Exists only when status is Pending or Enabled
    ulhsrLastPrekey :: Maybe LastPrekey,
    -- | Exists only when status is Pending or Enabled
    ulhsrClientId :: Maybe ClientId
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON UserLegalHoldStatusResponse where
  toJSON (UserLegalHoldStatusResponse status lastPrekey' clientId') =
    object $
      "status" .= status
        # "last_prekey" .= lastPrekey'
        # "client" .= (IdObject <$> clientId')
        # []

instance FromJSON UserLegalHoldStatusResponse where
  parseJSON = withObject "UserLegalHoldStatusResponse" $ \o ->
    UserLegalHoldStatusResponse <$> o .: "status"
      <*> o .:? "last_prekey"
      <*> (fromIdObject @ClientId <$$> (o .:? "client"))

--------------------------------------------------------------------------------
-- RemoveLegalHoldSettingsRequest

data RemoveLegalHoldSettingsRequest = RemoveLegalHoldSettingsRequest
  { rmlhsrPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RemoveLegalHoldSettingsRequest where
  toJSON (RemoveLegalHoldSettingsRequest password) =
    object $
      "password" .= password
        # []

instance FromJSON RemoveLegalHoldSettingsRequest where
  parseJSON = withObject "RemoveLegalHoldSettingsRequest" $ \o ->
    RemoveLegalHoldSettingsRequest
      <$> o .:? "password"

--------------------------------------------------------------------------------
-- DisableLegalHoldForUserRequest

data DisableLegalHoldForUserRequest = DisableLegalHoldForUserRequest
  { dlhfuPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DisableLegalHoldForUserRequest where
  toJSON (DisableLegalHoldForUserRequest password) =
    object $
      "password" .= password
        # []

instance FromJSON DisableLegalHoldForUserRequest where
  parseJSON = withObject "DisableLegalHoldForUserRequest" $ \o ->
    DisableLegalHoldForUserRequest
      <$> o .:? "password"

--------------------------------------------------------------------------------
-- ApproveLegalHoldForUserRequest

data ApproveLegalHoldForUserRequest = ApproveLegalHoldForUserRequest
  { alhfuPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ApproveLegalHoldForUserRequest where
  toJSON (ApproveLegalHoldForUserRequest password) =
    object $
      "password" .= password
        # []

instance FromJSON ApproveLegalHoldForUserRequest where
  parseJSON = withObject "ApproveLegalHoldForUserRequest" $ \o ->
    ApproveLegalHoldForUserRequest
      <$> o .:? "password"
