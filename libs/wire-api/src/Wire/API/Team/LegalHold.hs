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

module Wire.API.Team.LegalHold where

import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Misc
import qualified Data.Text as T
import Imports
import Wire.API.Provider
import Wire.API.User.Client.Prekey

data LegalHoldStatus = LegalHoldDisabled | LegalHoldEnabled
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSON LegalHoldStatus where
  toJSON LegalHoldEnabled = "enabled"
  toJSON LegalHoldDisabled = "disabled"

instance FromJSON LegalHoldStatus where
  parseJSON = withText "LegalHoldStatus" $ \case
    "enabled" -> pure LegalHoldEnabled
    "disabled" -> pure LegalHoldDisabled
    x -> fail $ "unexpected status type: " <> T.unpack x

data LegalHoldTeamConfig = LegalHoldTeamConfig
  { legalHoldTeamConfigStatus :: !LegalHoldStatus
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LegalHoldTeamConfig where
  toJSON s =
    object $
      "status" .= legalHoldTeamConfigStatus s
        # []

instance FromJSON LegalHoldTeamConfig where
  parseJSON = withObject "LegalHoldTeamConfig" $ \o ->
    LegalHoldTeamConfig <$> o .: "status"

-- | This type is analogous to 'NewService' for bots.
data NewLegalHoldService = NewLegalHoldService
  { newLegalHoldServiceUrl :: !HttpsUrl,
    newLegalHoldServiceKey :: !ServiceKeyPEM,
    newLegalHoldServiceToken :: !ServiceToken
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

data LegalHoldService = LegalHoldService
  { legalHoldServiceTeam :: !TeamId,
    legalHoldServiceUrl :: !HttpsUrl,
    legalHoldServiceFingerprint :: !(Fingerprint Rsa),
    legalHoldServiceToken :: !ServiceToken,
    legalHoldServiceKey :: !ServiceKey
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LegalHoldService where
  toJSON s =
    object $
      "team_id" .= legalHoldServiceTeam s
        # "base_url" .= legalHoldServiceUrl s
        # "fingerprint" .= legalHoldServiceFingerprint s
        # "auth_token" .= legalHoldServiceToken s
        # "public_key" .= legalHoldServiceKey s
        # []

instance FromJSON LegalHoldService where
  parseJSON = withObject "LegalHoldService" $ \o ->
    LegalHoldService
      <$> o .: "team_id"
      <*> o .: "base_url"
      <*> o .: "fingerprint"
      <*> o .: "auth_token"
      <*> o .: "public_key"

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
  { viewLegalHoldServiceTeam :: !TeamId,
    viewLegalHoldServiceUrl :: !HttpsUrl,
    viewLegalHoldServiceFingerprint :: !(Fingerprint Rsa),
    viewLegalHoldServiceAuthToken :: !ServiceToken,
    viewLegalHoldServiceKey :: !ServiceKeyPEM
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

legalHoldService :: TeamId -> Fingerprint Rsa -> NewLegalHoldService -> ServiceKey -> LegalHoldService
legalHoldService tid fpr (NewLegalHoldService u _ t) k = LegalHoldService tid u fpr t k

viewLegalHoldService :: LegalHoldService -> ViewLegalHoldService
viewLegalHoldService (LegalHoldService tid u fpr t k) =
  ViewLegalHoldService $ ViewLegalHoldServiceInfo tid u fpr t (serviceKeyPEM k)

-- This is the payload that the LH service returns upon calling @/initiate@
data NewLegalHoldClient = NewLegalHoldClient
  { newLegalHoldClientPrekeys :: [Prekey],
    newLegalHoldClientLastKey :: !LastPrekey
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON NewLegalHoldClient where
  toJSON c =
    object $
      "prekeys" .= newLegalHoldClientPrekeys c
        # "last_prekey" .= newLegalHoldClientLastKey c
        # []

instance FromJSON NewLegalHoldClient where
  parseJSON = withObject "NewLegalHoldClient" $ \o ->
    NewLegalHoldClient <$> o .: "prekeys"
      <*> o .: "last_prekey"

-- This is the payload that the LH service expects
data RequestNewLegalHoldClient = RequestNewLegalHoldClient
  { userId :: !UserId,
    teamId :: !TeamId
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RequestNewLegalHoldClient where
  toJSON (RequestNewLegalHoldClient userId teamId) =
    object $
      "user_id" .= userId
        # "team_id" .= teamId
        # []

instance FromJSON RequestNewLegalHoldClient where
  parseJSON = withObject "RequestNewLegalHoldClient" $ \o ->
    RequestNewLegalHoldClient <$> o .: "user_id"
      <*> o .: "team_id"

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

data LegalHoldClientRequest = LegalHoldClientRequest
  { lhcrRequester :: !UserId,
    lhcrLastPrekey :: !LastPrekey
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON LegalHoldClientRequest where
  parseJSON = withObject "LegalHoldClientRequest" $ \o ->
    LegalHoldClientRequest
      <$> o .: "requester"
      <*> o .: "last_prekey"

instance ToJSON LegalHoldClientRequest where
  toJSON (LegalHoldClientRequest requester lastPrekey') =
    object $
      "requester" .= requester
        # "last_prekey" .= lastPrekey'
        # []

-- Request body definition for the @/confirm@ endpoint on the LegalHold Service
data LegalHoldServiceConfirm = LegalHoldServiceConfirm
  { lhcClientId :: !ClientId,
    lhcUserId :: !UserId,
    lhcTeamId :: !TeamId,
    -- | Replace with Legal Hold Token Type
    lhcRefreshToken :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LegalHoldServiceConfirm where
  toJSON (LegalHoldServiceConfirm clientId userId teamId refreshToken) =
    object $
      "client_id" .= clientId
        # "user_id" .= userId
        # "team_id" .= teamId
        # "refresh_token" .= refreshToken
        # []

instance FromJSON LegalHoldServiceConfirm where
  parseJSON = withObject "LegalHoldServiceConfirm" $ \o ->
    LegalHoldServiceConfirm
      <$> o .: "client_id"
      <*> o .: "user_id"
      <*> o .: "team_id"
      <*> o .: "refresh_token"

data LegalHoldServiceRemove = LegalHoldServiceRemove
  { lhrUserId :: !UserId,
    lhrTeamId :: !TeamId
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LegalHoldServiceRemove where
  toJSON (LegalHoldServiceRemove userId teamId) =
    object $
      "user_id" .= userId
        # "team_id" .= teamId
        # []

data RemoveLegalHoldSettingsRequest = RemoveLegalHoldSettingsRequest
  { rmlhsrPassword :: !(Maybe PlainTextPassword)
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

data DisableLegalHoldForUserRequest = DisableLegalHoldForUserRequest
  { dlhfuPassword :: !(Maybe PlainTextPassword)
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

data ApproveLegalHoldForUserRequest = ApproveLegalHoldForUserRequest
  { alhfuPassword :: !(Maybe PlainTextPassword)
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
