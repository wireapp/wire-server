{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brig.Types.Team.LegalHold where

import Imports
import Brig.Types.Provider
import Brig.Types.Client.Prekey
import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.Misc
import qualified Data.Text as T

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
    toJSON s = object
        $ "status" .= legalHoldTeamConfigStatus s
        # []

instance FromJSON LegalHoldTeamConfig where
    parseJSON = withObject "LegalHoldTeamConfig" $ \o ->
        LegalHoldTeamConfig <$> o .: "status"

-- | This type is analogous to 'NewService' for bots.
data NewLegalHoldService = NewLegalHoldService
    { newLegalHoldServiceUrl     :: !HttpsUrl
    , newLegalHoldServiceKey     :: !ServiceKeyPEM
    , newLegalHoldServiceToken   :: !ServiceToken
    }
  deriving stock (Eq, Show, Generic)

instance ToJSON NewLegalHoldService where
    toJSON s = object
        $ "base_url"    .= newLegalHoldServiceUrl s
        # "public_key"  .= newLegalHoldServiceKey s
        # "auth_token"  .= newLegalHoldServiceToken s
        # []

instance FromJSON NewLegalHoldService where
    parseJSON = withObject "NewLegalHoldService" $ \o -> NewLegalHoldService
                   <$> o .: "base_url"
                   <*> o .: "public_key"
                   <*> o .: "auth_token"

data LegalHoldService = LegalHoldService
    { legalHoldServiceTeam        :: !TeamId
    , legalHoldServiceUrl         :: !HttpsUrl
    , legalHoldServiceFingerprint :: !(Fingerprint Rsa)
    , legalHoldServiceToken       :: !ServiceToken
    }
  deriving stock (Eq, Show, Generic)

instance ToJSON LegalHoldService where
    toJSON s = object
        $ "team_id"     .= legalHoldServiceTeam s
        # "base_url"    .= legalHoldServiceUrl s
        # "fingerprint" .= legalHoldServiceFingerprint s
        # "auth_token"  .= legalHoldServiceToken s
        # []

instance FromJSON LegalHoldService where
    parseJSON = withObject "LegalHoldService" $ \o -> LegalHoldService
                   <$> o .: "team_id"
                   <*> o .: "base_url"
                   <*> o .: "fingerprint"
                   <*> o .: "auth_token"

data ViewLegalHoldService
    = ViewLegalHoldService ViewLegalHoldServiceInfo
    | ViewLegalHoldServiceNotConfigured
    | ViewLegalHoldServiceDisabled
  deriving stock (Eq, Show, Generic)

instance ToJSON ViewLegalHoldService where
    toJSON s = case s of
        ViewLegalHoldService info -> object
            $ "status" .= String "configured"
            # "info"   .= info
            # []
        ViewLegalHoldServiceNotConfigured -> object
            $ "status" .= String "not_configured"
            # []
        ViewLegalHoldServiceDisabled -> object
            $ "status" .= String "disabled"
            # []

instance FromJSON ViewLegalHoldService where
    parseJSON = withObject "LegalHoldService" $ \o -> do
        status :: Text <- o .: "status"
        case status of
            "configured"     -> ViewLegalHoldService <$> (o .: "info")
            "not_configured" -> pure ViewLegalHoldServiceNotConfigured
            "disabled"       -> pure ViewLegalHoldServiceDisabled
            _ -> fail "status (one of configured, not_configured, disabled)"

data ViewLegalHoldServiceInfo
    = ViewLegalHoldServiceInfo
        { viewLegalHoldServiceTeam        :: !TeamId
        , viewLegalHoldServiceUrl         :: !HttpsUrl
        , viewLegalHoldServiceFingerprint :: !(Fingerprint Rsa)
        }
  deriving stock (Eq, Show, Generic)

instance ToJSON ViewLegalHoldServiceInfo where
    toJSON info = object
        $ "team_id"     .= viewLegalHoldServiceTeam info
        # "base_url"    .= viewLegalHoldServiceUrl info
        # "fingerprint" .= viewLegalHoldServiceFingerprint info
        # []

instance FromJSON ViewLegalHoldServiceInfo where
    parseJSON = withObject "LegalHoldServiceInfo" $ \o -> do
        ViewLegalHoldServiceInfo
            <$> o .: "team_id"
            <*> o .: "base_url"
            <*> o .: "fingerprint"

legalHoldService :: TeamId -> Fingerprint Rsa -> NewLegalHoldService -> LegalHoldService
legalHoldService tid fpr (NewLegalHoldService u _ t) = LegalHoldService tid u fpr t

viewLegalHoldService :: LegalHoldService -> ViewLegalHoldService
viewLegalHoldService (LegalHoldService tid u fpr _) =
    ViewLegalHoldService $ ViewLegalHoldServiceInfo tid u fpr

-- TODO: Do we need a Client ID?
data NewLegalHoldClient = NewLegalHoldClient
    { newLegalHoldClientPrekeys  :: [Prekey]
    , newLegalHoldClientLastKey  :: !LastPrekey
    , newLegalHoldClientFingerprint :: Fingerprint HumanReadable
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON NewLegalHoldClient where
    toJSON c = object
        $ "prekeys"  .= newLegalHoldClientPrekeys c
        # "last_prekey"  .= newLegalHoldClientLastKey c
        # "fingerprint"  .= newLegalHoldClientFingerprint c
        # []

instance FromJSON NewLegalHoldClient where
    parseJSON = withObject "NewLegalHoldClient" $ \o ->
        NewLegalHoldClient <$> o .:  "prekeys"
                           <*> o .:  "last_prekey"
                           <*> o .:  "fingerprint"

data RequestNewLegalHoldClient = RequestNewLegalHoldClient
    { userId :: !UserId
    , teamId :: !TeamId
    } deriving stock (Show, Eq, Generic)

instance ToJSON RequestNewLegalHoldClient where
    toJSON (RequestNewLegalHoldClient userId teamId) = object
        $ "user_id"    .= userId
        # "team_id"    .= teamId
        # []

instance FromJSON RequestNewLegalHoldClient where
    parseJSON = withObject "RequestNewLegalHoldClient" $ \o ->
        RequestNewLegalHoldClient <$> o .: "user_id"
                                  <*> o .: "team_id"

data UserLegalHoldStatusResponse =
    UserLegalHoldStatusResponse
      { ulhsrStatus      :: UserLegalHoldStatus
      , ulhsrFingerprint :: Maybe (Fingerprint HumanReadable)
      }
   deriving stock (Eq, Show, Generic)

instance ToJSON UserLegalHoldStatusResponse where
    toJSON (UserLegalHoldStatusResponse status fingerprint) = object
        $  "status"      .= status
        #  "fingerprint" .= fingerprint
        # []

instance FromJSON UserLegalHoldStatusResponse where
    parseJSON = withObject "UserLegalHoldStatusResponse" $ \o ->
        UserLegalHoldStatusResponse <$> o .: "status"
                                    <*> o .: "fingerprint"

data UserLegalHoldStatus
    = UserLegalHoldEnabled
    | UserLegalHoldPending
    | UserLegalHoldDisabled
    deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON UserLegalHoldStatus where
    toJSON UserLegalHoldEnabled = "enabled"
    toJSON UserLegalHoldPending = "pending"
    toJSON UserLegalHoldDisabled = "disabled"

instance FromJSON UserLegalHoldStatus where
    parseJSON = withText "LegalHoldStatus" $ \case
      "enabled" -> pure UserLegalHoldEnabled
      "pending" -> pure UserLegalHoldPending
      "disabled" -> pure UserLegalHoldDisabled
      x -> fail $ "unexpected status type: " <> T.unpack x

data LegalHoldClientRequest =
    LegalHoldClientRequest
    { lhcrRequester  :: !UserId
    , lhcrTargetUser :: !UserId
    , lhcrLastPrekey :: !LastPrekey
    , lhcrPrekeys    :: ![Prekey]
    } deriving stock (Eq, Show, Generic)

instance FromJSON LegalHoldClientRequest where
  parseJSON = withObject "LegalHoldClientRequest" $ \o ->
    LegalHoldClientRequest
        <$> o .: "requester"
        <*> o .: "target_user"
        <*> o .: "last_prekey"
        <*> o .: "prekeys"

instance ToJSON LegalHoldClientRequest where
  toJSON (LegalHoldClientRequest requester targetUser lastPrekey' prekeys) = object
        $  "requester" .= requester
        #  "target_user" .= targetUser
        #  "last_prekey" .= lastPrekey'
        #  "prekeys" .= prekeys
        # []

-- Request body definition for the @/confirm@ endpoint on the LegalHold Service
data LegalHoldServiceConfirm =
    LegalHoldServiceConfirm
    { lhcClientId     :: !ClientId
    , lhcUserId       :: !UserId
    , lhcTeamId       :: !TeamId
    , lhcRefreshToken :: !Text -- ^ Replace with Legal Hold Token Type
    } deriving stock (Eq, Show, Generic)

instance ToJSON LegalHoldServiceConfirm where
  toJSON (LegalHoldServiceConfirm clientId userId teamId refreshToken) = object
        $  "client_id" .= clientId
        #  "user_id" .= userId
        #  "team_id" .= teamId
        #  "refresh_token" .= refreshToken
        # []
