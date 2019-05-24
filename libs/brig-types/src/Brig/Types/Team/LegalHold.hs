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
   deriving (Eq, Show, Ord, Enum, Bounded)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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

data ViewLegalHoldService = ViewLegalHoldService
    { viewLegalHoldServiceTeam        :: !TeamId
    , viewLegalHoldServiceUrl         :: !HttpsUrl
    , viewLegalHoldServiceFingerprint :: !(Fingerprint Rsa)
    }
  deriving (Eq, Show)

instance ToJSON ViewLegalHoldService where
    toJSON s = object
        $ "team_id"     .= viewLegalHoldServiceTeam s
        # "base_url"    .= viewLegalHoldServiceUrl s
        # "fingerprint" .= viewLegalHoldServiceFingerprint s
        # []

instance FromJSON ViewLegalHoldService where
    parseJSON = withObject "LegalHoldService" $ \o -> ViewLegalHoldService
                   <$> o .: "team_id"
                   <*> o .: "base_url"
                   <*> o .: "fingerprint"


legalHoldService :: TeamId -> Fingerprint Rsa -> NewLegalHoldService -> LegalHoldService
legalHoldService tid fpr (NewLegalHoldService u _ t) = LegalHoldService tid u fpr t

viewLegalHoldService :: LegalHoldService -> ViewLegalHoldService
viewLegalHoldService (LegalHoldService tid u fpr _) = ViewLegalHoldService tid u fpr

-- TODO: Do we need a Client ID?
data NewLegalHoldClient = NewLegalHoldClient
    { newLegalHoldClientPrekeys  :: [Prekey]
    , newLegalHoldClientLastKey  :: !LastPrekey
    }
    deriving stock (Eq, Show)

instance ToJSON NewLegalHoldClient where
    toJSON c = object
        $ "prekeys"  .= newLegalHoldClientPrekeys c
        -- TODO: Currently the LH Service uses 'last_prekey'; but internally we usually
        -- use lastkey
        -- TODO: Ask Dejan about the 'fingeprint' field of the 'initiate' response.
        -- What is it for? Should we be using it for something?
        # "lastkey"  .= newLegalHoldClientLastKey c
        # []

instance FromJSON NewLegalHoldClient where
    parseJSON = withObject "NewLegalHoldClient" $ \o ->
        NewLegalHoldClient <$> o .:  "prekeys"
                           <*> o .:  "lastkey"

-- Request to LH Service to initiate creation of a device
data InitiateRequest = InitiateRequest
    { userId :: !UserId
    , teamId :: !TeamId
    } deriving stock (Show, Eq)

instance ToJSON InitiateRequest where
    toJSON InitiateRequest{userId, teamId} = object
        $ "userId"    .= userId
        # "teamId"    .= teamId
        # []

