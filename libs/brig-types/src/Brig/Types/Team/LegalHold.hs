{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brig.Types.Team.LegalHold where

import Imports
import Brig.Types.Provider
import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.Misc

data LegalHoldTeamConfig = LegalHoldTeamConfig
    { legalHoldTeamConfigEnabled :: !Bool
    }
  deriving (Eq, Show)

instance ToJSON LegalHoldTeamConfig where
    toJSON s = object
        $ "enabled" .= legalHoldTeamConfigEnabled s
        # []

instance FromJSON LegalHoldTeamConfig where
    parseJSON = withObject "LegalHoldTeamConfig" $ \o ->
        LegalHoldTeamConfig <$> o .: "enabled"

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

data ViewLegalHoldService
    = ViewLegalHoldService ViewLegalHoldServiceInfo
    | ViewLegalHoldServiceNotConfigured
    | ViewLegalHoldServiceDisabled
  deriving (Eq, Show)

data ViewLegalHoldServiceInfo
    = ViewLegalHoldServiceInfo
        { viewLegalHoldServiceTeam        :: !TeamId
        , viewLegalHoldServiceUrl         :: !HttpsUrl
        , viewLegalHoldServiceFingerprint :: !(Fingerprint Rsa)
        }
  deriving (Eq, Show)

instance ToJSON ViewLegalHoldService where
    toJSON s = case s of
        ViewLegalHoldService info -> object
            $ "status"      .= String "configured"
            # "team_id"     .= viewLegalHoldServiceTeam info
            # "base_url"    .= viewLegalHoldServiceUrl info
            # "fingerprint" .= viewLegalHoldServiceFingerprint info
            # []
        ViewLegalHoldServiceNotConfigured -> object
            $ "status"      .= String "not_configured"
            # []
        ViewLegalHoldServiceDisabled -> object
            $ "status"      .= String "disabled"
            # []

instance FromJSON ViewLegalHoldService where
    parseJSON = withObject "LegalHoldService" $ \o -> do
        status :: Text <- o .: "status"
        case status of
            "configured" -> ViewLegalHoldService <$> (ViewLegalHoldServiceInfo
                <$> o .: "team_id"
                <*> o .: "base_url"
                <*> o .: "fingerprint")
            "not_configured" -> pure ViewLegalHoldServiceNotConfigured
            "disabled" -> pure ViewLegalHoldServiceDisabled
            _ -> fail "status (one of configured, not_configured, disabled)"


legalHoldService :: TeamId -> Fingerprint Rsa -> NewLegalHoldService -> LegalHoldService
legalHoldService tid fpr (NewLegalHoldService u _ t) = LegalHoldService tid u fpr t

viewLegalHoldService :: LegalHoldService -> ViewLegalHoldService
viewLegalHoldService (LegalHoldService tid u fpr _) =
    ViewLegalHoldService $ ViewLegalHoldServiceInfo tid u fpr
