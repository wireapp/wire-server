{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brig.Types.Team.LegalHold where

import Imports
import Brig.Types.Provider
import Data.Aeson
import Data.Json.Util

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
    { legalHoldServiceUrl   :: !HttpsUrl
    , legalHoldServiceKey   :: !ServiceKeyPEM
    , legalHoldServiceToken :: !ServiceToken
    }
  deriving (Eq, Show)

instance ToJSON LegalHoldService where
    toJSON s = object
        $ "base_url"    .= legalHoldServiceUrl s
        # "public_key"  .= legalHoldServiceKey s
        # "auth_token"  .= legalHoldServiceToken s
        # []

instance FromJSON LegalHoldService where
    parseJSON = withObject "LegalHoldService" $ \o -> LegalHoldService
                   <$> o .: "base_url"
                   <*> o .: "public_key"
                   <*> o .: "auth_token"

-- | this is what the team members can see when they get status info.  (TODO: is this right?
-- either way make it at least a newtype.)
newtype ViewLegalHoldService = ViewLegalHoldService LegalHoldService
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)
