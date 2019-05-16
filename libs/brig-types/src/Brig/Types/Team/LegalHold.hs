{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Team.LegalHold where

import Imports
import Brig.Types.Provider
import Data.Aeson
import Data.Id

-- | This type is analogous to 'NewService' for bots.
data NewLegalHoldService = NewLegalHoldService
    { newLegalHoldServiceUrl     :: !HttpsUrl
    , newLegalHoldServiceKey     :: !ServiceKeyPEM
    , newLegalHoldServiceToken   :: !ServiceToken
    }
  deriving (Eq, Show)

instance ToJSON NewLegalHoldService where
    toJSON = undefined

instance FromJSON NewLegalHoldService where
    parseJSON = undefined

type LegalHoldId = Id "legalhold"

data LegalHoldService = LegalHoldService
    { legalHoldServiceUrl   :: !HttpsUrl
    , legalHoldServiceKey   :: !ServiceKeyPEM
    , legalHoldServiceToken :: !ServiceToken
    }
  deriving (Eq, Show)

instance ToJSON LegalHoldService where
    toJSON = undefined

instance FromJSON LegalHoldService where
    parseJSON = undefined


-- | this is what the team members can see when they get status info.  (TODO: is this right?
-- either way make it at least a newtype.)
type ViewLegalHoldService = LegalHoldService
