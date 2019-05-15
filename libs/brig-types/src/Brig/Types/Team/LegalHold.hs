{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Team.LegalHold where

import Imports
import Brig.Types.Provider
import Data.Aeson

-- | This type is analogous to 'NewService' for bots.
data NewLegalHoldService = NewLegalHoldService
    { newLegalHoldServiceUrl     :: !HttpsUrl
    , newLegalHoldServiceKey     :: !ServiceKeyPEM
    , newLegalHoldServiceToken   :: !(Maybe ServiceToken)
    }

instance ToJSON NewLegalHoldService where
    toJSON = undefined

instance FromJSON NewLegalHoldService where
    parseJSON = undefined
