module Brig.Types.Client.LegalHold
    ( LegalHoldClientRequest(..)
    ) where

import Imports
import Data.Id
import Data.Aeson
import Data.Json.Util
import Brig.Types.Client.Prekey

data LegalHoldClientRequest =
    LegalHoldClientRequest
    { lhcrRequester  :: !UserId
    , lhcrTargetUser :: !UserId
    , lhcrLastPrekey :: !LastPrekey
    , lhcrPrekeys    :: ![Prekey]
    }

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

