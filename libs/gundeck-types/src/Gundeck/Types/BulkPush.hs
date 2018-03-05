{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gundeck.Types.BulkPush where

import Data.Aeson
import Data.Id
import GHC.Generics
import Gundeck.Types.Notification


type PushTarget = (UserId, ConnId)

-- | A lazy bytestring as needed for constructing a binary
-- <https://hackage.haskell.org/package/websockets-0.12.3.1/docs/Network-WebSockets.html#t:DataMessage>.
data BulkPushRequest = BulkPush
    { bpBody :: ![(Notification, [PushTarget])]
    } deriving ( Show
               , Generic
               )

instance FromJSON BulkPushRequest
instance ToJSON BulkPushRequest

data BulkPushResponse = PushResponse
    { bprespBody :: ![(PushTarget, Int)]  -- ^ (Morally the 'Int' is 'Status' from http-types, but
                                          -- that would require an orphan instance and the extra
                                          -- package dependency.)
    } deriving ( Show
               , Generic
               )

instance FromJSON BulkPushResponse
instance ToJSON BulkPushResponse
