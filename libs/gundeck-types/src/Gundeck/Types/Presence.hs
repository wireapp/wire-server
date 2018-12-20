{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Gundeck.Types.Presence
    ( module Gundeck.Types.Presence
    , module Common
    ) where

import Imports
import Data.Aeson
import Data.Id
import Data.Misc (Milliseconds)
import Gundeck.Types.Common as Common

import qualified Data.ByteString.Lazy as Lazy

-- | This is created in gundeck by cannon every time the client opens a new websocket connection.
-- (That's why we always have a 'ConnId' from the most recent connection by that client.)
data Presence = Presence
    { userId    :: !UserId
    , connId    :: !ConnId
    , resource  :: !URI  -- ^ cannon instance hosting the presence
    , clientId  :: !(Maybe ClientId)  -- ^ This is 'Nothing' if either (a) the presence is older
                                      -- than mandatory end-to-end encryption, or (b) the client is
                                      -- operating the team settings pages without the need for
                                      -- end-to-end crypto.
    , createdAt :: !Milliseconds
    , __field   :: !Lazy.ByteString -- ^ REFACTOR: temp. addition to ease migration
    } deriving (Eq, Show)

instance ToJSON Presence where
    toJSON p = object
        [ "user_id"    .= userId p
        , "device_id"  .= connId p
        , "resource"   .= resource p
        , "client_id"  .= clientId p
        , "created_at" .= createdAt p
        ]

instance FromJSON Presence where
    parseJSON = withObject "Presence" $ \o ->
        Presence
            <$> o .:  "user_id"
            <*> o .:  "device_id"
            <*> o .:  "resource"
            <*> o .:? "client_id"
            <*> o .:? "created_at" .!= 0
            <*> pure ""
