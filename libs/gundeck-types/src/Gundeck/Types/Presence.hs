{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Gundeck.Types.Presence
    ( module Gundeck.Types.Presence
    , module Common
    ) where

import Data.Aeson
import Data.Id
import Data.Text
import Data.Word
import Gundeck.Types.Common as Common

import qualified Data.ByteString.Lazy as Lazy

newtype Milliseconds = Ms
    { ms :: Word64
    } deriving (Eq, Ord, Show, Num, ToJSON, FromJSON)

data Presence = Presence
    { userId     :: !UserId
    , connId     :: !ConnId
    , resource   :: !URI
    , resourceb  :: !(Maybe URI)
    , cannonhost :: !(Maybe Text)
    , clientId   :: !(Maybe ClientId)
    , createdAt  :: !Milliseconds
    , __field    :: !Lazy.ByteString -- temp. addition to ease migration
    } deriving (Eq, Show)

instance ToJSON Presence where
    toJSON p = object
        [ "user_id"    .= userId p
        , "device_id"  .= connId p
        , "resource"   .= resource p
        , "resourceb"  .= resourceb p
        , "cannon_host" .= cannonhost p
        , "client_id"  .= clientId p
        , "created_at" .= createdAt p
        ]

instance FromJSON Presence where
    parseJSON = withObject "Presence" $ \o ->
        Presence
            <$> o .:  "user_id"
            <*> o .:  "device_id"
            <*> o .:  "resource"
            <*> o .:? "resourceb"
            <*> o .:? "cannon_host"
            <*> o .:? "client_id"
            <*> o .:? "created_at" .!= 0
            <*> pure ""
