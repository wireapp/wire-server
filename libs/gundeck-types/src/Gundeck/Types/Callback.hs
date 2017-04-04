{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Legacy:
module Gundeck.Types.Callback
    ( Callback        (..)
    , CallbackTrigger (..)
    , ServiceName     (..)
    , module C
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Id
import Data.Text (Text)
import Gundeck.Types.Common as C


data ServiceName = Belfry deriving (Eq, Show)

instance FromJSON ServiceName where
    parseJSON (String "belfry") = return Belfry
    parseJSON x = fail $ "No service-id: " ++ show (encode x)

instance ToJSON ServiceName where
    toJSON Belfry = String "belfry"

data CallbackTrigger = CallbackTrigger
    { cbtUserId :: !UserId
    , cbtConnId :: !ConnId
    }

instance FromJSON CallbackTrigger where
    parseJSON = withObject "callback-trigger object" $ \o ->
        CallbackTrigger <$> o .: "user_id"
                        <*> o .: "conn_id"

instance ToJSON CallbackTrigger where
    toJSON CallbackTrigger{..} = object
        [ "user_id" .= cbtUserId
        , "conn_id" .= cbtConnId
        ]

data Callback = Callback
    { _cb_user_id    :: !UserId
    , _cb_device_id  :: !ConnId
    , _cb_service    :: !ServiceName
    , _cb_path       :: !Text
    } deriving (Eq, Show)

deriveJSON defaultOptions
    { fieldLabelModifier = drop (length ("_cb_" :: String))
    } ''Callback
