{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Brig.InternalEvent.Types
    ( InternalNotification(..)
    ) where

import BasePrelude

import Data.Aeson
import Data.Id

data InternalNotification
    = DeleteUser !UserId
    deriving (Eq, Show)

data InternalNotificationType
    = UserDeletion
    deriving (Eq, Show)

instance FromJSON InternalNotificationType where
    parseJSON "user.delete" = return UserDeletion
    parseJSON x             = fail $ "InternalNotificationType: Unknown type " <> show x

instance ToJSON InternalNotificationType where
    toJSON UserDeletion = "user.delete"

instance FromJSON InternalNotification where
    parseJSON = withObject "InternalNotification" $ \o -> do
        t <- o .: "type"
        case (t :: InternalNotificationType) of
            UserDeletion -> DeleteUser <$> o .: "user"

instance ToJSON InternalNotification where
    toJSON (DeleteUser u) = object
        [ "user" .= u
        , "type" .= UserDeletion
        ]
