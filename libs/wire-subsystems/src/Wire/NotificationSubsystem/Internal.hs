{-# LANGUAGE TemplateHaskell #-}

module Wire.NotificationSubsystem.Internal where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Gundeck.Types hiding (Push (..), Recipient, newPush)
import Imports
import Polysemy
import Wire.Arbitrary

data Recipient = Recipient
  { _recipientUserId :: UserId,
    _recipientClients :: RecipientClients
  }
  deriving stock (Show, Ord, Eq, Generic)
  deriving (Arbitrary) via GenericUniform Recipient

makeLenses ''Recipient

data Push = Push
  { _pushConn :: Maybe ConnId,
    _pushTransient :: Bool,
    _pushRoute :: Route,
    _pushNativePriority :: Maybe Priority,
    pushOrigin :: Maybe UserId,
    _pushRecipients :: NonEmpty Recipient,
    pushJson :: Object,
    _pushApsData :: Maybe ApsData
  }
  deriving stock (Eq, Generic, Show)
  deriving (Arbitrary) via GenericUniform Push

makeLenses ''Push

data NotificationSubsystem m a where
  PushNotifications :: [Push] -> NotificationSubsystem m ()
  PushNotificationsSlowly :: [Push] -> NotificationSubsystem m ()
  UserDeleted :: UserId -> NotificationSubsystem m ()
  UnregisterPushClient :: UserId -> ClientId -> NotificationSubsystem m ()
  GetPushTokens :: UserId -> NotificationSubsystem m [PushToken]

makeSem ''NotificationSubsystem
