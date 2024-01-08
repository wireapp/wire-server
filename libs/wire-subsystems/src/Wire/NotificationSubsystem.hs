{-# LANGUAGE TemplateHaskell #-}

module Wire.NotificationSubsystem where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
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

data PushTo = PushTo
  { _pushConn :: Maybe ConnId,
    _pushTransient :: Bool,
    _pushRoute :: Route,
    _pushNativePriority :: Maybe Priority,
    pushOrigin :: Maybe UserId,
    _pushRecipients :: NonEmpty Recipient,
    pushJson :: Object
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving (Arbitrary) via GenericUniform PushTo

makeLenses ''PushTo

type PushToUser = PushTo

data NotificationSubsystem m a where
  Push :: [PushToUser] -> NotificationSubsystem m ()
  PushSlowly :: [PushToUser] -> NotificationSubsystem m ()

makeSem ''NotificationSubsystem

newPush1 :: Maybe UserId -> Object -> NonEmpty Recipient -> PushToUser
newPush1 from e rr =
  PushTo
    { _pushConn = Nothing,
      _pushTransient = False,
      _pushRoute = RouteAny,
      _pushNativePriority = Nothing,
      pushJson = e,
      pushOrigin = from,
      _pushRecipients = rr
    }

newPush :: Maybe UserId -> Object -> [Recipient] -> Maybe PushToUser
newPush _ _ [] = Nothing
newPush u e (r : rr) = Just $ newPush1 u e (r :| rr)

newPushLocal :: UserId -> Object -> [Recipient] -> Maybe PushToUser
newPushLocal uid = newPush (Just uid)

newPushLocal1 :: UserId -> Object -> NonEmpty Recipient -> PushToUser
newPushLocal1 uid = newPush1 (Just uid)
