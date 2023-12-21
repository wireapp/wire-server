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
import Wire.Sem.Delay (Delay, delay)

data RecipientBy user = Recipient
  { _recipientUserId :: user,
    _recipientClients :: RecipientClients
  }
  deriving stock (Functor, Foldable, Traversable, Show, Ord, Eq, Generic)
  deriving (Arbitrary) via GenericUniform (RecipientBy user)

makeLenses ''RecipientBy

type Recipient = RecipientBy UserId

data PushTo user = PushTo
  { _pushConn :: Maybe ConnId,
    _pushTransient :: Bool,
    _pushRoute :: Route,
    _pushNativePriority :: Maybe Priority,
    pushOrigin :: Maybe UserId,
    _pushRecipients :: NonEmpty (RecipientBy user),
    pushJson :: Object
  }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable, Show)
  deriving (Arbitrary) via GenericUniform (PushTo user)

makeLenses ''PushTo

type PushToUser = PushTo UserId

data NotificationSubsystem m a where
  Push :: [PushToUser] -> NotificationSubsystem m ()

makeSem ''NotificationSubsystem

-- TODO: Test
pushSlowly ::
  ( Member NotificationSubsystem r,
    Member Delay r
  ) =>
  [PushToUser] ->
  Sem r ()
pushSlowly ps = do
  -- TODO this comes from the app configuration
  let mmillies = 10000
      d = 1000 * mmillies
  for_ ps \p -> do
    delay d
    push [p]

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
