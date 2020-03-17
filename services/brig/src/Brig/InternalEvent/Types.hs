module Brig.InternalEvent.Types
  ( InternalNotification (..),
  )
where

import BasePrelude
import Data.Aeson
import Data.Id
import Imports

data InternalNotification
  = -- | see 'onEvent'.
    --
    -- UserDeleted event to contacts
    -- via galley: MemberLeave EdMembersLeave event to members for all conversations the user was in
    DeleteUser !N !E !UserId
  | DeleteService !ProviderId !ServiceId
  deriving (Eq, Show)

data InternalNotificationType
  = UserDeletion
  | ServiceDeletion
  deriving (Eq, Show)

instance FromJSON InternalNotificationType where
  parseJSON = \case
    "user.delete" -> return UserDeletion
    "service.delete" -> return ServiceDeletion
    x -> fail $ "InternalNotificationType: Unknown type " <> show x

instance ToJSON InternalNotificationType where
  toJSON UserDeletion = "user.delete"
  toJSON ServiceDeletion = "service.delete"

instance FromJSON InternalNotification where
  parseJSON = withObject "InternalNotification" $ \o -> do
    t <- o .: "type"
    case (t :: InternalNotificationType) of
      UserDeletion -> DeleteUser N E <$> o .: "user"
      ServiceDeletion -> DeleteService <$> o .: "provider" <*> o .: "service"

instance ToJSON InternalNotification where
  toJSON (DeleteUser N E uid) =
    object
      [ "user" .= uid,
        "type" .= UserDeletion
      ]
  toJSON (DeleteService pid sid) =
    object
      [ "provider" .= pid,
        "service" .= sid,
        "type" .= ServiceDeletion
      ]
