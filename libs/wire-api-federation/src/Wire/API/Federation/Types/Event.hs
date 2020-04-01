{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Wire.API.Federation.Types.Event
  ( AnyEvent,
    Event (..),
    AnyEventData (..),
    MemberJoin (..),
    SimpleMember (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Qualified (Qualified)
import Data.Time
import Galley.Types ()
import Galley.Types.Conversations.Roles ()
import Imports
import Wire.API.Federation.Util.Aeson (CustomEncoded (CustomEncoded))

-- | Similar to 'Galley.Types.Event', but all IDs are qualified, to allow this
-- representation to be sent across backends.
type AnyEvent = Event AnyEventData

-- | Instead of just being a generic event, it also allows to specify which type
-- of event it is, e.g. @Event MemberJoin@.
data Event a
  = Event
      { eventConversation :: Qualified ConvId,
        eventFrom :: Qualified UserId,
        eventTime :: UTCTime,
        eventData :: a
      }
  deriving stock (Foldable, Functor, Show, Generic)

-- FUTUREWORK(federation): Extend with the other Event types that need to be
-- sent across backends.
data AnyEventData
  = DataMemberJoin MemberJoin
  deriving stock (Show, Generic)

newtype MemberJoin
  = MemberJoin
      { smUsers :: [SimpleMember]
      }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MemberJoin)

data SimpleMember
  = SimpleMember
      { smId :: Qualified UserId,
        smConversationRole :: RoleName
      }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded SimpleMember)

newtype RoleName
  = RoleName {roleNameText :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)
