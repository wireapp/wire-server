{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

-- TODO: do we need all these extensions?
-- TODO: add license headers

module Wire.API.Federation.Types.Event
  ( AnyEvent,
    Event (..),
    AnyEventData (..),
    MemberJoin (..),
    SimpleMember (..),
    ConversationRole (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Qualified (Qualified)
import Data.Time
import Galley.Types ()
import Galley.Types.Conversations.Roles ()
import Imports
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary (arbitrary))
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
  deriving stock (Show, Generic, Foldable, Functor, Traversable)
  deriving (ToJSON, FromJSON) via (CustomEncoded (Event a))

-- FUTUREWORK(federation): Extend with the other Event types that need to be
-- sent across backends.
data AnyEventData
  = DataMemberJoin MemberJoin
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded AnyEventData)

-- TODO when putting this into Event, we don't get a tag, making the representation
-- different to the one wrapped into AnyEventData
newtype MemberJoin
  = MemberJoin
      { smUsers :: [SimpleMember]
      }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MemberJoin)

data SimpleMember
  = SimpleMember
      { smId :: Qualified UserId,
        smConversationRole :: ConversationRole
      }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded SimpleMember)

data ConversationRole
  = ConversationRoleAdmin
  | ConversationRoleMember
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationRole)

-- Arbitrary

instance Arbitrary a => Arbitrary (Event a) where
  arbitrary = Event <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary MemberJoin where
  arbitrary = MemberJoin <$> arbitrary

instance Arbitrary SimpleMember where
  arbitrary = SimpleMember <$> arbitrary <*> arbitrary

instance Arbitrary ConversationRole where
  arbitrary = QC.elements [ConversationRoleAdmin, ConversationRoleMember]
