{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Federation.Event
  ( AnyEvent (..),
    ConversationEvent (..),

    -- * MemberJoin
    MemberJoin (..),
    SimpleMember (..),
    ConversationRole (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Qualified (Qualified)
import Data.Time
import Imports
import Test.QuickCheck (Arbitrary (arbitrary))
import qualified Test.QuickCheck as QC
import Wire.API.Federation.Util.Aeson (CustomEncoded (CustomEncoded))

-- | Similar to 'Galley.Types.Event', but all IDs are qualified, to allow this
-- representation to be sent across backends.
data AnyEvent
  = EventMemberJoin (ConversationEvent MemberJoin)
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded AnyEvent)

-- | Instead of just being a generic event, it also allows to specify which type
-- of event it is, e.g. @ConversationEvent MemberJoin@.
data ConversationEvent a = ConversationEvent
  { eventConversation :: Qualified ConvId,
    eventFrom :: Qualified UserId,
    eventTime :: UTCTime,
    eventData :: a
  }
  deriving stock (Eq, Show, Generic, Foldable, Functor, Traversable)
  deriving (ToJSON, FromJSON) via (CustomEncoded (ConversationEvent a))

newtype MemberJoin = MemberJoin
  { smUsers :: [SimpleMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded MemberJoin)

data SimpleMember = SimpleMember
  { smId :: Qualified UserId,
    smConversationRole :: ConversationRole
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded SimpleMember)

data ConversationRole
  = ConversationRoleAdmin
  | ConversationRoleMember
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (CustomEncoded ConversationRole)

-- Arbitrary

instance Arbitrary AnyEvent where
  arbitrary =
    QC.oneof
      [ EventMemberJoin <$> arbitrary
      ]

instance Arbitrary a => Arbitrary (ConversationEvent a) where
  arbitrary = ConversationEvent <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary MemberJoin where
  arbitrary = MemberJoin <$> arbitrary

instance Arbitrary SimpleMember where
  arbitrary = SimpleMember <$> arbitrary <*> arbitrary

instance Arbitrary ConversationRole where
  arbitrary = QC.elements [ConversationRoleAdmin, ConversationRoleMember]
