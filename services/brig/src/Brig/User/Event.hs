{-# LANGUAGE RecordWildCards #-}

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

module Brig.User.Event where

import Brig.Types
import Data.Handle (Handle)
import Data.Id
import Imports

data Event
  = ConnectionEvent !ConnectionEvent
  | PropertyEvent !PropertyEvent
  | ClientEvent !ClientEvent

data UserCreated = UserCreated !User

-- | A user is activated when the first user identity (email address or phone number)
-- is verified. {#RefActivationEvent}
data UserActivated = UserActivated !User

-- | Account & API access of a user has been suspended.
data UserSuspended = UserSuspended !UserId

-- | Account & API access of a previously suspended user
-- has been restored.
data UserResumed = UserResumed !UserId

-- | The user account has been deleted.
data UserDeleted = UserDeleted !UserId

data UserNameUpdated = UserNameUpdated UserId Name

data UserHandleUpdated = UserHandleUpdated UserId Handle

data UserLocaleUpdated = UserLocaleUpdated UserId Locale

-- ...  (there were a few more updates, but they can be added when we actually do this.)

data UserIdentityUpdated = UserIdentityUpdated !UserIdentityUpdatedData

data UserIdentityRemoved = UserIdentityRemoved !UserIdentityRemovedData

data UserLegalHoldDisabled = UserLegalHoldDisabled !UserId

data UserLegalHoldEnabled = UserLegalHoldEnabled !UserId

data LegalHoldClientRequested = LegalHoldClientRequested LegalHoldClientRequestedData

data ConnectionEvent = ConnectionUpdated
  { ucConn :: !UserConnection,
    ucPrev :: !(Maybe Relation),
    ucName :: !(Maybe Name)
  }

data PropertyEvent
  = PropertySet !UserId !PropertyKey !PropertyValue
  | PropertyDeleted !UserId !PropertyKey
  | PropertiesCleared !UserId

data ClientEvent
  = ClientAdded !UserId !Client
  | ClientRemoved !UserId !Client

data UserIdentityUpdatedData = UserIdentityUpdatedData
  { eiuId :: !UserId,
    eiuEmail :: !(Maybe Email),
    eiuPhone :: !(Maybe Phone)
  }
  deriving stock (Show)

data UserIdentityRemovedData = UserIdentityRemovedData
  { eirId :: !UserId,
    eirEmail :: !(Maybe Email),
    eirPhone :: !(Maybe Phone)
  }
  deriving stock (Show)

data LegalHoldClientRequestedData = LegalHoldClientRequestedData
  { lhcTargetUser :: !UserId,
    lhcLastPrekey :: !LastPrekey,
    lhcClientId :: !ClientId
  }
  deriving stock (Show)

connEventUserId :: ConnectionEvent -> UserId
connEventUserId ConnectionUpdated {..} = ucFrom ucConn

class Typeable a => IsUserEvent a where
  userEventUserId :: a -> UserId

instance IsUserEvent UserCreated where
  userEventUserId = undefined

-- ...

propEventUserId :: PropertyEvent -> UserId
propEventUserId (PropertySet u _ _) = u
propEventUserId (PropertyDeleted u _) = u
propEventUserId (PropertiesCleared u) = u
