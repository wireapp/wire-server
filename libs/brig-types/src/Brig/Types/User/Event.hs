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

module Brig.Types.User.Event where

import Brig.Types
import Data.ByteString.Conversion
import Data.Handle (Handle)
import Data.Id
import Data.Qualified (Qualified (Qualified))
import Imports
import System.Logger.Class

data Event
  = UserEvent !UserEvent
  | ConnectionEvent !ConnectionEvent
  | PropertyEvent !PropertyEvent
  | ClientEvent !ClientEvent

data UserEvent
  = UserCreated !User
  | -- | A user is activated when the first user identity (email address or phone number)
    -- is verified. {#RefActivationEvent}
    UserActivated !User
  | -- | Account & API access of a user has been suspended.
    UserSuspended !UserId
  | -- | Account & API access of a previously suspended user
    -- has been restored.
    UserResumed !UserId
  | -- | The user account has been deleted.
    UserDeleted !UserId
  | UserUpdated !UserUpdatedData
  | UserIdentityUpdated !UserIdentityUpdatedData
  | UserIdentityRemoved !UserIdentityRemovedData
  | UserLegalHoldDisabled !UserId
  | UserLegalHoldEnabled !UserId
  | LegalHoldClientRequested LegalHoldClientRequestedData

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

data UserUpdatedData = UserUpdatedData
  { eupId :: !UserId,
    eupName :: !(Maybe Name),
    -- | DEPRECATED
    eupPict :: !(Maybe Pict),
    eupAccentId :: !(Maybe ColourId),
    eupAssets :: !(Maybe [Asset]),
    eupHandle :: !(Maybe Handle),
    eupLocale :: !(Maybe Locale),
    eupManagedBy :: !(Maybe ManagedBy),
    eupSSOId :: !(Maybe UserSSOId),
    eupSSOIdRemoved :: Bool
  }
  deriving stock (Show)

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

emailRemoved :: UserId -> Email -> UserEvent
emailRemoved u e =
  UserIdentityRemoved $ UserIdentityRemovedData u (Just e) Nothing

phoneRemoved :: UserId -> Phone -> UserEvent
phoneRemoved u p =
  UserIdentityRemoved $ UserIdentityRemovedData u Nothing (Just p)

emailUpdated :: UserId -> Email -> UserEvent
emailUpdated u e =
  UserIdentityUpdated $ UserIdentityUpdatedData u (Just e) Nothing

phoneUpdated :: UserId -> Phone -> UserEvent
phoneUpdated u p =
  UserIdentityUpdated $ UserIdentityUpdatedData u Nothing (Just p)

handleUpdated :: UserId -> Handle -> UserEvent
handleUpdated u h =
  UserUpdated $ (emptyUserUpdatedData u) {eupHandle = Just h}

localeUpdate :: UserId -> Locale -> UserEvent
localeUpdate u loc =
  UserUpdated $ (emptyUserUpdatedData u) {eupLocale = Just loc}

managedByUpdate :: UserId -> ManagedBy -> UserEvent
managedByUpdate u mb =
  UserUpdated $ (emptyUserUpdatedData u) {eupManagedBy = Just mb}

profileUpdated :: UserId -> UserUpdate -> UserEvent
profileUpdated u UserUpdate {..} =
  UserUpdated $
    (emptyUserUpdatedData u)
      { eupName = uupName,
        eupPict = uupPict,
        eupAccentId = uupAccentId,
        eupAssets = uupAssets
      }

emptyUpdate :: UserId -> UserEvent
emptyUpdate = UserUpdated . emptyUserUpdatedData

emptyUserUpdatedData :: UserId -> UserUpdatedData
emptyUserUpdatedData u =
  UserUpdatedData
    { eupId = u,
      eupName = Nothing,
      eupPict = Nothing,
      eupAccentId = Nothing,
      eupAssets = Nothing,
      eupHandle = Nothing,
      eupLocale = Nothing,
      eupManagedBy = Nothing,
      eupSSOId = Nothing,
      eupSSOIdRemoved = False
    }

connEventUserId :: ConnectionEvent -> UserId
connEventUserId ConnectionUpdated {..} = ucFrom ucConn

userEventUserId :: UserEvent -> UserId
userEventUserId (UserCreated u) = userId u
userEventUserId (UserActivated u) = userId u
userEventUserId (UserSuspended u) = u
userEventUserId (UserResumed u) = u
userEventUserId (UserDeleted u) = u
userEventUserId (UserUpdated u) = eupId u
userEventUserId (UserIdentityUpdated u) = eiuId u
userEventUserId (UserIdentityRemoved u) = eirId u
userEventUserId (UserLegalHoldDisabled uid) = uid
userEventUserId (UserLegalHoldEnabled uid) = uid
userEventUserId (LegalHoldClientRequested dat) = lhcTargetUser dat

propEventUserId :: PropertyEvent -> UserId
propEventUserId (PropertySet u _ _) = u
propEventUserId (PropertyDeleted u _) = u
propEventUserId (PropertiesCleared u) = u

logConnection :: UserId -> Qualified UserId -> Msg -> Msg
logConnection from (Qualified toUser toDomain) =
  "connection.from" .= toByteString from
    ~~ "connection.to" .= toByteString toUser
    ~~ "connection.to_domain" .= toByteString toDomain

logLocalConnection :: UserId -> UserId -> Msg -> Msg
logLocalConnection from to =
  "connection.from" .= toByteString from
    ~~ "connection.to" .= toByteString to

instance ToBytes Event where
  bytes (UserEvent e) = bytes e
  bytes (ConnectionEvent e) = bytes e
  bytes (PropertyEvent e) = bytes e
  bytes (ClientEvent e) = bytes e

instance ToBytes UserEvent where
  bytes e@UserCreated {} = val "user.new: " +++ toByteString (userEventUserId e)
  bytes e@UserActivated {} = val "user.activate: " +++ toByteString (userEventUserId e)
  bytes e@UserUpdated {} = val "user.update: " +++ toByteString (userEventUserId e)
  bytes e@UserIdentityUpdated {} = val "user.update: " +++ toByteString (userEventUserId e)
  bytes e@UserIdentityRemoved {} = val "user.identity-remove: " +++ toByteString (userEventUserId e)
  bytes e@UserSuspended {} = val "user.suspend: " +++ toByteString (userEventUserId e)
  bytes e@UserResumed {} = val "user.resume: " +++ toByteString (userEventUserId e)
  bytes e@UserDeleted {} = val "user.delete: " +++ toByteString (userEventUserId e)
  bytes e@UserLegalHoldDisabled {} = val "user.legalhold-disable: " +++ toByteString (userEventUserId e)
  bytes e@UserLegalHoldEnabled {} = val "user.legalhold-enable: " +++ toByteString (userEventUserId e)
  bytes (LegalHoldClientRequested payload) = val "user.legalhold-request: " +++ show payload

instance ToBytes ConnectionEvent where
  bytes e@ConnectionUpdated {} = val "user.connection: " +++ toByteString (connEventUserId e)

instance ToBytes PropertyEvent where
  bytes e@PropertySet {} = val "user.properties-set: " +++ toByteString (propEventUserId e)
  bytes e@PropertyDeleted {} = val "user.properties-delete: " +++ toByteString (propEventUserId e)
  bytes e@PropertiesCleared {} = val "user.properties-clear: " +++ toByteString (propEventUserId e)

instance ToBytes ClientEvent where
  bytes (ClientAdded u _) = val "user.client-add: " +++ toByteString u
  bytes (ClientRemoved u _) = val "user.client-remove: " +++ toByteString u
