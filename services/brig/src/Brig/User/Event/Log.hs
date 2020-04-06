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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brig.User.Event.Log where

import Brig.User.Event
import Data.ByteString.Conversion
import Data.Id
import Imports
import System.Logger.Class

connection :: UserId -> UserId -> Msg -> Msg
connection from to =
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
