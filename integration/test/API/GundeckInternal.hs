-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module API.GundeckInternal where

import Testlib.Prelude

postPush ::
  ( HasCallStack,
    MakesValue user,
    MakesValue a
  ) =>
  user ->
  [a] ->
  App Response
postPush user payloads = do
  req <- baseRequest user Gundeck Unversioned "/i/push/v2"
  body <- traverse make payloads
  submit "POST" $ req & addJSON body

getPresence ::
  (HasCallStack, MakesValue user) =>
  user ->
  App Response
getPresence u = do
  uid <- u %. "id" & asString
  req <-
    baseRequest u Gundeck Unversioned
      $ "/i/presences/"
      <> uid
  submit "GET" req

unregisterUser ::
  (HasCallStack, MakesValue user) =>
  user ->
  App Response
unregisterUser u = do
  req <- baseRequest u Gundeck Unversioned "/i/user/"
  submit "DELETE" req

getPushTokens ::
  (HasCallStack, MakesValue user) =>
  user ->
  App Response
getPushTokens u = do
  uid <- u %. "id" & asString
  req <- baseRequest u Gundeck Unversioned ("/i/push-tokens/" <> uid)
  submit "GET" req
