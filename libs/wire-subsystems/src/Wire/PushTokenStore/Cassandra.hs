-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Cassandra implementation of the user push-token store.
--
-- The queries are ported verbatim from "Gundeck.Push.Data" (retry counts and
-- consistency levels included). The ARN travels as 'Text'; Cassandra's
-- 'EndpointArn' conversion happens at the gundeck facade.
module Wire.PushTokenStore.Cassandra
  ( interpretPushTokenStoreToCassandra,
  )
where

import Cassandra as C
import Data.Id (ClientId, ConnId, UserId)
import Imports hiding (lookup)
import Polysemy
import Polysemy.Input
import Wire.API.Push.V2 (AppName, Token, Transport)
import Wire.PushTokenStore
import Wire.Util (embedClientInput)

interpretPushTokenStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r
  ) =>
  Sem (PushTokenStore ': r) a ->
  Sem r a
interpretPushTokenStoreToCassandra = interpret $ \case
  LookupPushTokens u c -> embedClientInput $ lookup u c
  InsertPushToken u t a p arn conn client ->
    embedClientInput $ insert u t a p arn conn client
  UpdatePushTokenArn u t a p arn ->
    embedClientInput $ updateArn u t a p arn
  DeletePushToken u t a p ->
    embedClientInput $ delete u t a p
  ErasePushTokens u ->
    embedClientInput $ erase u

lookup ::
  (MonadClient m) =>
  UserId ->
  Consistency ->
  m [PushTokenRow]
lookup u c = retry x1 (query q (params c (Identity u)))
  where
    q :: PrepQuery R (Identity UserId) (UserId, Transport, AppName, Token, Maybe Text, ConnId, Maybe ClientId)
    q = "select usr, transport, app, ptoken, arn, connection, client from user_push where usr = ?"

insert ::
  (MonadClient m) =>
  UserId ->
  Transport ->
  AppName ->
  Token ->
  Text ->
  ConnId ->
  ClientId ->
  m ()
insert u t a p arn conn client =
  retry x5 $ write q (params LocalQuorum (u, t, a, p, arn, conn, client))
  where
    q :: PrepQuery W (UserId, Transport, AppName, Token, Text, ConnId, ClientId) ()
    q = "insert into user_push (usr, transport, app, ptoken, arn, connection, client) values (?, ?, ?, ?, ?, ?, ?)"

updateArn ::
  (MonadClient m) =>
  UserId ->
  Transport ->
  AppName ->
  Token ->
  Text ->
  m ()
updateArn u t a p arn = retry x5 $ write q (params LocalQuorum (arn, u, t, a, p))
  where
    q :: PrepQuery W (Text, UserId, Transport, AppName, Token) ()
    q = {- `IF EXISTS`, but that requires benchmarking -} "update user_push set arn = ? where usr = ? and transport = ? and app = ? and ptoken = ?"

delete ::
  (MonadClient m) =>
  UserId ->
  Transport ->
  AppName ->
  Token ->
  m ()
delete u t a p = retry x5 $ write q (params LocalQuorum (u, t, a, p))
  where
    q :: PrepQuery W (UserId, Transport, AppName, Token) ()
    q = "delete from user_push where usr = ? and transport = ? and app = ? and ptoken = ?"

erase ::
  (MonadClient m) =>
  UserId ->
  m ()
erase u = retry x5 $ write q (params LocalQuorum (Identity u))
  where
    q :: PrepQuery W (Identity UserId) ()
    q = "delete from user_push where usr = ?"
