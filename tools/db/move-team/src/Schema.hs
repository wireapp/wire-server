{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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

module Schema where

import Cassandra
import Conduit
import Data.Id
import Data.Time
import Data.UUID
import Imports
import Types
import Wire.API.Team.Permission

-- galley.team_member

type RowGalleyTeamMember = (Maybe UUID, Maybe UUID, Maybe UTCTime, Maybe UUID, Maybe Int32, Maybe Permissions)

selectGalleyTeamMember :: PrepQuery R (Identity TeamId) RowGalleyTeamMember
selectGalleyTeamMember = "select team, user, invited_at, invited_by, legalhold_status, perms from team_member where team =  ?"

readGalleyTeamMember :: Env -> TeamId -> IO [RowGalleyTeamMember]
readGalleyTeamMember Env {..} tid =
  runClient envGalley $
    retry x1 (query selectGalleyTeamMember (params Quorum (pure tid)))

readGalleyTeamMemberConduit :: Env -> TeamId -> ConduitM () [RowGalleyTeamMember] IO ()
readGalleyTeamMemberConduit Env {..} tid =
  transPipe (runClient envGalley) $
    paginateC selectGalleyTeamMember (paramsP Quorum (pure tid) envPageSize) x5

-- galley.team_conv

type RowGalleyTeamConv = (Maybe UUID, Maybe UUID, Maybe Bool)

selectGalleyTeamConv :: PrepQuery R (Identity TeamId) RowGalleyTeamConv
selectGalleyTeamConv = "select team, conv, managed from team_conv where team =  ?"

readGalleyTeamConv :: Env -> TeamId -> IO [RowGalleyTeamConv]
readGalleyTeamConv Env {..} tid =
  runClient envGalley $
    retry x1 (query selectGalleyTeamConv (params Quorum (pure tid)))

readGalleyTeamConvConduit :: Env -> TeamId -> ConduitM () [RowGalleyTeamConv] IO ()
readGalleyTeamConvConduit Env {..} tid =
  transPipe (runClient envGalley) $
    paginateC selectGalleyTeamConv (paramsP Quorum (pure tid) envPageSize) x5

-- galley.clients

type RowGalleyClients = (Maybe UUID, Maybe (Cassandra.Set Text))

selectGalleyClients :: PrepQuery R (Identity [UserId]) RowGalleyClients
selectGalleyClients = "select user, clients from clients where user in  ?"

readGalleyClients :: Env -> [UserId] -> IO [RowGalleyClients]
readGalleyClients Env {..} uids =
  runClient envGalley $
    retry x1 (query selectGalleyClients (params Quorum (pure uids)))

readGalleyClientsConduit :: Env -> [UserId] -> ConduitM () [RowGalleyClients] IO ()
readGalleyClientsConduit Env {..} uids =
  transPipe (runClient envGalley) $
    paginateC selectGalleyClients (paramsP Quorum (pure uids) envPageSize) x5
