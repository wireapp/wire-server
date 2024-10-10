-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Intra.Effects
  ( interpretBrigAccess,
    interpretSparAccess,
    interpretBotAccess,
  )
where

import Galley.API.Error
import Galley.Cassandra.Util
import Galley.Effects.BotAccess (BotAccess (..))
import Galley.Effects.BrigAccess (BrigAccess (..))
import Galley.Effects.SparAccess (SparAccess (..))
import Galley.Env
import Galley.Intra.Client
import Galley.Intra.Spar
import Galley.Intra.Team
import Galley.Intra.User
import Galley.Monad
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import UnliftIO qualified

interpretBrigAccess ::
  ( Member (Embed IO) r,
    Member (Error InternalError) r,
    Member TinyLog r,
    Member (Input Env) r
  ) =>
  Sem (BrigAccess ': r) a ->
  Sem r a
interpretBrigAccess = interpret $ \case
  GetConnectionsUnqualified uids muids mrel -> do
    logEffect "BrigAccess.GetConnectionsUnqualified"
    embedApp $ getConnectionsUnqualified uids muids mrel
  GetConnectionsUnqualifiedBidi uids1 uids2 mrel1 mrel2 -> do
    logEffect "BrigAccess.GetConnectionsUnqualifiedBidi"
    embedApp $
      UnliftIO.concurrently
        (getConnectionsUnqualified uids1 (Just uids2) mrel1)
        (getConnectionsUnqualified uids2 (Just uids1) mrel2)
  GetConnections uids mquids mrel -> do
    logEffect "BrigAccess.GetConnections"
    embedApp $
      getConnections uids mquids mrel
  PutConnectionInternal uc -> do
    logEffect "BrigAccess.PutConnectionInternal"
    embedApp $ putConnectionInternal uc
  ReauthUser uid reauth -> do
    logEffect "BrigAccess.ReauthUser"
    embedApp $ reAuthUser uid reauth
  LookupActivatedUsers uids -> do
    logEffect "BrigAccess.LookupActivatedUsers"
    embedApp $ lookupActivatedUsers uids
  GetUsers uids -> do
    logEffect "BrigAccess.GetUsers"
    embedApp $ getUsers uids
  DeleteUser uid -> do
    logEffect "BrigAccess.DeleteUser"
    embedApp $ deleteUser uid
  GetContactList uid -> do
    logEffect "BrigAccess.GetContactList"
    embedApp $ getContactList uid
  GetRichInfoMultiUser uids -> do
    logEffect "BrigAccess.GetRichInfoMultiUser"
    embedApp $ getRichInfoMultiUser uids
  GetUserExportData uid -> do
    logEffect "BrigAccess.GetUserExportData"
    embedApp $ getUserExportData uid
  GetSize tid -> do
    logEffect "BrigAccess.GetSize"
    embedApp $ getSize tid
  LookupClients uids -> do
    logEffect "BrigAccess.LookupClients"
    embedApp $ lookupClients uids
  LookupClientsFull uids -> do
    logEffect "BrigAccess.LookupClientsFull"
    embedApp $ lookupClientsFull uids
  NotifyClientsAboutLegalHoldRequest self other pk -> do
    logEffect "BrigAccess.NotifyClientsAboutLegalHoldRequest"
    embedApp $ notifyClientsAboutLegalHoldRequest self other pk
  GetLegalHoldAuthToken uid mpwd -> do
    logEffect "BrigAccess.GetLegalHoldAuthToken"
    getLegalHoldAuthToken uid mpwd
  AddLegalHoldClientToUserEither uid conn pks lpk -> do
    logEffect "BrigAccess.AddLegalHoldClientToUserEither"
    embedApp $ addLegalHoldClientToUser uid conn pks lpk
  RemoveLegalHoldClientFromUser uid -> do
    logEffect "BrigAccess.RemoveLegalHoldClientFromUser"
    embedApp $ removeLegalHoldClientFromUser uid
  GetAccountConferenceCallingConfigClient uid -> do
    logEffect "BrigAccess.GetAccountConferenceCallingConfigClient"
    embedApp $ getAccountConferenceCallingConfigClient uid
  GetLocalMLSClients qusr ss -> do
    logEffect "BrigAccess.GetLocalMLSClients"
    embedApp $ getLocalMLSClients qusr ss
  UpdateSearchVisibilityInbound status -> do
    logEffect "BrigAccess.UpdateSearchVisibilityInbound"
    embedApp $ updateSearchVisibilityInbound status

interpretSparAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r,
    Member TinyLog r
  ) =>
  Sem (SparAccess ': r) a ->
  Sem r a
interpretSparAccess = interpret $ \case
  DeleteTeam tid -> do
    logEffect "SparAccess.DeleteTeam"
    embedApp $ deleteTeam tid
  LookupScimUserInfos uids -> do
    logEffect "SparAccess.LookupScimUserInfos"
    embedApp $ lookupScimUserInfos uids

interpretBotAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r,
    Member TinyLog r
  ) =>
  Sem (BotAccess ': r) a ->
  Sem r a
interpretBotAccess = interpret $ \case
  DeleteBot cid bid -> do
    logEffect "BotAccess.DeleteBot"
    embedApp $ deleteBot cid bid
