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
    interpretGundeckAccess,
  )
where

import Galley.API.Error
import Galley.Effects.BotAccess (BotAccess (..))
import Galley.Effects.BrigAccess (BrigAccess (..))
import Galley.Effects.GundeckAccess (GundeckAccess (..))
import Galley.Effects.SparAccess (SparAccess (..))
import Galley.Env
import Galley.Intra.Client
import qualified Galley.Intra.Push.Internal as G
import Galley.Intra.Spar
import Galley.Intra.Team
import Galley.Intra.User
import Galley.Monad
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified UnliftIO
import Galley.Intra.Util (HasIntraComponentEndpoints)

interpretBrigAccess ::
  forall c r a.
  ( Member (Embed IO) r,
    Member (Error InternalError) r,
    Member P.TinyLog r,
    Member (Input c) r,
    HasIntraComponentEndpoints c,
    HasManager c,
    HasRequestId' c
  ) =>
  Sem (BrigAccess ': r) a ->
  Sem r a
interpretBrigAccess = interpret $ \case
  GetConnectionsUnqualified uids muids mrel ->
    embedApp' @c $ unApp' $ getConnectionsUnqualified uids muids mrel
  GetConnectionsUnqualifiedBidi uids1 uids2 mrel1 mrel2 ->
    embedApp' @c $ unApp' $
      UnliftIO.concurrently
        (getConnectionsUnqualified uids1 (Just uids2) mrel1)
        (getConnectionsUnqualified uids2 (Just uids1) mrel2)
  GetConnections uids mquids mrel ->
    embedApp' @c $ unApp' $
      getConnections uids mquids mrel
  PutConnectionInternal uc -> embedApp' @c $ unApp' $ putConnectionInternal uc
  ReauthUser uid reauth -> embedApp' @c $ unApp' $ reAuthUser uid reauth
  LookupActivatedUsers uids -> embedApp' @c $ unApp' $ lookupActivatedUsers uids
  GetUsers uids -> embedApp' @c $ unApp' $ getUsers uids
  DeleteUser uid -> embedApp' @c $ unApp' $ deleteUser uid
  GetContactList uid -> embedApp' @c $ unApp' $ getContactList uid
  GetRichInfoMultiUser uids -> embedApp' @c $ unApp' $ getRichInfoMultiUser uids
  GetSize tid -> embedApp' @c $ unApp' $ getSize tid
  LookupClients uids -> embedApp' @c $ unApp' $ lookupClients uids
  LookupClientsFull uids -> embedApp' @c $ unApp' $ lookupClientsFull uids
  NotifyClientsAboutLegalHoldRequest self other pk ->
    embedApp' @c $ unApp' $ notifyClientsAboutLegalHoldRequest self other pk
  GetLegalHoldAuthToken uid mpwd -> getLegalHoldAuthToken @c uid mpwd
  AddLegalHoldClientToUserEither uid conn pks lpk ->
    embedApp' @c $ unApp' $ addLegalHoldClientToUser uid conn pks lpk
  RemoveLegalHoldClientFromUser uid ->
    embedApp' @c $ unApp' $ removeLegalHoldClientFromUser uid
  GetAccountConferenceCallingConfigClient uid ->
    embedApp' @c $ unApp' $ getAccountConferenceCallingConfigClient uid
  GetClientByKeyPackageRef ref ->
    embedApp' @c $ unApp' $ getClientByKeyPackageRef ref
  GetLocalMLSClients qusr ss -> embedApp' @c $ unApp' $ getLocalMLSClients qusr ss
  AddKeyPackageRef ref qusr cl qcnv ->
    embedApp' @c $ unApp' $
      addKeyPackageRef ref qusr cl qcnv
  ValidateAndAddKeyPackageRef nkp ->
    embedApp' @c $ unApp' $
      validateAndAddKeyPackageRef nkp
  UpdateKeyPackageRef update ->
    embedApp' @c $ unApp' $
      updateKeyPackageRef update
  UpdateSearchVisibilityInbound status ->
    embedApp' @c $ unApp' $ updateSearchVisibilityInbound status

interpretSparAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r
  ) =>
  Sem (SparAccess ': r) a ->
  Sem r a
interpretSparAccess = interpret $ \case
  DeleteTeam tid -> embedApp $ deleteTeam tid
  LookupScimUserInfos uids -> embedApp $ lookupScimUserInfos uids

interpretBotAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r
  ) =>
  Sem (BotAccess ': r) a ->
  Sem r a
interpretBotAccess = interpret $ \case
  DeleteBot cid bid -> embedApp $ deleteBot cid bid

interpretGundeckAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r
  ) =>
  Sem (GundeckAccess ': r) a ->
  Sem r a
interpretGundeckAccess = interpret $ \case
  Push ps -> embedApp $ G.push ps
  PushSlowly ps -> embedApp $ G.pushSlowly ps
