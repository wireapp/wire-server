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

interpretBrigAccess ::
  Members '[Embed IO, Error InternalError, P.TinyLog, Input Env] r =>
  Sem (BrigAccess ': r) a ->
  Sem r a
interpretBrigAccess = interpret $ \case
  GetConnectionsUnqualified uids muids mrel ->
    embedApp $ getConnectionsUnqualified uids muids mrel
  GetConnectionsUnqualifiedBidi uids1 uids2 mrel1 mrel2 ->
    embedApp $
      UnliftIO.concurrently
        (getConnectionsUnqualified uids1 (Just uids2) mrel1)
        (getConnectionsUnqualified uids2 (Just uids1) mrel2)
  GetConnections uids mquids mrel ->
    embedApp $
      getConnections uids mquids mrel
  PutConnectionInternal uc -> embedApp $ putConnectionInternal uc
  ReauthUser uid reauth -> embedApp $ reAuthUser uid reauth
  LookupActivatedUsers uids -> embedApp $ lookupActivatedUsers uids
  GetUsers uids -> embedApp $ getUsers uids
  DeleteUser uid -> embedApp $ deleteUser uid
  GetContactList uid -> embedApp $ getContactList uid
  GetRichInfoMultiUser uids -> embedApp $ getRichInfoMultiUser uids
  GetSize tid -> embedApp $ getSize tid
  LookupClients uids -> embedApp $ lookupClients uids
  LookupClientsFull uids -> embedApp $ lookupClientsFull uids
  NotifyClientsAboutLegalHoldRequest self other pk ->
    embedApp $ notifyClientsAboutLegalHoldRequest self other pk
  GetLegalHoldAuthToken uid mpwd -> getLegalHoldAuthToken uid mpwd
  AddLegalHoldClientToUserEither uid conn pks lpk ->
    embedApp $ addLegalHoldClientToUser uid conn pks lpk
  RemoveLegalHoldClientFromUser uid ->
    embedApp $ removeLegalHoldClientFromUser uid
  GetAccountFeatureConfigClient uid ->
    embedApp $ getAccountFeatureConfigClient uid
  GetClientByKeyPackageRef ref ->
    embedApp $ getClientByKeyPackageRef ref
  GetLocalMLSClients qusr ss -> embedApp $ getLocalMLSClients qusr ss
  UpdateSearchVisibilityInbound status ->
    embedApp $ updateSearchVisibilityInbound status

interpretSparAccess ::
  Members '[Embed IO, Input Env] r =>
  Sem (SparAccess ': r) a ->
  Sem r a
interpretSparAccess = interpret $ \case
  DeleteTeam tid -> embedApp $ deleteTeam tid

interpretBotAccess ::
  Members '[Embed IO, Input Env] r =>
  Sem (BotAccess ': r) a ->
  Sem r a
interpretBotAccess = interpret $ \case
  DeleteBot cid bid -> embedApp $ deleteBot cid bid

interpretGundeckAccess ::
  Members '[Embed IO, Input Env] r =>
  Sem (GundeckAccess ': r) a ->
  Sem r a
interpretGundeckAccess = interpret $ \case
  Push ps -> embedApp $ G.push ps
  PushSlowly ps -> embedApp $ G.pushSlowly ps
