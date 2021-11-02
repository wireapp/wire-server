-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

import Galley.Effects.BotAccess (BotAccess (..))
import Galley.Effects.BrigAccess (BrigAccess (..))
import Galley.Effects.SparAccess (SparAccess (..))
import Galley.Env
import Galley.Intra.Client
import Galley.Intra.Spar
import Galley.Intra.Team
import Galley.Intra.User
import Galley.Intra.Util
import Imports
import Polysemy
import qualified Polysemy.Reader as P
import qualified Polysemy.TinyLog as P
import qualified UnliftIO

interpretBrigAccess ::
  Members '[Embed IO, P.TinyLog, P.Reader Env] r =>
  Sem (BrigAccess ': r) a ->
  Sem r a
interpretBrigAccess = interpret $ \case
  GetConnectionsUnqualified uids muids mrel ->
    embedIntra $ getConnectionsUnqualified uids muids mrel
  GetConnectionsUnqualifiedBidi uids1 uids2 mrel1 mrel2 ->
    embedIntra $
      UnliftIO.concurrently
        (getConnectionsUnqualified uids1 (Just uids2) mrel1)
        (getConnectionsUnqualified uids2 (Just uids1) mrel2)
  GetConnections uids mquids mrel ->
    embedIntra $
      getConnections uids mquids mrel
  PutConnectionInternal uc -> embedIntra $ putConnectionInternal uc
  ReauthUser uid reauth -> embedIntra $ reAuthUser uid reauth
  LookupActivatedUsers uids -> embedIntra $ lookupActivatedUsers uids
  GetUsers uids -> embedIntra $ getUsers uids
  DeleteUser uid -> embedIntra $ deleteUser uid
  GetContactList uid -> embedIntra $ getContactList uid
  GetRichInfoMultiUser uids -> embedIntra $ getRichInfoMultiUser uids
  GetSize tid -> embedIntra $ getSize tid
  LookupClients uids -> embedIntra $ lookupClients uids
  LookupClientsFull uids -> embedIntra $ lookupClientsFull uids
  NotifyClientsAboutLegalHoldRequest self other pk ->
    embedIntra $ notifyClientsAboutLegalHoldRequest self other pk
  GetLegalHoldAuthToken uid mpwd -> getLegalHoldAuthToken uid mpwd
  AddLegalHoldClientToUser uid conn pks lpk ->
    embedIntra $ addLegalHoldClientToUser uid conn pks lpk
  RemoveLegalHoldClientFromUser uid ->
    embedIntra $ removeLegalHoldClientFromUser uid

interpretSparAccess ::
  Members '[Embed IO, P.Reader Env] r =>
  Sem (SparAccess ': r) a ->
  Sem r a
interpretSparAccess = interpret $ \case
  DeleteTeam tid -> embedIntra $ deleteTeam tid

interpretBotAccess ::
  Members '[Embed IO, P.Reader Env] r =>
  Sem (BotAccess ': r) a ->
  Sem r a
interpretBotAccess = interpret $ \case
  DeleteBot cid bid -> embedIntra $ deleteBot cid bid
