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

module Wire.MockInterpreters.InvitationStore where

import Data.Id (InvitationId, TeamId)
import Data.Map (alter, elems, (!?))
import Data.Map qualified as M
import Imports hiding ((!?))
import Polysemy
import Polysemy.State (State, get, gets, modify)
import Wire.API.User (InvitationCode (..))
import Wire.InvitationStore

inMemoryInvitationStoreInterpreter ::
  forall r.
  ( Member (State (Map (TeamId, InvitationId) StoredInvitation)) r,
    Member (State (Map (InvitationCode) StoredInvitation)) r
  ) =>
  InterpreterFor InvitationStore r
inMemoryInvitationStoreInterpreter = interpret \case
  InsertInvitation inv _timeout ->
    let -- NB: timeout is not taken into account, mock invitations live forever.
        modByIds :: Map (TeamId, InvitationId) StoredInvitation -> Map (TeamId, InvitationId) StoredInvitation
        modByIds = alter (\_ -> Just $ insertInvToStoredInv inv) (inv.teamId, inv.invitationId)

        modByCode :: Map (InvitationCode) StoredInvitation -> Map (InvitationCode) StoredInvitation
        modByCode = alter (\_ -> Just $ insertInvToStoredInv inv) inv.code
     in do
          modify modByIds
          modify modByCode
          fromJust <$> gets (!? inv.code)
  LookupInvitation tid iid -> gets (!? (tid, iid))
  LookupInvitationByCode iid -> gets (!? iid)
  LookupInvitationsByEmail em ->
    let c i = guard (i.email == em) $> i
     in mapMaybe c . elems <$> get @(Map (TeamId, InvitationId) _)
  LookupInvitationsPaginated {} -> error "LookupInvitationsPaginated"
  CountInvitations tid -> gets (fromIntegral . M.size . M.filterWithKey (\(tid', _) _v -> tid == tid'))
  DeleteInvitation _tid _invId -> error "DeleteInvitation"
  DeleteAllTeamInvitations _tid -> error "DeleteAllTeamInvitations"
