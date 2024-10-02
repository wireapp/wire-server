module Wire.MockInterpreters.InvitationStore where

import Data.Id (InvitationId, TeamId)
import Data.Map (elems, (!?))
import Data.Map qualified as M
import Imports
import Polysemy
import Polysemy.State (State, get, gets)
import Wire.API.User (InvitationCode (..))
import Wire.InvitationStore

inMemoryInvitationStoreInterpreter ::
  forall r.
  ( Member (State (Map (TeamId, InvitationId) StoredInvitation)) r,
    Member (State (Map (InvitationCode) StoredInvitation)) r
  ) =>
  InterpreterFor InvitationStore r
inMemoryInvitationStoreInterpreter = interpret \case
  InsertInvitation _a _timeout -> error "InsertInvitation"
  LookupInvitation tid iid -> gets (!? (tid, iid))
  LookupInvitationByCode iid -> gets (!? iid)
  LookupInvitationsByEmail em ->
    let c i = guard (i.email == em) $> i
     in mapMaybe c . elems <$> get @(Map (TeamId, InvitationId) _)
  LookupInvitationsPaginated {} -> error "LookupInvitationsPaginated"
  CountInvitations tid -> gets (fromIntegral . M.size . M.filterWithKey (\(tid', _) _v -> tid == tid'))
  DeleteInvitation _tid _invId -> error "DeleteInvitation"
  DeleteAllTeamInvitations _tid -> error "DeleteAllTeamInvitations"
