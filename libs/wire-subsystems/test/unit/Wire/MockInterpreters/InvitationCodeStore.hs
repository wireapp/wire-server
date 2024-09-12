{-# LANGUAGE RecordWildCards #-}

module Wire.MockInterpreters.InvitationCodeStore where

import Data.Id (InvitationId, TeamId)
import Data.Json.Util (toUTCTimeMillis)
import Data.Map (elems, (!?))
import Data.Map qualified as M
import GHC.IO.Unsafe (unsafePerformIO)
import Imports
import Polysemy
import Polysemy.State (State, get, gets, modify')
import Wire.API.User (InvitationCode (..))
import Wire.InvitationCodeStore
import Wire.InvitationCodeStore.Cassandra (mkInvitationCode)

inMemoryInvitationCodeStoreInterpreter ::
  forall r.
  ( Member (State (Map (TeamId, InvitationId) StoredInvitation)) r,
    Member (State (Map (InvitationCode) StoredInvitationInfo)) r
  ) =>
  InterpreterFor InvitationCodeStore r
inMemoryInvitationCodeStoreInterpreter = interpret \case
  InsertInvitation (MkInsertInvitation invitationId teamId role' createdAt' createdBy email name) _timeout -> do
    -- Currently unused.
    let code = unsafePerformIO mkInvitationCode -- Should confirm that this works as intended (new code on every run)
    let role = Just role'
        createdAt = toUTCTimeMillis createdAt'
        inv = MkStoredInvitation {..}
    modify' $ \s -> M.insert (inv.teamId, inv.invitationId) inv s
    pure inv
  LookupInvitation tid iid -> gets (!? (tid, iid))
  LookupInvitationInfo iid -> gets (!? iid)
  LookupInvitationCodesByEmail em ->
    let c MkStoredInvitation {..}
          | email == em = Just MkStoredInvitationInfo {..}
          | otherwise = Nothing
     in mapMaybe c . elems <$> get
  LookupInvitationsPaginated {} -> error "LookupInvitationsPaginated"
  CountInvitations tid -> gets (fromIntegral . M.size . M.filterWithKey (\(tid', _) _v -> tid == tid'))
  DeleteInvitation _tid _invId -> error "DeleteInvitation"
  DeleteAllTeamInvitations _tid -> error "DeleteAllTeamInvitations"
