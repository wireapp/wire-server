{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wwarn #-}

module Wire.MockInterpreters.InvitationCodeStore where

import Data.Id (InvitationId, TeamId)
import Data.Map (elems, (!?))
import Imports
import Polysemy
import Polysemy.State (State, get, gets)
import Wire.API.User (InvitationCode)
import Wire.InvitationCodeStore

-- TODO(mangoiv): I start to feel like we want a proper (in memory) DB here
inMemoryInvitationCodeStoreInterpreter ::
  forall r.
  ( Member (State (Map (TeamId, InvitationId) StoredInvitation)) r,
    Member (State (Map (InvitationCode) InvitationInfo)) r
  ) =>
  InterpreterFor InvitationCodeStore r
inMemoryInvitationCodeStoreInterpreter = interpret \case
  LookupInvitation tid iid -> gets (!? (tid, iid))
  LookupInvitationInfo iid -> gets (!? iid)
  LookupInvitationCodesByEmail em ->
    let c MkStoredInvitation {..}
          | email == em = Just MkStoredInvitationByTeam {..}
          | otherwise = Nothing
     in mapMaybe c . elems <$> get
