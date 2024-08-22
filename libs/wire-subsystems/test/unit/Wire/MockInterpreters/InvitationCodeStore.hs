{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Wire.MockInterpreters.InvitationCodeStore where

import Data.Id (InvitationId, TeamId)
import Data.Map (elems, (!?))
import Imports
import Polysemy
import Polysemy.State (State, get, gets)
import Wire.InvitationCodeStore

-- TODO(mangoiv): I start to feel like we want a proper (in memory) DB here
inMemoryInvitationCodeStoreInterpreter :: forall r. (Member (State (Map (TeamId, InvitationId) StoredInvitation)) r) => InterpreterFor InvitationCodeStore r
inMemoryInvitationCodeStoreInterpreter = interpret \case
  LookupInvitation tid iid -> gets (!? (tid, iid))
  LookupInvitationInfo _iid -> todo
  LookupInvitationCodesByEmail em ->
    let c MkStoredInvitation {..}
          | email == em = Just MkStoredInvitationByTeam {..}
          | otherwise = Nothing
     in mapMaybe c . elems <$> get
