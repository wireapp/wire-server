{-# LANGUAGE TemplateHaskell #-}

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

module Wire.ProposalStore where

import Imports
import Polysemy
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

data StoredProposal = StoredProposal
  { ref :: ProposalRef,
    origin :: Maybe ProposalOrigin,
    proposal :: RawMLS Proposal
  }

data ProposalStore m a where
  StoreProposal ::
    GroupId ->
    Epoch ->
    StoredProposal ->
    ProposalStore m ()
  -- | Store several proposals in bulk. The refs in the list must be distinct:
  -- the Postgres implementation upserts with @ON CONFLICT DO UPDATE@, which
  -- raises an error if the same ref appears twice in one batch. Callers
  -- satisfy this by construction (one proposal per signed message).
  StoreProposals ::
    GroupId ->
    Epoch ->
    [StoredProposal] ->
    ProposalStore m ()
  GetAllPendingProposals ::
    GroupId ->
    Epoch ->
    ProposalStore m [StoredProposal]
  DeleteAllProposals ::
    GroupId ->
    ProposalStore m ()

makeSem ''ProposalStore
