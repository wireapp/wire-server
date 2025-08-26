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

module Galley.Cassandra.Proposal
  ( interpretProposalStoreToCassandra,
    ProposalOrigin (..),
  )
where

import Cassandra
import Data.Timeout
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Effects.ProposalStore
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.ConversationStore.Cassandra.Instances ()

-- | Proposals in the database expire after this timeout
defaultTTL :: Timeout
defaultTTL = 28 # Day

interpretProposalStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (ProposalStore ': r) a ->
  Sem r a
interpretProposalStoreToCassandra = interpret $ \case
  StoreProposal groupId epoch ref origin raw -> do
    logEffect "ProposalStore.StoreProposal"
    embedClient . retry x5 $
      write (storeQuery defaultTTL) (params LocalQuorum (groupId, epoch, ref, origin, raw))
  GetProposal groupId epoch ref -> do
    logEffect "ProposalStore.GetProposal"
    embedClient (runIdentity <$$> retry x1 (query1 getQuery (params LocalQuorum (groupId, epoch, ref))))
  GetAllPendingProposalRefs groupId epoch -> do
    logEffect "ProposalStore.GetAllPendingProposalRefs"
    embedClient (runIdentity <$$> retry x1 (query getAllPendingRef (params LocalQuorum (groupId, epoch))))
  GetAllPendingProposals groupId epoch -> do
    logEffect "ProposalStore.GetAllPendingProposals"
    embedClient $ retry x1 (query getAllPending (params LocalQuorum (groupId, epoch)))
  DeleteAllProposals groupId -> do
    logEffect "ProposalStore.DeleteAllProposals"
    embedClient $ retry x5 (write deleteAllProposalsForGroup (params LocalQuorum (Identity groupId)))

storeQuery :: Timeout -> PrepQuery W (GroupId, Epoch, ProposalRef, ProposalOrigin, RawMLS Proposal) ()
storeQuery ttl =
  fromString $
    "insert into mls_proposal_refs (group_id, epoch, ref, origin, proposal)\
    \ values (?, ?, ?, ?, ?) using ttl "
      <> show (ttl #> Second)

getQuery :: PrepQuery R (GroupId, Epoch, ProposalRef) (Identity (RawMLS Proposal))
getQuery = "select proposal from mls_proposal_refs where group_id = ? and epoch = ? and ref = ?"

getAllPendingRef :: PrepQuery R (GroupId, Epoch) (Identity ProposalRef)
getAllPendingRef = "select ref from mls_proposal_refs where group_id = ? and epoch = ?"

getAllPending :: PrepQuery R (GroupId, Epoch) (Maybe ProposalOrigin, RawMLS Proposal)
getAllPending = "select origin, proposal from mls_proposal_refs where group_id = ? and epoch = ?"

deleteAllProposalsForGroup :: PrepQuery W (Identity GroupId) ()
deleteAllProposalsForGroup = "delete from mls_proposal_refs where group_id = ?"
