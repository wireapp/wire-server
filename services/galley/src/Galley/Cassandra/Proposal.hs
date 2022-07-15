{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Galley.Cassandra.Proposal (interpretProposalStoreToCassandra) where

import Cassandra
import Galley.Cassandra.Instances ()
import Galley.Cassandra.Store
import Galley.Effects.ProposalStore
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

type TTL = Integer

-- | Proposals in the database expire after this timeout in seconds
defaultTTL :: TTL
defaultTTL = 30 * 24 * 60 * 60

interpretProposalStoreToCassandra ::
  Members '[Embed IO, Input ClientState] r =>
  Sem (ProposalStore ': r) a ->
  Sem r a
interpretProposalStoreToCassandra =
  interpret $
    embedClient . \case
      StoreProposal groupId epoch ref raw ->
        retry x5 $
          write (storeQuery defaultTTL) (params LocalQuorum (groupId, epoch, ref, raw))
      GetProposal groupId epoch ref ->
        runIdentity <$$> retry x1 (query1 getQuery (params LocalQuorum (groupId, epoch, ref)))
      GetAllPendingProposals groupId epoch ->
        runIdentity <$$> retry x1 (query getAllPending (params LocalQuorum (groupId, epoch)))

storeQuery :: TTL -> PrepQuery W (GroupId, Epoch, ProposalRef, RawMLS Proposal) ()
storeQuery ttl =
  fromString $
    "insert into mls_proposal_refs (group_id, epoch, ref, proposal)\
    \ values (?, ?, ?, ?) using ttl "
      <> show ttl

getQuery :: PrepQuery R (GroupId, Epoch, ProposalRef) (Identity (RawMLS Proposal))
getQuery = "select proposal from mls_proposal_refs where group_id = ? and epoch = ? and ref = ?"

getAllPending :: PrepQuery R (GroupId, Epoch) (Identity ProposalRef)
getAllPending = "select ref from mls_proposal_refs where group_id = ? and epoch = ?"
