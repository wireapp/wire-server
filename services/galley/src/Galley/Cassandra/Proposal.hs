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
import Wire.API.MLS.Group
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

newtype TTL = TTL Integer
  deriving newtype (Cql)

-- | Proposals in the database expire after this timeout in seconds
ttl :: TTL
ttl = TTL $ 30 * 24 * 60 * 60

interpretProposalStoreToCassandra ::
  Members '[Embed IO, Input ClientState] r =>
  Sem (ProposalStore ': r) a ->
  Sem r a
interpretProposalStoreToCassandra =
  interpret $
    embedClient . \case
      StoreProposal ref raw gid epoch ->
        retry x5 $
          write storeQuery (params LocalQuorum (ref, raw, gid, epoch, ttl))
      GetProposal ref ->
        retry x1 (query1 getQuery (params LocalQuorum (Identity ref)))

storeQuery :: PrepQuery W (ProposalRef, RawMLS Proposal, GroupId, Epoch, TTL) ()
storeQuery = "insert into mls_proposal_refs (ref, proposal, group_id, epoch) values (?, ?, ?, ?) using ttl ?"

getQuery :: PrepQuery R (Identity ProposalRef) (RawMLS Proposal, GroupId, Epoch)
getQuery = "select proposal, group_id, epoch from mls_proposal_refs where ref = ?"
