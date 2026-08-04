-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ProposalStore.Postgres
  ( interpretProposalStoreToPostgres,
  )
where

import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.PostgresMarshall
import Wire.Postgres
import Wire.ProposalStore (ProposalStore (..), StoredProposal (..))

interpretProposalStoreToPostgres ::
  (PGConstraints r) =>
  Sem (ProposalStore ': r) a ->
  Sem r a
interpretProposalStoreToPostgres = interpret $ \case
  StoreProposal groupId epoch sp ->
    insertProposal groupId epoch sp
  GetProposal groupId epoch ref ->
    selectProposal groupId epoch ref
  GetAllPendingProposalRefs groupId epoch ->
    selectAllPendingProposalRefs groupId epoch
  GetAllPendingProposals groupId epoch ->
    selectAllPendingProposals groupId epoch
  DeleteAllProposals groupId ->
    deleteAllProposals groupId

insertProposal ::
  (PGConstraints r) =>
  GroupId ->
  Epoch ->
  StoredProposal ->
  Sem r ()
insertProposal groupId epoch sp =
  runStatement (groupId, epoch, sp.ref, sp.origin, sp.proposal) insert
  where
    insert ::
      Hasql.Statement (GroupId, Epoch, ProposalRef, Maybe ProposalOrigin, RawMLS Proposal) ()
    insert =
      lmapPG
        [resultlessStatement|
          INSERT INTO mls_proposal_refs (group_id, epoch, ref, origin, proposal, expires_at)
          VALUES ($1 :: bytea, $2 :: int8, $3 :: bytea, $4 :: int4?, $5 :: bytea, now () + interval '28 days')
          ON CONFLICT (group_id, epoch, ref) DO UPDATE
          SET origin = ($4 :: int4?),
              proposal = ($5 :: bytea),
              expires_at = now () + interval '28 days'
        |]

selectProposal ::
  (PGConstraints r) =>
  GroupId ->
  Epoch ->
  ProposalRef ->
  Sem r (Maybe (RawMLS Proposal))
selectProposal groupId epoch ref =
  runStatement (groupId, epoch, ref) select
  where
    select ::
      Hasql.Statement (GroupId, Epoch, ProposalRef) (Maybe (RawMLS Proposal))
    select =
      dimapPG
        [maybeStatement|
          SELECT (proposal :: bytea)
          FROM mls_proposal_refs
          WHERE group_id = ($1 :: bytea)
            AND epoch = ($2 :: int8)
            AND ref = ($3 :: bytea)
            AND expires_at > now ()
        |]

selectAllPendingProposalRefs ::
  (PGConstraints r) =>
  GroupId ->
  Epoch ->
  Sem r [ProposalRef]
selectAllPendingProposalRefs groupId epoch =
  runStatement (groupId, epoch) select
  where
    select :: Hasql.Statement (GroupId, Epoch) [ProposalRef]
    select =
      dimapPG
        [vectorStatement|
          SELECT (ref :: bytea)
          FROM mls_proposal_refs
          WHERE group_id = ($1 :: bytea)
            AND epoch = ($2 :: int8)
            AND expires_at > now ()
        |]

selectAllPendingProposals ::
  (PGConstraints r) =>
  GroupId ->
  Epoch ->
  Sem r [StoredProposal]
selectAllPendingProposals groupId epoch =
  fmap mkStoredProposal <$> runStatement (groupId, epoch) select
  where
    mkStoredProposal (ref, origin, proposal) = StoredProposal ref origin proposal
    select ::
      Hasql.Statement (GroupId, Epoch) [(ProposalRef, Maybe ProposalOrigin, RawMLS Proposal)]
    select =
      dimapPG
        [vectorStatement|
          SELECT (ref :: bytea), (origin :: int4?), (proposal :: bytea)
          FROM mls_proposal_refs
          WHERE group_id = ($1 :: bytea)
            AND epoch = ($2 :: int8)
            AND expires_at > now ()
        |]

deleteAllProposals ::
  (PGConstraints r) =>
  GroupId ->
  Sem r ()
deleteAllProposals groupId =
  runStatement groupId delete
  where
    delete :: Hasql.Statement GroupId ()
    delete =
      lmapPG
        [resultlessStatement|
          DELETE FROM mls_proposal_refs
          WHERE group_id = ($1 :: bytea)
        |]
