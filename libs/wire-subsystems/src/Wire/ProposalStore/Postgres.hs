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
    upsertProposal,
  )
where

import Data.Timeout
import Data.Vector qualified as V
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
import Wire.ProposalStore

interpretProposalStoreToPostgres ::
  (PGConstraints r) =>
  Sem (ProposalStore ': r) a ->
  Sem r a
interpretProposalStoreToPostgres = interpret $ \case
  StoreProposal groupId epoch proposal ->
    upsertProposal groupId epoch proposal.ref proposal.origin proposal.proposal defaultProposalTTLSeconds
  GetProposal groupId epoch ref -> do
    mrow <- runStatement (groupId, epoch, ref) getProposalStatement
    pure $ mrow >>= either (const Nothing) Just . postgresUnmarshall
  GetAllPendingProposalRefs groupId epoch -> do
    rows <- runStatement (groupId, epoch) getAllPendingProposalRefsStatement
    pure $
      mapMaybe
        (either (const Nothing) Just . postgresUnmarshall)
        (V.toList rows)
  GetAllPendingProposals groupId epoch -> do
    rows <- runStatement (groupId, epoch) getAllPendingProposalsStatement
    pure $
      mapMaybe
        ( \(ref, origin, proposal) -> do
            ref' <- either (const Nothing) Just (postgresUnmarshall ref)
            origin' <- traverse (either (const Nothing) Just . postgresUnmarshall) origin
            proposal' <- either (const Nothing) Just (postgresUnmarshall proposal)
            pure (StoredProposal ref' origin' proposal')
        )
        (V.toList rows)
  DeleteAllProposals groupId ->
    runStatement groupId deleteAllProposalsStatement

-- | Proposals in the database expire after this timeout
defaultProposalTTL :: Timeout
defaultProposalTTL = 28 # Day

defaultProposalTTLSeconds :: Int32
defaultProposalTTLSeconds = fromIntegral (defaultProposalTTL #> Second)

-- | Shared by the interpreter and the migration copy loop so they cannot
-- drift. Mirrors the Cassandra insert, which is an upsert.
upsertProposal ::
  (PGConstraints r) =>
  GroupId ->
  Epoch ->
  ProposalRef ->
  Maybe ProposalOrigin ->
  RawMLS Proposal ->
  -- | Remaining TTL in seconds
  Int32 ->
  Sem r ()
upsertProposal groupId epoch ref origin proposal ttl =
  runStatement (groupId, epoch, ref, origin, proposal, ttl) upsertProposalStatement

upsertProposalStatement :: Hasql.Statement (GroupId, Epoch, ProposalRef, Maybe ProposalOrigin, RawMLS Proposal, Int32) ()
upsertProposalStatement =
  lmapPG
    [resultlessStatement|INSERT INTO mls_proposal_refs
                           (group_id, epoch, ref, origin, proposal, expires_at)
                         VALUES
                           ($1 :: bytea, $2 :: int8, $3 :: bytea, $4 :: int4?, $5 :: bytea, now() + make_interval(secs => $6 :: int))
                         ON CONFLICT (group_id, epoch, ref) DO UPDATE
                         SET origin = ($4 :: int4?),
                             proposal = ($5 :: bytea),
                             expires_at = now() + make_interval(secs => $6 :: int)
     |]

getProposalStatement :: Hasql.Statement (GroupId, Epoch, ProposalRef) (Maybe ByteString)
getProposalStatement =
  lmapPG
    [maybeStatement|SELECT proposal :: bytea
                   FROM mls_proposal_refs
                   WHERE group_id = ($1 :: bytea)
                     AND epoch = ($2 :: int8)
                     AND ref = ($3 :: bytea)
                     AND expires_at > now()
                   |]

getAllPendingProposalRefsStatement :: Hasql.Statement (GroupId, Epoch) (V.Vector ByteString)
getAllPendingProposalRefsStatement =
  lmapPG
    [vectorStatement|SELECT ref :: bytea
                    FROM mls_proposal_refs
                    WHERE group_id = ($1 :: bytea)
                      AND epoch = ($2 :: int8)
                      AND expires_at > now()
                   |]

getAllPendingProposalsStatement :: Hasql.Statement (GroupId, Epoch) (V.Vector (ByteString, Maybe Int32, ByteString))
getAllPendingProposalsStatement =
  lmapPG
    [vectorStatement|SELECT ref :: bytea, origin :: int4?, proposal :: bytea
                    FROM mls_proposal_refs
                    WHERE group_id = ($1 :: bytea)
                      AND epoch = ($2 :: int8)
                      AND expires_at > now()
                   |]

deleteAllProposalsStatement :: Hasql.Statement GroupId ()
deleteAllProposalsStatement =
  lmapPG
    [resultlessStatement|DELETE FROM mls_proposal_refs
                       WHERE group_id = ($1 :: bytea)
                      |]
