{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.JobStore.Postgres
  ( interpretJobStoreToPostgres,
  )
where

import Hasql.Pool
import Hasql.TH
import Imports
import Data.UUID (UUID)
import Polysemy
import Polysemy.Error (Error)
import Polysemy.Input
import Wire.API.Jobs
import Wire.API.PostgresMarshall
import Wire.JobStore
import Wire.Postgres

interpretJobStoreToPostgres ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  InterpreterFor JobStore r
interpretJobStoreToPostgres =
  interpret $ \case
    CreateJob job -> createJobImpl job
    FindJobById jobId -> findJobByIdImpl jobId
    FindJobsByTeamAndKind teamId kind -> findJobsByTeamAndKindImpl teamId kind
    FindJobsByConversationId conversationId -> findJobsByConversationIdImpl conversationId
    DeleteJob jobId -> deleteJobImpl jobId
    DeleteJobsByTeamAndKind teamId kind -> deleteJobsByTeamAndKindImpl teamId kind

createJobImpl ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  ScheduledJob ->
  Sem r ()
createJobImpl job =
  runStatement job $
    lmapPG
      [resultlessStatement|
        INSERT INTO scheduled_jobs
          (id, kind, team_id, conversation_id, scheduled_for)
        VALUES
          ($1 :: uuid, $2 :: int4, $3 :: uuid, $4 :: uuid?, $5 :: timestamptz) |]

findJobByIdImpl ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  UUID ->
  Sem r (Maybe ScheduledJob)
findJobByIdImpl jobId =
  runStatement jobId $
    dimapPG
      [maybeStatement|
        SELECT
          (id :: uuid), (kind :: int4), (team_id :: uuid), (conversation_id :: uuid?),
          (scheduled_for :: timestamptz)
        FROM scheduled_jobs
        WHERE id = ($1 :: uuid) |]

findJobsByTeamAndKindImpl ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  UUID ->
  ScheduledJobKind ->
  Sem r [ScheduledJob]
findJobsByTeamAndKindImpl teamId kind =
  runStatement (teamId, kind) $
    dimapPG
      [vectorStatement|
        SELECT
          (id :: uuid), (kind :: int4), (team_id :: uuid), (conversation_id :: uuid?),
          (scheduled_for :: timestamptz)
        FROM scheduled_jobs
        WHERE team_id = ($1 :: uuid) AND kind = ($2 :: int4)
        ORDER BY scheduled_for ASC, id ASC |]

findJobsByConversationIdImpl ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  UUID ->
  Sem r [ScheduledJob]
findJobsByConversationIdImpl conversationId =
  runStatement conversationId $
    dimapPG
      [vectorStatement|
        SELECT
          (id :: uuid), (kind :: int4), (team_id :: uuid), (conversation_id :: uuid?),
          (scheduled_for :: timestamptz)
        FROM scheduled_jobs
        WHERE conversation_id = ($1 :: uuid)
        ORDER BY scheduled_for ASC, id ASC |]

deleteJobImpl ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  UUID ->
  Sem r ()
deleteJobImpl jobId =
  runStatement jobId $
    lmapPG
      [resultlessStatement|
        DELETE FROM scheduled_jobs
        WHERE id = ($1 :: uuid) |]

deleteJobsByTeamAndKindImpl ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  UUID ->
  ScheduledJobKind ->
  Sem r ()
deleteJobsByTeamAndKindImpl teamId kind =
  runStatement (teamId, kind) $
    lmapPG
      [resultlessStatement|
        DELETE FROM scheduled_jobs
        WHERE team_id = ($1 :: uuid) AND kind = ($2 :: int4) |]
