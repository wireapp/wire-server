{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.AppStore.Postgres
  ( interpretAppStoreToPostgres,
  )
where

import Data.Id
import Hasql.Pool
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error (Error)
import Polysemy.Input
import Wire.API.PostgresMarshall
import Wire.AppStore
import Wire.Postgres

interpretAppStoreToPostgres ::
  ( Member (Embed IO) r,
    Member (Input Pool) r,
    Member (Error UsageError) r
  ) =>
  InterpreterFor AppStore r
interpretAppStoreToPostgres =
  interpret $ \case
    CreateApp app -> createAppImpl app
    GetApp userId teamId -> getAppImpl userId teamId

createAppImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  StoredApp ->
  Sem r ()
createAppImpl app =
  runStatement app $
    lmapPG
      [resultlessStatement|
        insert into apps (user_id, team_id, metadata, category, description, author)
        values ($1 :: uuid, $2 :: uuid, $3 :: json, $4 :: text, $5 :: text, $6 :: text) |]

getAppImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  TeamId ->
  Sem r (Maybe StoredApp)
getAppImpl uid tid =
  runStatement (uid, tid) $
    dimapPG
      [maybeStatement| select (user_id :: uuid), (team_id :: uuid), (metadata :: json), (category :: text), (description :: text), (author :: text)
        from apps where user_id = ($1 :: uuid) and team_id = ($2 :: uuid) |]
