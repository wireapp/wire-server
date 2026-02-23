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
import Data.Range
import Hasql.Pool
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error (Error)
import Polysemy.Input
import Wire.API.App qualified as App
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
    GetApps teamId -> getAppsImpl teamId
    UpdateApp teamId appId put -> updateAppImpl teamId appId put
    DeleteApp userId teamId -> deleteAppImpl userId teamId

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
        insert into apps (user_id, team_id, metadata, category, description, creator)
        values ($1 :: uuid, $2 :: uuid, $3 :: json, $4 :: text, $5 :: text, $6 :: uuid) |]

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
      [maybeStatement| select (user_id :: uuid), (team_id :: uuid), (metadata :: json), (category :: text), (description :: text), (creator :: uuid)
        from apps where user_id = ($1 :: uuid) and team_id = ($2 :: uuid) |]

getAppsImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  TeamId ->
  Sem r [StoredApp]
getAppsImpl tid =
  runStatement tid $
    dimapPG
      [vectorStatement| select (user_id :: uuid), (team_id :: uuid), (metadata :: json), (category :: text), (description :: text), (creator :: uuid)
        from apps where team_id = ($1 :: uuid) |]

updateAppImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  TeamId ->
  UserId ->
  StoredAppUpdate ->
  Sem r (Either AppStoreError ())
updateAppImpl (toUUID -> teamId) (toUUID -> appId) upd = do
  exists <-
    runStatement (appId, teamId) $
      [singletonStatement|
      SELECT EXISTS(SELECT 1 FROM apps WHERE user_id = ($1 :: uuid) AND team_id = ($2 :: uuid)) :: bool |]
  if exists
    then
      Right <$> do
        case (App.categoryToText <$> upd.category, fromRange <$> upd.description) of
          (Just cat, Just desc) ->
            runStatement (cat, desc, appId, teamId) $
              [resultlessStatement|
              UPDATE apps SET category = ($1 :: text), description = ($2 :: text)
              WHERE user_id = ($3 :: uuid) AND team_id = ($4 :: uuid) |]
          (Just cat, Nothing) ->
            runStatement (cat, appId, teamId) $
              [resultlessStatement|
              UPDATE apps SET category = ($1 :: text)
              WHERE user_id = ($2 :: uuid) AND team_id = ($3 :: uuid) |]
          (Nothing, Just desc) ->
            runStatement (desc, appId, teamId) $
              [resultlessStatement|
              UPDATE apps SET description = ($1 :: text)
              WHERE user_id = ($2 :: uuid) AND team_id = ($3 :: uuid) |]
          (Nothing, Nothing) -> pure ()
    else do
      pure $ Left NotFound

deleteAppImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  TeamId ->
  Sem r ()
deleteAppImpl uid tid =
  runStatement (uid, tid) $
    lmapPG
      [resultlessStatement|
        delete from apps where user_id = ($1 :: uuid) and team_id = ($2 :: uuid) |]
