{-# LANGUAGE RecordWildCards #-}

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
    GetApp userId -> getAppImpl userId

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
        insert into apps (user_id, team_id, metadata)
        values ($1 :: uuid, $2 :: uuid, $3 :: json) |]

getAppImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  Sem r (Maybe StoredApp)
getAppImpl uid =
  runStatement uid $
    dimapPG
      [maybeStatement| select (user_id :: uuid), (team_id :: uuid), (metadata :: json) 
        from apps where user_id = ($1 :: uuid) |]
