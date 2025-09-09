{-# LANGUAGE RecordWildCards #-}

module Wire.AppStore.Postgres
  ( interpretAppStoreToPostgres,
  )
where

import Data.Aeson
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
    CreateApp userId teamId meta -> createAppImpl userId teamId meta

createAppImpl ::
  ( Member (Input Pool) r,
    Member (Embed IO) r,
    Member (Error UsageError) r
  ) =>
  UserId ->
  TeamId ->
  Object ->
  Sem r ()
createAppImpl userId teamId meta =
  runResultlessStatement (userId, teamId, meta) $
    lmapPG
      [resultlessStatement|
        insert into apps (user_id, team_id, metadata)
        values ($1 :: uuid, $2 :: uuid, $3 :: json) |]
