{-# LANGUAGE TemplateHaskell #-}

module Wire.AppStore where

import Data.Aeson
import Data.Id
import Data.UUID
import Imports
import Polysemy
import Wire.API.PostgresMarshall

data StoredApp = StoredApp
  { id :: UserId,
    teamId :: TeamId,
    meta :: Object
  }
  deriving (Eq, Ord, Show)

instance PostgresMarshall StoredApp (UUID, UUID, Value) where
  postgresMarshall app =
    ( postgresMarshall app.id,
      postgresMarshall app.teamId,
      postgresMarshall app.meta
    )

instance PostgresUnmarshall (UUID, UUID, Value) StoredApp where
  postgresUnmarshall (uid, teamId, meta) =
    StoredApp
      <$> postgresUnmarshall uid
      <*> postgresUnmarshall teamId
      <*> postgresUnmarshall meta

data AppStore m a where
  CreateApp :: StoredApp -> AppStore m ()
  GetApp :: UserId -> AppStore m (Maybe StoredApp)

makeSem ''AppStore
