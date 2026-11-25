{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Wire.FederationConfigStore.Postgres
  ( interpretFederationDomainConfigStorePostgres,
    AddFederationRemoteResult (..),
  )
where

import Control.Exception (throwIO)
import Control.Lens
import Data.Domain
import Data.Id
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog (TinyLog, err)
import System.Logger.Message qualified as Log
import Wire.API.PostgresMarshall
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search
import Wire.FederationConfigStore
import Wire.Postgres

-- | Interpreter for the federation config store (Postgres database only).
-- Config-file precedence and strategy merging live in Wire.FederationConfigSubsystem.Interpreter.
interpretFederationDomainConfigStorePostgres ::
  forall r a.
  (PGConstraints r, Member TinyLog r) =>
  Sem (FederationConfigStore ': r) a ->
  Sem r a
interpretFederationDomainConfigStorePostgres =
  interpret $ \case
    GetFederationConfig d -> do
      runError (getFederationConfigImpl d) >>= \case
        Left exc -> do
          err $
            Log.msg
              (Log.val "Invalid restriction entry encountered in database during getFederationConfig.")
              . Log.field "domain" (show d)
          embed $ throwIO exc
        Right res -> pure res
    GetFederationConfigs -> do
      runError getFederationConfigsImpl >>= \case
        Left exc -> do
          err $
            Log.msg
              (Log.val "Invalid restriction entry encountered in database during getFederationConfigs.")
          embed $ throwIO exc
        Right res -> pure res
    AddFederationConfig cnf -> addFederationConfigImpl cnf
    UpdateFederationConfig cnf -> updateFederationConfigImpl cnf
    AddFederationRemoteTeam d t -> addFederationRemoteTeamImpl d t
    RemoveFederationRemoteTeam d t -> removeFederationRemoteTeamImpl d t
    GetFederationRemoteTeams d -> getFederationRemoteTeamsImpl d

getFederationConfigImpl :: (PGConstraints r) => Domain -> Sem (Error RestrictionException : r) (Maybe FederationDomainConfig)
getFederationConfigImpl rDomain = do
  res <- runStatement rDomain get
  rTids <- runStatement rDomain getTids
  case res of
    Just (policy, restriction) ->
      Just
        . FederationDomainConfig rDomain policy
        <$> toRestriction (fromMaybe 0 restriction) rTids
    Nothing -> pure Nothing
  where
    get :: Hasql.Statement Domain (Maybe (FederatedUserSearchPolicy, Maybe Int32))
    get =
      dimapPG
        [maybeStatement|
          SELECT search_policy :: integer, restriction :: integer?
          FROM federation_remotes
          WHERE domain = $1 :: text
        |]
    getTids :: Hasql.Statement Domain [TeamId]
    getTids =
      dimapPG
        [vectorStatement|
          SELECT team :: uuid
          FROM federation_remote_teams
          WHERE domain = $1 :: text
        |]

getFederationConfigsImpl :: forall r. (PGConstraints r) => Sem (Error RestrictionException : r) [FederationDomainConfig]
getFederationConfigsImpl =
  traverse mkFederationDomainConfig =<< runStatement () getAll
  where
    mkFederationDomainConfig :: (Domain, FederatedUserSearchPolicy, Maybe Int32, [TeamId]) -> Sem (Error RestrictionException : r) FederationDomainConfig
    mkFederationDomainConfig (dom, fedUSP, rInt, rTids) =
      FederationDomainConfig dom fedUSP
        <$> toRestriction (fromMaybe 0 rInt) rTids

    getAll :: Hasql.Statement () [(Domain, FederatedUserSearchPolicy, Maybe Int32, [TeamId])]
    getAll =
      dimapPG @_ @_ @(Vector (Text, Int32, Maybe Int32, Vector UUID))
        [vectorStatement|
            SELECT
              fr.domain :: text,
              fr.search_policy :: integer,
              fr.restriction :: integer?,
              COALESCE(array_agg(frt.team) FILTER (WHERE frt.team IS NOT NULL), array[]::uuid[]) :: uuid[]
            FROM federation_remotes as fr
            LEFT JOIN federation_remote_teams AS frt
              ON fr.domain = frt.domain
              GROUP BY fr.domain, fr.search_policy, fr.restriction
          |]

-- Note: unlike the cassandra store, this does not enforce any `maxKnownNodes`.
-- This limitation was due to an issue with cassandra and is dropped in the postgres implementation.
addFederationConfigImpl :: (PGConstraints r) => FederationDomainConfig -> Sem r AddFederationRemoteResult
addFederationConfigImpl (FederationDomainConfig rDomain searchPolicy restriction) = do
  AddFederationRemoteSuccess <$ do
    _ <- runStatement (rDomain, searchPolicy, fromRestriction restriction) addConfig
    case restriction of
      FederationRestrictionByTeam tids ->
        runStatement (rDomain, tids) addFederationRemoteTeamsQuery
      FederationRestrictionAllowAll -> pure ()
  where
    addConfig :: Hasql.Statement (Domain, FederatedUserSearchPolicy, Int32) ()
    addConfig =
      lmapPG
        [resultlessStatement|
          INSERT INTO federation_remotes (domain, search_policy, restriction)
          VALUES ($1 :: text, $2 :: integer, $3 :: integer)
          ON CONFLICT (domain) DO UPDATE
          SET search_policy = EXCLUDED.search_policy,
              restriction = EXCLUDED.restriction
        |]

addFederationRemoteTeamImpl :: (PGConstraints r) => Domain -> TeamId -> Sem r AddFederationRemoteTeamResult
addFederationRemoteTeamImpl rDomain tid = do
  runStatement (rDomain, [tid]) addFederationRemoteTeamsQuery
  pure AddFederationRemoteTeamSuccess

updateFederationConfigImpl :: forall r. (PGConstraints r) => FederationDomainConfig -> Sem r UpdateFederationResult
updateFederationConfigImpl (FederationDomainConfig rDomain searchPolicy restriction) = do
  updated <- runStatement (searchPolicy, fromRestriction restriction, rDomain) updateConfig
  updateTeams
  case updated of
    0 -> pure UpdateFederationRemoteNotFound
    _ -> pure UpdateFederationSuccess
  where
    updateConfig :: Hasql.Statement (FederatedUserSearchPolicy, Int32, Domain) (Int64)
    updateConfig =
      lmapPG
        [rowsAffectedStatement|
          UPDATE federation_remotes
          SET search_policy = $1 :: integer,
              restriction   = $2 :: integer
          WHERE domain = $3 :: text
        |]

    updateTeams :: Sem r ()
    updateTeams = do
      runStatement rDomain dropTeams
      case restriction of
        FederationRestrictionByTeam tids ->
          runStatement (rDomain, tids) addFederationRemoteTeamsQuery
        FederationRestrictionAllowAll -> pure ()

    dropTeams :: Hasql.Statement Domain ()
    dropTeams =
      lmapPG
        [resultlessStatement|
          DELETE FROM federation_remote_teams
          WHERE domain = $1 :: text
        |]

getFederationRemoteTeamsImpl :: (PGConstraints r) => Domain -> Sem r [FederationRemoteTeam]
getFederationRemoteTeamsImpl rDomain =
  FederationRemoteTeam <$$> runStatement rDomain get
  where
    get :: Hasql.Statement Domain [TeamId]
    get =
      dimapPG
        [vectorStatement|
          SELECT team :: uuid
          FROM federation_remote_teams
          WHERE domain = $1 :: text
        |]

removeFederationRemoteTeamImpl :: (PGConstraints r) => Domain -> TeamId -> Sem r ()
removeFederationRemoteTeamImpl rDomain rTeam =
  runStatement (rDomain, rTeam) delete
  where
    delete :: Hasql.Statement (Domain, TeamId) ()
    delete =
      lmapPG
        [resultlessStatement|
          DELETE FROM federation_remote_teams
          WHERE domain = $1 :: text AND team = $2 :: uuid
        |]

data RestrictionException = RestrictionException Int32

instance Show RestrictionException where
  show (RestrictionException v) =
    "Expected a RestrictionPolicy encoding, but found a value " <> show v

instance Exception RestrictionException

toRestriction :: (Member (Error RestrictionException) r) => Int32 -> [TeamId] -> Sem r FederationRestriction
toRestriction 0 _ = pure FederationRestrictionAllowAll
toRestriction 1 tids = pure $ FederationRestrictionByTeam tids
toRestriction v _ = throw . RestrictionException $ v

fromRestriction :: FederationRestriction -> Int32
fromRestriction FederationRestrictionAllowAll = 0
fromRestriction (FederationRestrictionByTeam _) = 1

------------------------
-- Shared queries
------------------------

-- Since in cassandra insert == upsert, we need to ensure that inserting already existing
-- values does not fail, therefore the ON CONFLICT clause was added.
addFederationRemoteTeamsQuery :: Hasql.Statement (Domain, [TeamId]) ()
addFederationRemoteTeamsQuery =
  lmapPG @(Text, Vector UUID)
    [resultlessStatement|
      INSERT INTO federation_remote_teams (domain, team)
      SELECT $1 :: text, * FROM UNNEST ($2 :: uuid[])
      ON CONFLICT (domain, team) DO NOTHING
    |]
