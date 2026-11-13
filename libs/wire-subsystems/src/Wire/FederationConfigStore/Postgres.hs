{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# OPTIONS -Wwarn #-}

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
  ( interpretFederationDomainConfig,
    AddFederationRemoteResult (..),
  )
where

import Control.Exception (throwIO)
import Control.Lens
import Data.Domain
import Data.Id
import Data.Vector (Vector)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.PostgresMarshall
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search
import Wire.FederationConfigStore
import Wire.Postgres

-- | Interpreter for the federation config store (Postgres database only).
-- Config-file precedence and strategy merging live in Wire.FederationConfigSubsystem.Interpreter.
interpretFederationDomainConfig ::
  forall r a.
  (PGConstraints r) =>
  Sem (FederationConfigStore ': r) a ->
  Sem r a
interpretFederationDomainConfig =
  interpret $ \case
    GetFederationConfig d -> do
      runError (getFederationConfigImpl d) >>= \case
        Left exc -> do
          -- todo log
          embed $ throwIO exc
        Right res -> pure res
    GetFederationConfigs -> do
      runError getFederationConfigsImpl >>= \case
        Left exc -> do
          -- todo log
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
  case res of
    Just (policy, restriction) ->
      Just
        . FederationDomainConfig rDomain policy
        <$> toRestriction rDomain (fromMaybe 0 restriction)
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

getFederationConfigsImpl :: forall r. (PGConstraints r) => Sem (Error RestrictionException : r) [FederationDomainConfig]
getFederationConfigsImpl =
  traverse mkFederationDomainConfig =<< runStatement () getAll
  where
    mkFederationDomainConfig :: (Domain, Int32, Maybe Int32) -> Sem (Error RestrictionException : r) FederationDomainConfig
    mkFederationDomainConfig (dom, fedUSP, rInt) =
      FederationDomainConfig dom (toEnum (fromIntegral fedUSP))
        <$> toRestriction dom (fromMaybe 0 rInt)

    getAll :: Hasql.Statement () [(Domain, Int32, Maybe Int32)]
    getAll =
      dimapPG
        [vectorStatement|
          SELECT domain :: text, search_policy :: integer, restriction :: integer? 
          FROM federation_remotes
        |]

addFederationConfigImpl :: (PGConstraints r) => FederationDomainConfig -> Sem r AddFederationRemoteResult
addFederationConfigImpl (FederationDomainConfig rDomain searchPolicy restriction) = do
  AddFederationRemoteSuccess <$ do
    _ <- runStatement (rDomain, searchPolicy, fromRestriction restriction) addConfig
    case restriction of
      FederationRestrictionByTeam tids -> 
        traverse_ (\tid -> runStatement (rDomain, tid) addFederationRemoteTeamQuery) tids
      FederationRestrictionAllowAll -> pure ()
  where
    addConfig :: Hasql.Statement (Domain, FederatedUserSearchPolicy, Int32) ()
    addConfig =
      lmapPG
        [resultlessStatement|
          INSERT INTO federation_remotes (domain, search_policy, restriction) 
          VALUES ($1 :: text, $2 :: integer, $3 :: integer)
        |]

addFederationRemoteTeamImpl :: (PGConstraints r) => Domain -> TeamId -> Sem r AddFederationRemoteTeamResult
addFederationRemoteTeamImpl rDomain tid = do
  runStatement (rDomain, tid) addFederationRemoteTeamQuery
  pure AddFederationRemoteTeamSuccess


updateFederationConfigImpl :: forall r. (PGConstraints r) => FederationDomainConfig -> Sem r UpdateFederationResult
updateFederationConfigImpl (FederationDomainConfig rDomain searchPolicy restriction) = do
  updated <- runStatement (searchPolicy, fromRestriction restriction, rDomain) updateConfig
  updateTeams
  case length updated of
    0 -> pure UpdateFederationRemoteNotFound
    _ -> pure UpdateFederationSuccess
  where
    updateConfig :: Hasql.Statement (FederatedUserSearchPolicy, Int32, Domain) (Vector ())
    updateConfig =
      lmapPG
        [vectorStatement|
          UPDATE federation_remotes 
          SET search_policy = $1 :: integer,
              restriction = $2 :: integer
          WHERE domain = $3 :: text
        |]

    updateTeams :: Sem r ()
    updateTeams = do
      runStatement rDomain dropTeams
      case restriction of
        FederationRestrictionByTeam tids ->
          traverse_ (\tid -> runStatement (rDomain, tid) addFederationRemoteTeamQuery) tids
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

toRestriction :: (Member (Error RestrictionException) r, PGConstraints r) => Domain -> Int32 -> Sem r FederationRestriction
toRestriction _ 0 = pure FederationRestrictionAllowAll
toRestriction dom 1 =
  fmap FederationRestrictionByTeam $ runStatement dom get
  where
    get :: Hasql.Statement Domain [TeamId]
    get =
      dimapPG
        [vectorStatement|
          SELECT team :: uuid
          FROM federation_remote_teams 
          WHERE domain = $1 :: text
        |]
toRestriction _ v = throw . RestrictionException $ v

fromRestriction :: FederationRestriction -> Int32
fromRestriction FederationRestrictionAllowAll = 0
fromRestriction (FederationRestrictionByTeam _) = 1

------------------------
-- Shared queries
------------------------

addFederationRemoteTeamQuery :: Hasql.Statement (Domain, TeamId) ()
addFederationRemoteTeamQuery =
  lmapPG
    [resultlessStatement|
      INSERT INTO federation_remote_teams (domain, team)
      VALUES ($1 :: text, $2 :: uuid)
    |]