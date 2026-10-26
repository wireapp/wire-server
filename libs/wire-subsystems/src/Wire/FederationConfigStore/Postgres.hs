{-# LANGUAGE DeriveAnyClass #-}

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
  ( interpretFederationConfigStoreToPostgres,
    upsertFederationConfig,
    insertFederationRemoteTeam,
  )
where

import Control.Exception (throwIO)
import Data.Domain
import Data.Id
import Data.Int qualified as DI
import Data.Text qualified as Text
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search (FederatedUserSearchPolicy)
import Wire.FederationConfigStore
import Wire.Postgres

interpretFederationConfigStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor FederationConfigStore r
interpretFederationConfigStoreToPostgres = interpret $ \case
  GetFederationConfig d -> getFederationConfigImpl d
  GetFederationConfigs -> getFederationConfigsImpl
  AddFederationConfig cnf -> addFederationConfigImpl cnf
  UpdateFederationConfig cnf -> updateFederationConfigImpl cnf
  AddFederationRemoteTeam d t -> AddFederationRemoteTeamSuccess <$ insertFederationRemoteTeam d t
  RemoveFederationRemoteTeam d t -> removeFederationRemoteTeamImpl d t
  GetFederationRemoteTeams d -> getFederationRemoteTeamsImpl d

getFederationConfigImpl :: (PGConstraints r) => Domain -> Sem r (Maybe FederationDomainConfig)
getFederationConfigImpl d =
  runStatement (postgresMarshall d) getStatement
    >>= traverse (uncurry (decodeConfigRow (domainText d)))
  where
    getStatement :: Hasql.Statement Text (Maybe (DI.Int32, Maybe DI.Int32))
    getStatement =
      [maybeStatement|SELECT search_policy :: int4, restriction :: int4?
                      FROM federation_remotes
                      WHERE domain = ($1 :: text)|]

getFederationConfigsImpl :: (PGConstraints r) => Sem r [FederationDomainConfig]
getFederationConfigsImpl =
  runStatement () getAllStatement >>= traverse (\(d, p, r) -> decodeConfigRow d p r) . toList
  where
    getAllStatement :: Hasql.Statement () (Vector (Text, DI.Int32, Maybe DI.Int32))
    getAllStatement =
      [vectorStatement|SELECT domain :: text, search_policy :: int4, restriction :: int4?
                       FROM federation_remotes|]

decodeConfigRow :: (PGConstraints r) => Text -> DI.Int32 -> Maybe DI.Int32 -> Sem r FederationDomainConfig
decodeConfigRow dText policyInt restrictionRaw = do
  domain <- case postgresUnmarshall dText of
    Right dom -> pure dom
    Left err -> throwDecode $ "invalid domain " <> dText <> ": " <> err
  policy <- decodePolicy policyInt
  restriction <- toRestriction domain restrictionRaw
  pure $ FederationDomainConfig domain policy restriction

addFederationConfigImpl :: (PGConstraints r) => FederationDomainConfig -> Sem r AddFederationRemoteResult
addFederationConfigImpl (FederationDomainConfig d policy restriction) = do
  -- Same guard as the Cassandra interpreter: a bounded number of remotes.
  -- (Faithful port, including the cost: this decodes all rows on every add.)
  fedRemoteCount <- length <$> getFederationConfigsImpl
  if fedRemoteCount >= maxKnownNodes
    then pure AddFederationRemoteMaxRemotesReached
    else do
      upsertFederationConfig d policy (Just (restrictionInt restriction))
      traverse_ (insertFederationRemoteTeam d) (restrictionTeams restriction)
      pure AddFederationRemoteSuccess

maxKnownNodes :: Int
maxKnownNodes = 10000

-- | Mirrors the Cassandra LWT @IF EXISTS@ semantics with a guarded update:
-- the row is only written when the domain already exists.
updateFederationConfigImpl :: (PGConstraints r) => FederationDomainConfig -> Sem r UpdateFederationResult
updateFederationConfigImpl (FederationDomainConfig d policy restriction) = do
  updated <-
    runStatement
      (postgresMarshall d, postgresMarshall policy, Just (restrictionInt restriction))
      updateStatement
  -- The Cassandra interpreter rewrites the team restriction rows on every
  -- update (delete all, reinsert), regardless of whether the LWT matched;
  -- mirror that exactly.
  deleteAllFederationRemoteTeams d
  traverse_ (insertFederationRemoteTeam d) (restrictionTeams restriction)
  case updated of
    Just True -> pure UpdateFederationSuccess
    _ -> pure UpdateFederationRemoteNotFound
  where
    updateStatement :: Hasql.Statement (Text, DI.Int32, Maybe DI.Int32) (Maybe Bool)
    updateStatement =
      [maybeStatement|UPDATE federation_remotes
                      SET search_policy = ($2 :: int4), restriction = ($3 :: int4?)
                      WHERE domain = ($1 :: text)
                      RETURNING true :: boolean|]

removeFederationRemoteTeamImpl :: (PGConstraints r) => Domain -> TeamId -> Sem r ()
removeFederationRemoteTeamImpl d t =
  runStatement (postgresMarshall d, postgresMarshall t) deleteStatement
  where
    deleteStatement :: Hasql.Statement (Text, UUID) ()
    deleteStatement =
      [resultlessStatement|DELETE FROM federation_remote_teams
                           WHERE domain = ($1 :: text) AND team = ($2 :: uuid)|]

getFederationRemoteTeamsImpl :: (PGConstraints r) => Domain -> Sem r [FederationRemoteTeam]
getFederationRemoteTeamsImpl d = do
  teams <- getFederationRemoteTeamIds d
  pure $ map FederationRemoteTeam teams

getFederationRemoteTeamIds :: (PGConstraints r) => Domain -> Sem r [TeamId]
getFederationRemoteTeamIds d = do
  teams <- runStatement (postgresMarshall d) teamsStatement
  forM (toList teams) $ \t -> case postgresUnmarshall t of
    Right tid -> pure tid
    Left err -> throwDecode $ "invalid team uuid " <> Text.pack (show t) <> ": " <> err
  where
    teamsStatement :: Hasql.Statement Text (Vector UUID)
    teamsStatement =
      [vectorStatement|SELECT team :: uuid
                       FROM federation_remote_teams
                       WHERE domain = ($1 :: text)|]

deleteAllFederationRemoteTeams :: (PGConstraints r) => Domain -> Sem r ()
deleteAllFederationRemoteTeams d =
  runStatement (postgresMarshall d) deleteAllStatement
  where
    deleteAllStatement :: Hasql.Statement Text ()
    deleteAllStatement =
      [resultlessStatement|DELETE FROM federation_remote_teams
                           WHERE domain = ($1 :: text)|]

-- | Unconditional upsert: a Cassandra INSERT is an upsert, so 'AddFederationConfig'
-- overwrites any existing row for the domain.
upsertFederationConfig :: (PGConstraints r) => Domain -> FederatedUserSearchPolicy -> Maybe DI.Int32 -> Sem r ()
upsertFederationConfig d policy restriction =
  runStatement (postgresMarshall d, postgresMarshall policy, restriction) upsertStatement
  where
    upsertStatement :: Hasql.Statement (Text, DI.Int32, Maybe DI.Int32) ()
    upsertStatement =
      [resultlessStatement|INSERT INTO federation_remotes (domain, search_policy, restriction)
                           VALUES (($1 :: text), ($2 :: int4), ($3 :: int4?))
                           ON CONFLICT (domain) DO UPDATE
                           SET search_policy = EXCLUDED.search_policy,
                               restriction = EXCLUDED.restriction|]

insertFederationRemoteTeam :: (PGConstraints r) => Domain -> TeamId -> Sem r ()
insertFederationRemoteTeam d t =
  runStatement (postgresMarshall d, postgresMarshall t) insertStatement
  where
    insertStatement :: Hasql.Statement (Text, UUID) ()
    insertStatement =
      [resultlessStatement|INSERT INTO federation_remote_teams (domain, team)
                           VALUES (($1 :: text), ($2 :: uuid))
                           ON CONFLICT (domain, team) DO NOTHING|]

restrictionTeams :: FederationRestriction -> [TeamId]
restrictionTeams = \case
  FederationRestrictionAllowAll -> []
  FederationRestrictionByTeam tids -> tids

restrictionInt :: FederationRestriction -> DI.Int32
restrictionInt = postgresMarshall

decodePolicy :: (PGConstraints r) => DI.Int32 -> Sem r FederatedUserSearchPolicy
decodePolicy i = case postgresUnmarshall i of
  Right p -> pure p
  Left err -> throwDecode err

-- | Decodes the raw @restriction@ column value.  NULL and 0 mean "allow all";
-- 1 means "restricted by team", with the team list read from
-- @federation_remote_teams@ (mirroring @toRestriction@ in the Cassandra
-- interpreter).  Any other value is an encoding error.
toRestriction :: (PGConstraints r) => Domain -> Maybe DI.Int32 -> Sem r FederationRestriction
toRestriction _ (Just 0) = pure FederationRestrictionAllowAll
toRestriction _ Nothing = pure FederationRestrictionAllowAll
toRestriction d (Just 1) = FederationRestrictionByTeam <$> getFederationRemoteTeamIds d
toRestriction _ v = embed $ throwIO (RestrictionException v)

data RestrictionException = RestrictionException (Maybe DI.Int32)
  deriving stock (Show)
  deriving anyclass (Exception)

newtype DecodeException = DecodeException Text
  deriving stock (Show)
  deriving anyclass (Exception)

throwDecode :: (PGConstraints r) => Text -> Sem r a
throwDecode = embed . throwIO . DecodeException
