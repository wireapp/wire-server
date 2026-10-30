-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.FederationConfigStore.Cassandra
  ( interpretFederationConfigStoreCassandra,
    remotesMapFromCfgFile,
    AddFederationRemoteResult (..),
  )
where

import Cassandra
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens
import Control.Monad.Catch (throwM)
import Data.Domain
import Data.Id
import Data.Map qualified as Map
import Database.CQL.Protocol (SerialConsistency (LocalSerialConsistency), serialConsistency)
import Imports
import Polysemy
import Polysemy.Embed
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search
import Wire.FederationConfigStore

-- | Interpreter for the federation config store (Cassandra database only).
-- Config-file precedence and strategy merging live in Wire.FederationConfigSubsystem.Interpreter.
interpretFederationConfigStoreCassandra ::
  forall r a.
  (Member (Embed IO) r) =>
  ClientState ->
  Sem (FederationConfigStore ': r) a ->
  Sem r a
interpretFederationConfigStoreCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . embed . \case
      GetFederationConfig d -> getFederationConfigImpl d
      GetFederationConfigs -> getFederationConfigsImpl
      AddFederationConfig cnf -> addFederationConfigImpl cnf
      UpdateFederationConfig cnf -> updateFederationConfigImpl cnf
      AddFederationRemoteTeam d t -> addFederationRemoteTeamImpl d t
      RemoveFederationRemoteTeam d t -> removeFederationRemoteTeamImpl d t
      GetFederationRemoteTeams d -> getFederationRemoteTeamsImpl d

-- | Compile config file list into a map indexed by domains.  Use this to make sure the config
-- file is consistent (ie., no two entries for the same domain).
-- This is called during initialization of the interpreter and the service will fail if the config is not consistent.
remotesMapFromCfgFile :: [FederationDomainConfig] -> Map Domain FederationDomainConfig
remotesMapFromCfgFile cfg =
  let dict = [(cnf.domain, cnf) | cnf <- cfg]
      merge c c' =
        if c == c'
          then c
          else error $ "error in config file: conflicting parameters on domain: " <> show (c, c')
   in Map.fromListWith merge dict

getFederationConfigImpl :: (MonadClient m) => Domain -> m (Maybe FederationDomainConfig)
getFederationConfigImpl rDomain = do
  mCnf <- retry x1 (query1 q (params LocalQuorum (Identity rDomain)))
  case mCnf of
    Just (p, r) -> Just . FederationDomainConfig rDomain p <$> toRestriction rDomain (fromMaybe 0 r)
    Nothing -> pure Nothing
  where
    q :: PrepQuery R (Identity Domain) (FederatedUserSearchPolicy, Maybe Int32)
    q = "SELECT search_policy, restriction FROM federation_remotes WHERE domain = ?"

getFederationConfigsImpl :: forall m. (MonadClient m) => m [FederationDomainConfig]
getFederationConfigsImpl = (\(d, p, r) -> FederationDomainConfig d p r) <$$> qry
  where
    qry :: m [(Domain, FederatedUserSearchPolicy, FederationRestriction)]
    qry = do
      res <- retry x1 . query get $ params LocalQuorum ()
      forM res $ \(d, p, rInt) -> do
        (d,p,) <$> toRestriction d (fromMaybe 0 rInt)

    get :: PrepQuery R () (Domain, FederatedUserSearchPolicy, Maybe Int32)
    get = fromString $ "SELECT domain, search_policy, restriction FROM federation_remotes LIMIT " <> show maxKnownNodes

addFederationConfigImpl :: (MonadClient m) => FederationDomainConfig -> m AddFederationRemoteResult
addFederationConfigImpl (FederationDomainConfig rDomain searchPolicy restriction) = do
  -- get the number of existing remote federations in the db
  fedRemoteCount <- length <$> getFederationConfigsImpl
  if fedRemoteCount >= maxKnownNodes
    then
      pure AddFederationRemoteMaxRemotesReached
    else
      AddFederationRemoteSuccess <$ do
        retry x5 (write addConfig (params LocalQuorum (rDomain, searchPolicy, fromRestriction restriction)))
        case restriction of
          FederationRestrictionByTeam tids ->
            retry x5 . batch . forM_ tids $ addPrepQuery addTeams . (rDomain,)
          FederationRestrictionAllowAll -> pure ()
  where
    addConfig :: PrepQuery W (Domain, FederatedUserSearchPolicy, Int32) ()
    addConfig = "INSERT INTO federation_remotes (domain, search_policy, restriction) VALUES (?, ?, ?)"

    addTeams :: PrepQuery W (Domain, TeamId) ()
    addTeams = "INSERT INTO federation_remote_teams (domain, team) VALUES (?, ?)"

updateFederationConfigImpl :: (MonadClient m) => FederationDomainConfig -> m UpdateFederationResult
updateFederationConfigImpl (FederationDomainConfig rDomain searchPolicy restriction) = do
  let configParams =
        ( params
            LocalQuorum
            (searchPolicy, fromRestriction restriction, rDomain)
        )
          { serialConsistency = Just LocalSerialConsistency
          }
  r <- retry x1 (trans updateConfig configParams)
  updateTeams
  case r of
    [] -> pure UpdateFederationRemoteNotFound
    [_] -> pure UpdateFederationSuccess
    _ -> throwM $ ErrorCall "Primary key violation detected federation_remotes"
  where
    updateConfig :: PrepQuery W (FederatedUserSearchPolicy, Int32, Domain) x
    updateConfig = "UPDATE federation_remotes SET search_policy = ?, restriction = ? WHERE domain = ? IF EXISTS"

    updateTeams :: (MonadClient m) => m ()
    updateTeams = retry x5 $ do
      write dropTeams (params LocalQuorum (Identity rDomain))
      case restriction of
        FederationRestrictionByTeam tids ->
          batch . forM_ tids $ addPrepQuery insertTeam . (rDomain,)
        FederationRestrictionAllowAll -> pure ()

    dropTeams :: PrepQuery W (Identity Domain) ()
    dropTeams = "DELETE FROM federation_remote_teams WHERE domain = ?"

    insertTeam :: PrepQuery W (Domain, TeamId) ()
    insertTeam = "INSERT INTO federation_remote_teams (domain, team) VALUES (?, ?)"

addFederationRemoteTeamImpl :: (MonadClient m) => Domain -> TeamId -> m AddFederationRemoteTeamResult
addFederationRemoteTeamImpl rDomain tid = do
  retry x1 $ write add (params LocalQuorum (rDomain, tid))
  -- todo here or outside?
  pure AddFederationRemoteTeamSuccess
  where
    add :: PrepQuery W (Domain, TeamId) ()
    add = "INSERT INTO federation_remote_teams (domain, team) VALUES (?, ?)"

getFederationRemoteTeamsImpl :: (MonadClient m) => Domain -> m [FederationRemoteTeam]
getFederationRemoteTeamsImpl rDomain = do
  fmap (FederationRemoteTeam . runIdentity) <$> retry x1 (query get (params LocalQuorum (Identity rDomain)))
  where
    get :: PrepQuery R (Identity Domain) (Identity TeamId)
    get = "SELECT team FROM federation_remote_teams WHERE domain = ?"

removeFederationRemoteTeamImpl :: (MonadClient m) => Domain -> TeamId -> m ()
removeFederationRemoteTeamImpl rDomain rteam =
  retry x1 $ write delete (params LocalQuorum (rDomain, rteam))
  where
    delete :: PrepQuery W (Domain, TeamId) ()
    delete = "DELETE FROM federation_remote_teams WHERE domain = ? AND team = ?"

data RestrictionException = RestrictionException Int32

instance Show RestrictionException where
  show (RestrictionException v) =
    "Expected a RestrictionPolicy encoding, but found a value " <> show v

instance Exception RestrictionException

toRestriction :: (MonadClient m) => Domain -> Int32 -> m FederationRestriction
toRestriction _ 0 = pure FederationRestrictionAllowAll
toRestriction dom 1 =
  fmap FederationRestrictionByTeam $
    runIdentity <$$> retry x1 (query getTeams (params LocalQuorum (Identity dom)))
  where
    getTeams :: PrepQuery R (Identity Domain) (Identity TeamId)
    getTeams = fromString $ "SELECT team FROM federation_remote_teams WHERE domain = ?"
toRestriction _ v = throwM . RestrictionException $ v

fromRestriction :: FederationRestriction -> Int32
fromRestriction FederationRestrictionAllowAll = 0
fromRestriction (FederationRestrictionByTeam _) = 1

maxKnownNodes :: Int
maxKnownNodes = 10000
