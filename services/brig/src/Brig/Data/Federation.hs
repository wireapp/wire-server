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

module Brig.Data.Federation
  ( getFederationRemotes,
    addFederationRemote,
    updateFederationRemote,
    deleteFederationRemote,
    addFederationRemoteTeam,
    getFederationRemoteTeams,
    deleteFederationRemoteTeam,
    AddFederationRemoteResult (..),
  )
where

import Brig.Data.Instances ()
import Cassandra
import Control.Exception (ErrorCall (ErrorCall))
import Control.Monad.Catch (throwM)
import Data.Domain
import Data.Id
import Database.CQL.Protocol (SerialConsistency (LocalSerialConsistency), serialConsistency)
import Imports
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search

maxKnownNodes :: Int
maxKnownNodes = 10000

data RestrictionException = RestrictionException Int32

instance Show RestrictionException where
  show (RestrictionException v) =
    "Expected a RestrictionPolicy encoding, but found a value " <> show v

instance Exception RestrictionException

toRestriction :: MonadClient m => Domain -> Int32 -> m FederationRestriction
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

getFederationRemotes :: forall m. MonadClient m => m [FederationDomainConfig]
getFederationRemotes = (\(d, p, r) -> FederationDomainConfig d p r) <$$> qry
  where
    qry :: m [(Domain, FederatedUserSearchPolicy, FederationRestriction)]
    qry = do
      res <- retry x1 . query get $ params LocalQuorum ()
      forM res $ \(d, p, rInt) -> do
        (d,p,) <$> toRestriction d rInt

    get :: PrepQuery R () (Domain, FederatedUserSearchPolicy, Int32)
    get = fromString $ "SELECT domain, search_policy, restriction FROM federation_remotes LIMIT " <> show maxKnownNodes

data AddFederationRemoteResult = AddFederationRemoteSuccess | AddFederationRemoteMaxRemotesReached

addFederationRemote :: MonadClient m => FederationDomainConfig -> m AddFederationRemoteResult
addFederationRemote (FederationDomainConfig rDomain searchPolicy restriction) = do
  l <- length <$> getFederationRemotes
  if l >= maxKnownNodes
    then pure AddFederationRemoteMaxRemotesReached
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

updateFederationRemote :: MonadClient m => FederationDomainConfig -> m Bool
updateFederationRemote (FederationDomainConfig rDomain searchPolicy restriction) = do
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
    [] -> pure False
    [_] -> pure True
    _ -> throwM $ ErrorCall "Primary key violation detected federation_remotes"
  where
    updateConfig :: PrepQuery W (FederatedUserSearchPolicy, Int32, Domain) x
    updateConfig = "UPDATE federation_remotes SET search_policy = ?, restriction = ? WHERE domain = ? IF EXISTS"
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

deleteFederationRemote :: MonadClient m => Domain -> m ()
deleteFederationRemote rDomain =
  retry x1 $ do
    write deleteConfig p
    write dropTeams p
  where
    deleteConfig :: PrepQuery W (Identity Domain) ()
    deleteConfig = "DELETE FROM federation_remotes WHERE domain = ?"
    dropTeams :: PrepQuery W (Identity Domain) ()
    dropTeams = "DELETE FROM federation_remote_teams WHERE domain = ?"
    p = params LocalQuorum (Identity rDomain)

addFederationRemoteTeam :: MonadClient m => Domain -> FederationRemoteTeam -> m ()
addFederationRemoteTeam rDomain rteam =
  retry x1 $ write add (params LocalQuorum (rDomain, rteam.teamId))
  where
    add :: PrepQuery W (Domain, TeamId) ()
    add = "INSERT INTO federation_remote_teams (domain, team) VALUES (?, ?)"

getFederationRemoteTeams :: MonadClient m => Domain -> m [FederationRemoteTeam]
getFederationRemoteTeams rDomain = do
  fmap (FederationRemoteTeam . runIdentity) <$> retry x1 (query get (params LocalQuorum (Identity rDomain)))
  where
    get :: PrepQuery R (Identity Domain) (Identity TeamId)
    get = "SELECT team FROM federation_remote_teams WHERE domain = ?"

deleteFederationRemoteTeam :: MonadClient m => Domain -> TeamId -> m ()
deleteFederationRemoteTeam rDomain rteam =
  retry x1 $ write delete (params LocalQuorum (rDomain, rteam))
  where
    delete :: PrepQuery W (Domain, TeamId) ()
    delete = "DELETE FROM federation_remote_teams WHERE domain = ? AND team = ?"
