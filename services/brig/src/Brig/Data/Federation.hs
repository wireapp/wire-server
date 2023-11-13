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

getFederationRemotes :: forall m. MonadClient m => m [FederationDomainConfig]
getFederationRemotes = (\(d, p, r) -> FederationDomainConfig d p r) <$$> qry
  where
    qry :: m [(Domain, FederatedUserSearchPolicy, FederationRestriction)]
    qry = retry x1 . query get $ params LocalQuorum ()

    get :: PrepQuery R () (Domain, FederatedUserSearchPolicy, FederationRestriction)
    get = fromString $ "SELECT domain, search_policy, restriction FROM federation_remotes LIMIT " <> show maxKnownNodes

data AddFederationRemoteResult = AddFederationRemoteSuccess | AddFederationRemoteMaxRemotesReached

addFederationRemote :: MonadClient m => FederationDomainConfig -> m AddFederationRemoteResult
addFederationRemote (FederationDomainConfig rDomain searchPolicy restriction) = do
  l <- length <$> getFederationRemotes
  if l >= maxKnownNodes
    then pure AddFederationRemoteMaxRemotesReached
    else AddFederationRemoteSuccess <$ retry x5 (write add (params LocalQuorum (rDomain, searchPolicy, restriction)))
  where
    add :: PrepQuery W (Domain, FederatedUserSearchPolicy, FederationRestriction) ()
    add = "INSERT INTO federation_remotes (domain, search_policy, restriction) VALUES (?, ?, ?)"

updateFederationRemote :: MonadClient m => FederationDomainConfig -> m Bool
updateFederationRemote (FederationDomainConfig rDomain searchPolicy restriction) = do
  retry x1 (trans upd (params LocalQuorum (searchPolicy, restriction, rDomain)) {serialConsistency = Just LocalSerialConsistency}) >>= \case
    [] -> pure False
    [_] -> pure True
    _ -> throwM $ ErrorCall "Primary key violation detected federation_remotes"
  where
    upd :: PrepQuery W (FederatedUserSearchPolicy, FederationRestriction, Domain) x
    upd = "UPDATE federation_remotes SET search_policy = ?, restriction = ? WHERE domain = ? IF EXISTS"

deleteFederationRemote :: MonadClient m => Domain -> m ()
deleteFederationRemote rDomain =
  retry x1 $ write delete (params LocalQuorum (Identity rDomain))
  where
    delete :: PrepQuery W (Identity Domain) ()
    delete = "DELETE FROM federation_remotes WHERE domain = ?"

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
