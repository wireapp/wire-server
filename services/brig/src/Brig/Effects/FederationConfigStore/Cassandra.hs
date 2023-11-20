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

module Brig.Effects.FederationConfigStore.Cassandra
  ( interpretFederationDomainConfig,
    AddFederationRemoteResult (..),
  )
where

import Brig.Data.Instances ()
import Brig.Effects.FederationConfigStore
import Cassandra
import Control.Exception (ErrorCall (ErrorCall))
import Control.Monad.Catch (throwM)
import Data.Domain
import Data.Id
import Database.CQL.Protocol (SerialConsistency (LocalSerialConsistency), serialConsistency)
import Imports
import Polysemy
import Wire.API.Routes.FederationDomainConfig qualified as API
import Wire.API.User.Search

interpretFederationDomainConfig :: forall m r a. (MonadClient m, Member (Embed m) r) => Sem (FederationConfigStore ': r) a -> Sem r a
interpretFederationDomainConfig =
  interpret $
    embed @m . \case
      GetFederationConfig d -> getFederationConfig' d
      GetFederationConfigs -> getFederationConfigs'
      AddFederationConfig cnf -> addFederationConfig' cnf
      UpdateFederationConfig cnf -> updateFederationConfig' cnf
      AddFederationRemoteTeam d t -> addFederationRemoteTeam' d t
      RemoveFederationRemoteTeam d t -> removeFederationRemoteTeam' d t
      GetFederationRemoteTeams d -> getFederationRemoteTeams' d

getFederationConfigs' :: forall m. MonadClient m => m [FederationDomainConfig]
getFederationConfigs' = do
  xs <- getFederationRemotes
  forM xs $ \case
    API.FederationDomainConfig d p API.FederationRestrictionAllowAll ->
      pure $ FederationDomainConfig d p FederationRestrictionAllowAll
    API.FederationDomainConfig d p API.FederationRestrictionByTeam ->
      FederationDomainConfig d p . FederationRestrictionByTeam . fmap API.teamId <$> getFederationRemoteTeams' d

maxKnownNodes :: Int
maxKnownNodes = 10000

getFederationConfig' :: MonadClient m => Domain -> m (Maybe FederationDomainConfig)
getFederationConfig' rDomain = do
  mCnf <- retry x1 (query1 q (params LocalQuorum (Identity rDomain)))
  teams <- fmap API.teamId <$> getFederationRemoteTeams' rDomain
  pure $
    mCnf <&> \case
      (sp, API.FederationRestrictionAllowAll) -> FederationDomainConfig rDomain sp FederationRestrictionAllowAll
      (sp, API.FederationRestrictionByTeam) -> FederationDomainConfig rDomain sp (FederationRestrictionByTeam teams)
  where
    q :: PrepQuery R (Identity Domain) (FederatedUserSearchPolicy, API.FederationRestriction)
    q = "SELECT search_policy, restriction FROM federation_remotes WHERE domain = ?"

getFederationRemotes :: forall m. MonadClient m => m [API.FederationDomainConfig]
getFederationRemotes = (\(d, p, r) -> API.FederationDomainConfig d p r) <$$> qry
  where
    qry :: m [(Domain, FederatedUserSearchPolicy, API.FederationRestriction)]
    qry = retry x1 . query get $ params LocalQuorum ()

    get :: PrepQuery R () (Domain, FederatedUserSearchPolicy, API.FederationRestriction)
    get = fromString $ "SELECT domain, search_policy, restriction FROM federation_remotes LIMIT " <> show maxKnownNodes

addFederationConfig' :: MonadClient m => API.FederationDomainConfig -> m AddFederationRemoteResult
addFederationConfig' (API.FederationDomainConfig rDomain searchPolicy restriction) = do
  l <- length <$> getFederationRemotes
  if l >= maxKnownNodes
    then pure AddFederationRemoteMaxRemotesReached
    else AddFederationRemoteSuccess <$ retry x5 (write add (params LocalQuorum (rDomain, searchPolicy, restriction)))
  where
    add :: PrepQuery W (Domain, FederatedUserSearchPolicy, API.FederationRestriction) ()
    add = "INSERT INTO federation_remotes (domain, search_policy, restriction) VALUES (?, ?, ?)"

updateFederationConfig' :: MonadClient m => API.FederationDomainConfig -> m Bool
updateFederationConfig' (API.FederationDomainConfig rDomain searchPolicy restriction) = do
  retry x1 (trans upd (params LocalQuorum (searchPolicy, restriction, rDomain)) {serialConsistency = Just LocalSerialConsistency}) >>= \case
    [] -> pure False
    [_] -> pure True
    _ -> throwM $ ErrorCall "Primary key violation detected federation_remotes"
  where
    upd :: PrepQuery W (FederatedUserSearchPolicy, API.FederationRestriction, Domain) x
    upd = "UPDATE federation_remotes SET search_policy = ?, restriction = ? WHERE domain = ? IF EXISTS"

addFederationRemoteTeam' :: MonadClient m => Domain -> TeamId -> m ()
addFederationRemoteTeam' rDomain tid =
  retry x1 $ write add (params LocalQuorum (rDomain, tid))
  where
    add :: PrepQuery W (Domain, TeamId) ()
    add = "INSERT INTO federation_remote_teams (domain, team) VALUES (?, ?)"

getFederationRemoteTeams' :: MonadClient m => Domain -> m [API.FederationRemoteTeam]
getFederationRemoteTeams' rDomain = do
  fmap (API.FederationRemoteTeam . runIdentity) <$> retry x1 (query get (params LocalQuorum (Identity rDomain)))
  where
    get :: PrepQuery R (Identity Domain) (Identity TeamId)
    get = "SELECT team FROM federation_remote_teams WHERE domain = ?"

removeFederationRemoteTeam' :: MonadClient m => Domain -> TeamId -> m ()
removeFederationRemoteTeam' rDomain rteam =
  retry x1 $ write delete (params LocalQuorum (rDomain, rteam))
  where
    delete :: PrepQuery W (Domain, TeamId) ()
    delete = "DELETE FROM federation_remote_teams WHERE domain = ? AND team = ?"
