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
    AddFederationRemoteResult (..),
  )
where

import Brig.Data.Instances ()
import Cassandra
import Control.Exception (ErrorCall (ErrorCall))
import Control.Monad.Catch (throwM)
import Data.Domain
import Database.CQL.Protocol (SerialConsistency (LocalSerialConsistency), serialConsistency)
import Imports
import Wire.API.Routes.FederationDomainConfig
import Wire.API.User.Search

maxKnownNodes :: Int
maxKnownNodes = 10000

getFederationRemotes :: forall m. MonadClient m => m [FederationDomainConfig]
getFederationRemotes = uncurry FederationDomainConfig <$$> qry
  where
    qry :: m [(Domain, FederatedUserSearchPolicy)]
    qry = retry x1 . query get $ params LocalQuorum ()

    get :: PrepQuery R () (Domain, FederatedUserSearchPolicy)
    get = fromString $ "SELECT domain, search_policy FROM federation_remotes LIMIT " <> show maxKnownNodes

data AddFederationRemoteResult = AddFederationRemoteSuccess | AddFederationRemoteMaxRemotesReached

addFederationRemote :: MonadClient m => FederationDomainConfig -> m AddFederationRemoteResult
addFederationRemote (FederationDomainConfig rdom searchpolicy) = do
  l <- length <$> getFederationRemotes
  if l >= maxKnownNodes
    then pure AddFederationRemoteMaxRemotesReached
    else AddFederationRemoteSuccess <$ retry x5 (write add (params LocalQuorum (rdom, searchpolicy)))
  where
    add :: PrepQuery W (Domain, FederatedUserSearchPolicy) ()
    add = "INSERT INTO federation_remotes (domain, search_policy) VALUES (?, ?)"

updateFederationRemote :: MonadClient m => FederationDomainConfig -> m Bool
updateFederationRemote (FederationDomainConfig rdom spol) = do
  retry x1 (trans upd (params LocalQuorum (spol, rdom)) {serialConsistency = Just LocalSerialConsistency}) >>= \case
    [] -> pure False
    [_] -> pure True
    _ -> throwM $ ErrorCall "Primary key violation detected federation_remotes"
  where
    upd :: PrepQuery W (FederatedUserSearchPolicy, Domain) x
    upd = "UPDATE federation_remotes SET search_policy = ? WHERE domain = ? IF EXISTS"

deleteFederationRemote :: MonadClient m => Domain -> m ()
deleteFederationRemote rdom =
  retry x1 $ write delete (params LocalQuorum (Identity rdom))
  where
    delete :: PrepQuery W (Identity Domain) ()
    delete = "DELETE FROM federation_remotes WHERE domain = ?"
