module Galley.Cassandra.Connection where

import Cassandra (MonadClient, PrepQuery, W, params, Consistency (LocalQuorum), retry, x1, write)
import Imports
import Data.Domain
import Galley.Cassandra.Instances ()

-- Queries targeting this table are usually in Brig, but I've put this one
-- here so that we don't have yet another network call to Brig when most
-- everything is already happening in galley

deleteRemoteConnectionsByDomain
  :: MonadClient m
  => Domain
  -> m ()
deleteRemoteConnectionsByDomain domain =
  retry x1 . write remoteConnectionsDeleteByDomain $ params LocalQuorum $ pure domain

remoteConnectionsDeleteByDomain :: PrepQuery W (Identity Domain) ()
remoteConnectionsDeleteByDomain = "DELETE FROM connection_remote where right_domain = ?"