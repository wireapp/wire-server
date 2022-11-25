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

module V2_MigrateMLSMembers where

import Cassandra
import Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Domain
import Data.Id
import Data.Map.Strict (lookup)
import qualified Data.Map.Strict as Map
import Galley.Cassandra.Instances ()
import Galley.DataMigration.Types
import Imports hiding (lookup)
import qualified System.Logger.Class as Log
import UnliftIO (pooledMapConcurrentlyN_)
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 2,
      text = "Migrating from member_client to mls_group_member_client",
      action =
        runConduit $
          zipSources
            (C.sourceList [(1 :: Int32) ..])
            getMemberClientsFromLegacy
            .| C.mapM_
              ( \(i, rows) -> do
                  Log.info (Log.field "Entries " (show (i * pageSize)))
                  let convIds = map rowConvId rows
                  m <- lookupGroupIds convIds
                  let newRows = flip mapMaybe rows $ \(conv, domain, uid, client, ref) ->
                        conv `lookup` m >>= \groupId -> pure (groupId, domain, uid, client, ref)
                  insertMemberClients newRows
              )
    }

rowConvId :: (ConvId, Domain, UserId, ClientId, KeyPackageRef) -> ConvId
rowConvId (conv, _, _, _, _) = conv

pageSize :: Int32
pageSize = 1000

getMemberClientsFromLegacy :: MonadClient m => ConduitM () [(ConvId, Domain, UserId, ClientId, KeyPackageRef)] m ()
getMemberClientsFromLegacy = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (ConvId, Domain, UserId, ClientId, KeyPackageRef)
    cql = "SELECT conv, user_domain, user, client, key_package_ref from member_client"

lookupGroupIds :: [ConvId] -> MigrationActionT IO (Map ConvId GroupId)
lookupGroupIds convIds = do
  rows <- pooledMapConcurrentlyN 8 (\convId -> retry x5 (query1 cql (params LocalQuorum (Identity convId)))) convIds
  rows' <-
    rows
      & mapM
        ( \case
            (Just (c, mg)) -> do
              case mg of
                Nothing -> do
                  Log.warn (Log.msg ("No group found for conv " <> show c))
                  pure Nothing
                Just g -> pure (Just (c, g))
            Nothing -> do
              Log.warn (Log.msg ("Conversation is missing for entry" :: Text))
              pure Nothing
        )

  rows'
    & catMaybes
    & Map.fromList
    & pure
  where
    cql :: PrepQuery R (Identity ConvId) (ConvId, Maybe GroupId)
    cql = "SELECT conv, group_id from conversation where conv = ?"

insertMemberClients :: (MonadUnliftIO m, MonadClient m) => [(GroupId, Domain, UserId, ClientId, KeyPackageRef)] -> m ()
insertMemberClients rows = do
  pooledMapConcurrentlyN_ 8 (\row -> retry x5 (write cql (params LocalQuorum row))) rows
  where
    cql :: PrepQuery W (GroupId, Domain, UserId, ClientId, KeyPackageRef) ()
    cql = "INSERT INTO mls_group_member_client (group_id, user_domain, user, client, key_package_ref) VALUES (?, ?, ?, ?, ?)"
