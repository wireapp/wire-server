-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module V1_DropPhone (migration) where

import Brig.DataMigration.Types
import Cassandra
import Data.Conduit
import Data.Conduit.Combinators qualified as C
import Data.Id
import Imports
import System.Logger.Class qualified as Log

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 1,
      text = "Clear the excluded_phones table",
      action = do
        dropExcludedPhonesTable
        runConduit
          ( getUsers
              .| C.mapM
                ( \r ->
                    Log.info (Log.field "users with phone" (intercalate ", " (show . fst <$> r)))
                      >> pure r
                )
              .| C.concatMap (filter (isJust . snd))
              .| C.map fst
              .| C.mapM_ dropPhoneFromUser
          )
    }

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

-- | Drop the 'excluded_phones' Brig table
dropExcludedPhonesTable :: MonadClient m => m ()
dropExcludedPhonesTable = retry x1 . write cql $ paramsP LocalQuorum () pageSize
  where
    cql :: PrepQuery W () ()
    cql = "DELETE * FROM excluded_phones"

-- | Get users
getUsers :: MonadClient m => ConduitM () [(UserId, Maybe Text)] m ()
getUsers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (UserId, Maybe Text)
    cql = "SELECT id, phone FROM user"

-- | TODO(md): This should probably drop the whole user altogether.
--
-- Drop the phone column in the user table. The rows for which this is done
-- are selected by a filter on rows that have data in the column.
dropPhoneFromUser :: MonadClient m => UserId -> m ()
dropPhoneFromUser =
  retry x5
    . write cql
    . params LocalQuorum
    . Identity
  where
    cql :: PrepQuery W (Identity UserId) ()
    cql = "DELETE phone FROM user WHERE id = ?"
