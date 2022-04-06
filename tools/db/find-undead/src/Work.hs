{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Work where

import Cassandra
import Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log

runCommand :: Logger -> ClientState -> IO ()
runCommand l brig = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient brig) getUser)
      .| C.mapM_
        ( \(i, users) -> do
            Log.info l (Log.field "number of users processed" (show (i * pageSize)))
            mapM_ (validate l) users
        )

pageSize :: Int32
pageSize = 2000

type UserRow = (UserId, Maybe Text, Maybe Bool, Maybe Int32)

getUser :: ConduitM () [UserRow] Client ()
getUser = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () UserRow
    cql = "select id, name, activated, accent_id from user"

newtype ShowUserRow = ShowUserRow UserRow

instance Show ShowUserRow where
  show (ShowUserRow (uid, name, activated, accent)) =
    "id: " <> show uid
      <> ", name: "
      <> show (fromMaybe "null" name)
      <> ", activated: "
      <> show (maybe "null" show activated)
      <> ", color: "
      <> show (maybe "null" show accent)

validate :: Logger -> UserRow -> IO ()
validate l r@(_, name, activated, colorId) = do
  when (any isNothing [name $> (), activated $> (), colorId $> ()]) $
    Log.err l $ Log.field "user" (show (ShowUserRow r))
