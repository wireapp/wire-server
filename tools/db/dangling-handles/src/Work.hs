{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Brig.Data.Instances ()
import Brig.Types.Intra
import Cassandra
import Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Handle
import Data.Id
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log

runCommand :: Logger -> ClientState -> IO ()
runCommand l brig = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient brig) getHandles)
      .| C.mapM
        ( \(i, userHandles) -> do
            Log.info l (Log.field "userIds" (show ((i - 1) * pageSize + fromIntegral (length userHandles))))
            pure userHandles
        )
      .| C.mapM_ (mapM_ (uncurry (checkUser l brig)))

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

getHandles :: ConduitM () [(Handle, UserId)] Client ()
getHandles = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (Handle, UserId)
    cql = "SELECT handle, user from user_handle"

getUserDetails :: UserId -> Client (Maybe (Maybe AccountStatus, Maybe Handle, Maybe TeamId))
getUserDetails uid = retry x1 $ query1 cql (params LocalQuorum (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) (Maybe AccountStatus, Maybe Handle, Maybe TeamId)
    cql = "SELECT status handle team from user where uid = ?"

checkUser :: Logger -> ClientState -> Handle -> UserId -> IO ()
checkUser l brig handle uid = do
  let userLog = Log.field "user" (idToText uid)
      handleLog = Log.field "handle" (fromHandle handle)
  maybeDetails <- runClient brig $ getUserDetails uid
  case maybeDetails of
    Nothing ->
      Log.warn l (Log.msg (Log.val "No information found") . userLog . handleLog)
    Just (mStatus, mHandle, mTeam) -> do
      let teamLog = maybe id (\t -> Log.field "team" (idToText t)) mTeam
          statusLog = maybe id (\status -> Log.field "status" (show status)) mStatus
          userHandleLog = maybe id (\h -> Log.field "user.handle" (fromHandle h)) mHandle
          allFields = userLog . handleLog . teamLog . statusLog . userHandleLog
      case mStatus of
        Nothing ->
          Log.warn l $
            Log.msg (Log.val "No user status found")
              . allFields
        Just Deleted -> do
          Log.warn l $
            Log.msg (Log.val "Handle found for deleted user")
              . allFields
        _ -> pure ()
      case mHandle of
        Nothing ->
          Log.warn l $ Log.msg (Log.val "user.handle is null") . allFields
        Just userHandle ->
          if userHandle /= handle
            then Log.warn l $ Log.msg (Log.val "user.handle is not same as claimed handle") . allFields
            else pure ()
