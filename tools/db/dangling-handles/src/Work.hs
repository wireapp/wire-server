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
import Cassandra.Util
import Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Handle
import Data.Id
import Imports
import System.Logger
import qualified System.Logger as Log
import UnliftIO.Async
import Wire.API.User.Identity
import Wire.API.User.Profile

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
      .| C.mapM_ (pooledMapConcurrentlyN_ 12 (\(handle, userId, claimTime) -> (checkUser l brig handle userId claimTime)))

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

getHandles :: ConduitM () [(Handle, UserId, Writetime UserId)] Client ()
getHandles = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (Handle, UserId, Writetime UserId)
    cql = "SELECT handle, user, writetime(user) from user_handle"

type UserDetailsRow = (Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe Handle, Maybe (Writetime Handle), Maybe TeamId, Maybe (Writetime TeamId), Maybe UserSSOId, Maybe (Writetime UserSSOId), Maybe ManagedBy, Maybe (Writetime ManagedBy))

getUserDetails :: UserId -> Client (Maybe UserDetailsRow)
getUserDetails uid = retry x1 $ query1 cql (params LocalQuorum (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) UserDetailsRow
    cql = "SELECT status, writetime(status), handle, writetime(handle), team, writetime(team), managed_by, writetime(managed_by) from user where id = ?"

checkUser :: Logger -> ClientState -> Handle -> UserId -> Writetime UserId -> IO ()
checkUser l brig handle uid claimTime = do
  let userLog = Log.field "user" (idToText uid)
      handleLog = Log.field "handle" (fromHandle handle)
      claimTimeLog = Log.field "handleClaimTime" (show (writeTimeToUTC claimTime))
  maybeDetails <- runClient brig $ getUserDetails uid
  case maybeDetails of
    Nothing ->
      Log.warn l (Log.msg (Log.val "No information found") . userLog . handleLog . claimTimeLog)
    Just (mStatus, mStatusWriteTime, mHandle, mHandleWriteTime, mTeam, mTeamWriteTime, mSSOId, mSSOIdWriteTime, mManagedBy, mManagedByWriteTime) -> do
      let teamLog = logMaybeWithWritetime "team" (idToText <$> mTeam) mTeamWriteTime
          statusLog = logMaybeWithWritetime "status" (show <$> mStatus) mStatusWriteTime
          userHandleLog = logMaybeWithWritetime "user.handle" (fromHandle <$> mHandle) mHandleWriteTime
          ssoLog = logMaybeWithWritetime "ssoId" (show <$> mSSOId) mSSOIdWriteTime
          managedByLog = logMaybeWithWritetime "managedBy" (show <$> mManagedBy) mManagedByWriteTime
          allFields = userLog . handleLog . teamLog . statusLog . userHandleLog . claimTimeLog . ssoLog . managedByLog
          statusError = case mStatus of
            Nothing ->
              Just $ Log.field "statusError" (Log.val "No user status found")
            Just Deleted ->
              Just $ Log.field "statusError" (Log.val "Handle found for deleted user")
            _ -> Nothing
          handleError = case mHandle of
            Nothing ->
              Just $ Log.field "handleError" (Log.val "user.handle is null")
            Just userHandle ->
              if userHandle /= handle
                then Just $ Log.field "handleError" (Log.val "user.handle is not same as claimed handle")
                else Nothing
      case catMaybes [statusError, handleError] of
        [] -> pure ()
        msgs -> do
          Log.warn l $ (foldr (.) id msgs) . allFields

logMaybe :: ToBytes a => ByteString -> Maybe a -> Msg -> Msg
logMaybe _ Nothing = id
logMaybe fieldName (Just x) = Log.field fieldName x

logMaybeWithWritetime :: ToBytes a => ByteString -> Maybe a -> Maybe (Writetime b) -> Msg -> Msg
logMaybeWithWritetime fieldName x t =
  logMaybe fieldName x
    . logMaybe (fieldName <> "-writetime") (show . writeTimeToUTC <$> t)
