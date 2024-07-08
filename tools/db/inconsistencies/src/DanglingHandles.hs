{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module DanglingHandles where

import Cassandra
import Cassandra.Util
import Conduit
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Handle
import Data.Id
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Imports
import System.Logger
import System.Logger qualified as Log
import UnliftIO.Async
import Wire.API.User qualified as WU

runCommand :: Logger -> ClientState -> FilePath -> IO ()
runCommand l brig inconsistenciesFile = do
  runResourceT $
    runConduit $
      zipSources
        (C.sourceList [(1 :: Int32) ..])
        (transPipe (runClient brig) getHandles)
        .| C.mapM
          ( \(i, userHandles) -> do
              Log.info l (Log.field "userIds" (show ((i - 1) * pageSize + fromIntegral (length userHandles))))
              pure userHandles
          )
        .| C.mapM (liftIO . pooledMapConcurrentlyN 48 (\(handle, userId, claimTime) -> checkUser l brig handle userId claimTime False))
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (BS.toStrict . Aeson.encode) . catMaybes)
        .| sinkFile inconsistenciesFile

examineHandles :: Logger -> ClientState -> FilePath -> FilePath -> Bool -> IO ()
examineHandles l brig handlesFile errorsFile fixClaim = do
  handles <- mapMaybe parseHandle . Text.lines <$> Text.readFile handlesFile
  runResourceT $
    runConduit $
      zipSources
        (C.sourceList [(1 :: Int32) ..])
        (C.sourceList handles)
        .| C.mapM
          ( \(i, handle) -> do
              when (i `mod` 100 == 0) $
                Log.info l (Log.field "handlesProcesses" i)
              liftIO $ checkHandle l brig handle fixClaim
          )
        .| C.map ((<> "\n") . BS.toStrict . Aeson.encode)
        .| sinkFile errorsFile

pageSize :: Int32
pageSize = 1000

data HandleInfo = HandleInfo
  { -- | Handle in the user_handle table
    claimedHandle :: Handle,
    userId :: UserId,
    handleClaimTime :: Writetime Handle,
    status :: Maybe (WithWritetime WU.AccountStatus),
    -- | Handle in the user table
    userHandle :: Maybe (WithWritetime Handle)
  }
  deriving (Generic)

instance Aeson.ToJSON HandleInfo

data WithWritetime a = WithWritetime
  { value :: a,
    writetime :: Writetime a
  }
  deriving (Generic)

instance (Aeson.ToJSON a) => Aeson.ToJSON (WithWritetime a)

----------------------------------------------------------------------------
-- Queries

getHandle :: Handle -> Client (Maybe (UserId, Writetime UserId))
getHandle handle = retry x1 $ query1 cql (params LocalQuorum (Identity handle))
  where
    cql :: PrepQuery R (Identity Handle) (UserId, Writetime UserId)
    cql = "SELECT user, writetime(user) from user_handle where handle = ?"

getHandles :: ConduitM () [(Handle, UserId, Writetime UserId)] Client ()
getHandles = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (Handle, UserId, Writetime UserId)
    cql = "SELECT handle, user, writetime(user) from user_handle"

type UserDetailsRow = (Maybe WU.AccountStatus, Maybe (Writetime WU.AccountStatus), Maybe Handle, Maybe (Writetime Handle))

getUserDetails :: UserId -> Client (Maybe UserDetailsRow)
getUserDetails uid = retry x1 $ query1 cql (params LocalQuorum (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) UserDetailsRow
    cql = "SELECT status, writetime(status), handle, writetime(handle) from user where id = ?"

checkHandle :: Logger -> ClientState -> Handle -> Bool -> IO (Maybe HandleInfo)
checkHandle l brig handle fixHandle = do
  mUser <- runClient brig $ getHandle handle
  case mUser of
    Nothing -> do
      Log.warn l (Log.msg (Log.val "No user found for handle") . Log.field "handle" (fromHandle handle))
      pure Nothing
    Just (uid, claimTime) -> checkUser l brig handle uid claimTime fixHandle

freeHandle :: Logger -> Handle -> Client ()
freeHandle l handle = do
  Log.info l $ Log.msg (Log.val "Freeing handle") . Log.field "handle" (fromHandle handle)
  retry x5 $ write handleDelete (params LocalQuorum (Identity handle))
  where
    handleDelete :: PrepQuery W (Identity Handle) ()
    handleDelete = "DELETE FROM user_handle WHERE handle = ?"

checkUser :: Logger -> ClientState -> Handle -> UserId -> Writetime UserId -> Bool -> IO (Maybe HandleInfo)
checkUser l brig claimedHandle userId handleClaimTime' fixClaim = do
  maybeDetails <- runClient brig $ getUserDetails userId
  let handleClaimTime = Writetime . writetimeToUTC $ handleClaimTime'
  case maybeDetails of
    Nothing -> do
      let status = Nothing
          userHandle = Nothing
      when fixClaim $
        runClient brig $
          freeHandle l claimedHandle
      pure . Just $ HandleInfo {..}
    Just (mStatus, mStatusWriteTime, mHandle, mHandleWriteTime) -> do
      let status = WithWritetime <$> mStatus <*> mStatusWriteTime
          userHandle = WithWritetime <$> mHandle <*> mHandleWriteTime
          statusError = case mStatus of
            Nothing -> True
            Just WU.Deleted -> True
            _ -> False
          handleError = mHandle /= Just claimedHandle
      if statusError || handleError
        then do
          when fixClaim $
            runClient brig $
              freeHandle l claimedHandle
          pure . Just $ HandleInfo {..}
        else pure Nothing
