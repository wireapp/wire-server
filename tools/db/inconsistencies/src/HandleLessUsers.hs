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

module HandleLessUsers where

import Cassandra
import Cassandra.Util
import Conduit
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Handle
import Data.Id
import Imports
import System.Logger
import System.Logger qualified as Log
import UnliftIO.Async
import Wire.API.User (AccountStatus (..))

runCommand :: Logger -> ClientState -> FilePath -> IO ()
runCommand l brig inconsistenciesFile = do
  runResourceT $
    runConduit $
      zipSources
        (C.sourceList [(1 :: Int32) ..])
        (transPipe (runClient brig) getUsers)
        .| C.mapM
          ( \(i, userDetails) -> do
              Log.info l (Log.field "userIds" (show ((i - 1) * pageSize + fromIntegral (length userDetails))))
              pure $ mapMaybe userWithHandleAndStatus userDetails
          )
        .| C.mapM (liftIO . pooledMapConcurrentlyN 48 (checkUser brig))
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (BS.toStrict . Aeson.encode) . catMaybes)
        .| sinkFile inconsistenciesFile

pageSize :: Int32
pageSize = 1000

data HandleInfo = HandleInfo
  { userId :: UserId,
    status :: WithWritetime AccountStatus,
    -- | Handle in the user table
    userHandle :: WithWritetime Handle,
    handleClaimUser :: Maybe (WithWritetime UserId)
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

getUsers :: ConduitM () [UserDetailsRow] Client ()
getUsers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () UserDetailsRow
    cql = "SELECT id, status, writetime(status), handle, writetime(handle) from user"

type UserDetailsRow = (UserId, Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe Handle, Maybe (Writetime Handle))

userWithHandleAndStatus :: UserDetailsRow -> Maybe (UserId, AccountStatus, Writetime AccountStatus, Handle, Writetime Handle)
userWithHandleAndStatus (uid, mStatus, mStatusWritetime, mHandle, mHandleWritetime) =
  case (,,,) <$> mStatus <*> mStatusWritetime <*> mHandle <*> mHandleWritetime of
    Nothing -> Nothing
    Just (status, statusWritetime, handle, handleWritetime) -> Just (uid, status, statusWritetime, handle, handleWritetime)

checkUser :: ClientState -> (UserId, AccountStatus, Writetime AccountStatus, Handle, Writetime Handle) -> IO (Maybe HandleInfo)
checkUser brig (userId, statusValue, statusWritetime, userHandleValue, userHandleWriteTime) = do
  let status = WithWritetime statusValue statusWritetime
      userHandle = WithWritetime userHandleValue userHandleWriteTime
  mClaimDetails <- runClient brig $ getHandle userHandleValue
  case mClaimDetails of
    Nothing ->
      let handleClaimUser = Nothing
       in pure . Just $ HandleInfo {..}
    Just (handleClaimUserValue, handleClaimTime) -> do
      let handleClaimUser = Just $ WithWritetime handleClaimUserValue handleClaimTime
      if handleClaimUserValue == userId
        then pure Nothing
        else pure . Just $ HandleInfo {..}
