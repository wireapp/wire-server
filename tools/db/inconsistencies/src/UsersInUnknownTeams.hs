{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module UsersInUnknownTeams where

import Cassandra
import Cassandra.Util
import Conduit
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Id
import Imports
import System.Logger (Logger)
import System.Logger qualified as Log
import UnliftIO (pooledMapConcurrentlyN)
import Wire.API.Team.Permission
import Wire.API.User (AccountStatus)

runCommand :: Logger -> FilePath -> ClientState -> ClientState -> IO ()
runCommand l inconsistenciesFile casBrig casGalley = do
  runResourceT $
    runConduit $
      zipSources
        (C.sourceList [(1 :: Int32) ..])
        (transPipe (runClient casBrig) getUsers)
        .| C.mapM
          ( \(i, userDetailsRow) -> do
              let userDetails = map mkUserDetails userDetailsRow
              Log.info l (Log.field "userIds" (show ((i - 1) * pageSize + fromIntegral (length userDetails))))
              pure $
                mapMaybe
                  ( \user -> case user.teamId of
                      Nothing -> Nothing
                      Just tid -> Just (user, tid.value)
                  )
                  userDetails
          )
        .| C.mapM (pooledMapConcurrentlyN 48 (liftIO . checkUser l casBrig casGalley))
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (BS.toStrict . Aeson.encode) . catMaybes)
        .| sinkFile inconsistenciesFile

data InconsistentData = InconsistentData
  { user :: UserDetails,
    perms :: Maybe (WithWritetime Permissions),
    clients :: [ClientId],
    connections :: [UserId],
    admins :: [UserId]
  }
  deriving (Generic)

instance ToJSON InconsistentData

checkUser :: Logger -> ClientState -> ClientState -> (UserDetails, TeamId) -> IO (Maybe InconsistentData)
checkUser l casBrig casGalley (user, tid) = do
  mTeam <- runClient casGalley $ getTeam tid
  case mTeam of
    Just _ -> pure Nothing
    Nothing -> do
      Log.warn l $
        Log.msg (Log.val "team not found")
          . Log.field "team" (idToText tid)
          . Log.field "user" (idToText user.id_)
      mMember <- runClient casGalley $ getTeamMember tid user.id_
      let perms = case mMember of
            Nothing -> Nothing
            Just (p, writeTime) -> WithWritetime <$> p <*> writeTime
      admins <- runClient casGalley $ getTeamAdmins tid
      clients <- runClient casBrig $ getClients user.id_
      connections <- runClient casBrig $ getConnections user.id_
      pure . Just $ InconsistentData {..}

-- CQL

pageSize :: Int32
pageSize = 10000

getUsers :: ConduitM () [UserDetailsRow] Client ()
getUsers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () UserDetailsRow
    cql = "SELECT id, activated, status, writetime(status), team, writetime(team) from user"

getClients :: UserId -> Client [ClientId]
getClients uid = runIdentity <$$> query cql (params One (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) (Identity ClientId)
    cql = "SELECT client from clients where  user = ?"

getConnections :: UserId -> Client [UserId]
getConnections uid = runIdentity <$$> query cql (params One (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) (Identity UserId)
    cql = "SELECT right from connection where left = ?"

getTeamMember :: TeamId -> UserId -> Client (Maybe TeamMemberRow)
getTeamMember tid uid = query1 cql (params One (tid, uid))
  where
    cql :: PrepQuery R (TeamId, UserId) TeamMemberRow
    cql = "SELECT perms, writetime(perms) from team_member where team = ? AND user = ?"

getTeamAdmins :: TeamId -> Client [UserId]
getTeamAdmins tid = runIdentity <$$> query cql (params One (Identity tid))
  where
    cql :: PrepQuery R (Identity TeamId) (Identity UserId)
    cql = "SELECT user from team_admin where team = ?"

getTeam :: TeamId -> Client (Maybe TeamRow)
getTeam tid = query1 cql (params One (Identity tid))
  where
    cql :: PrepQuery R (Identity TeamId) TeamRow
    cql = "SELECT binding, creator, deleted, name, search_visibility, status from team where team = ?"

type UserDetailsRow = (UserId, Maybe Bool, Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe TeamId, Maybe (Writetime TeamId))

data WithWritetime a = WithWritetime
  { value :: a,
    writetime :: Writetime a
  }
  deriving (Generic)

instance (ToJSON a) => ToJSON (WithWritetime a)

data UserDetails = UserDetails
  { id_ :: UserId,
    activated :: Maybe Bool,
    accountStatus :: Maybe (WithWritetime AccountStatus),
    teamId :: Maybe (WithWritetime TeamId)
  }
  deriving (Generic)

instance ToJSON UserDetails

mkUserDetails :: UserDetailsRow -> UserDetails
mkUserDetails (uid, activated, accountStatus, accountStateWrite, teamId, teamIdWrite) =
  UserDetails
    { id_ = uid,
      activated = activated,
      accountStatus = WithWritetime <$> accountStatus <*> accountStateWrite,
      teamId = WithWritetime <$> teamId <*> teamIdWrite
    }

type TeamMemberRow = (Maybe Permissions, Maybe (Writetime Permissions))

type TeamRow = (Maybe Bool, Maybe UserId, Maybe Bool, Maybe Text, Maybe Int32, Maybe Int32)

data TeamDetails = TeamDetails
  { binding :: Maybe Bool,
    creator :: Maybe UserId,
    deleted :: Maybe Bool,
    name :: Maybe Text,
    searchVisibility :: Maybe Int32,
    status :: Maybe Int32
  }

mkTeamDetails :: TeamRow -> TeamDetails
mkTeamDetails (binding, creator, deleted, name, searchVisibility, status) =
  TeamDetails {..}
