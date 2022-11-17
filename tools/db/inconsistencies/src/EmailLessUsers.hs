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

module EmailLessUsers where

import Brig.Data.Instances ()
import qualified DanglingUserKeys as K
import Brig.Email
import Brig.Types.Intra
import Cassandra
import Cassandra.Util
import Conduit
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import Data.String.Conversions (cs)
import Imports
import System.Logger
import qualified System.Logger as Log
import UnliftIO.Async
import Brig.Data.UserKey

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
              pure $ mapMaybe userWithEmailAndStatus userDetails
          )
        .| C.mapM (liftIO . pooledMapConcurrentlyN 48 (checkUser brig))
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (cs . Aeson.encode) . catMaybes)
        .| sinkFile inconsistenciesFile

pageSize :: Int32
pageSize = 1000

data EmailInfo = EmailInfo
  { userId :: UserId,
    status :: WithWritetime AccountStatus,
    -- | Email in the user table
    userEmail :: WithWritetime Email,
    -- | Email in the user_keys table
    emailKey :: Maybe (WithWritetime UserId)
  }
  deriving (Generic)

instance Aeson.ToJSON EmailInfo

data WithWritetime a = WithWritetime
  { value :: a,
    writetime :: Writetime a
  }
  deriving (Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (WithWritetime a)

----------------------------------------------------------------------------
-- Queries

getUsers :: ConduitM () [UserDetailsRow] Client ()
getUsers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () UserDetailsRow
    cql = "SELECT id, status, writetime(status), email, writetime(email), activated from user"

type UserDetailsRow = (UserId, Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe Email, Maybe (Writetime Email), Bool)

userWithEmailAndStatus :: UserDetailsRow -> Maybe (UserId, AccountStatus, Writetime AccountStatus, Email, Writetime Email)
userWithEmailAndStatus (uid, mStatus, mStatusWritetime, mEmail, mEmailWritetime, activated) = do
  let act = if activated then Just True else Nothing 
  case (,,,,) <$> mStatus <*> mStatusWritetime <*> mEmail <*> mEmailWritetime <*> act of
    Nothing -> Nothing
    Just (status, statusWritetime, email, emailWritetime, _) -> Just (uid, status, statusWritetime, email, emailWritetime)

checkUser :: ClientState -> (UserId, AccountStatus, Writetime AccountStatus, Email, Writetime Email) -> IO (Maybe EmailInfo)
checkUser brig (userId, statusValue, statusWritetime, userEmailValue, userEmailWriteTime) = do
  let status = WithWritetime statusValue statusWritetime
      userEmail = WithWritetime userEmailValue userEmailWriteTime
  mKeyDetails <- runClient brig $ K.getKey (userEmailKey userEmailValue)
  case mKeyDetails of
    Nothing ->
      let emailKey = Nothing
       in pure . Just $ EmailInfo {..}
    Just (emailKeyValue, emailClaimTime) -> do
      let emailKey = Just $ WithWritetime emailKeyValue emailClaimTime
      if emailKeyValue == userId
        then pure Nothing
        else pure . Just $ EmailInfo {..}
