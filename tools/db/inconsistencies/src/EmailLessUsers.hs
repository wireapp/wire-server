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

import Brig.Email
import Cassandra
import Cassandra.Util
import Conduit
import DanglingUserKeys qualified as K
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Either.Extra
import Data.Id
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Imports
import System.Logger
import System.Logger qualified as Log
import UnliftIO.Async
import Wire.API.User hiding (userEmail)

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
        .| C.mapM (liftIO . pooledMapConcurrentlyN 48 (checkUser l brig False))
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (BS.toStrict . Aeson.encode) . catMaybes)
        .| sinkFile inconsistenciesFile

runRepair :: Logger -> ClientState -> FilePath -> FilePath -> Bool -> IO ()
runRepair l brig inputFile outputFile repairData = do
  inputLines :: [UserId] <- mapMaybe (eitherToMaybe . parseIdFromText) . Text.lines <$> Text.readFile inputFile
  runResourceT $
    runConduit $
      zipSources
        (C.sourceList [(1 :: Int32) ..])
        (C.sourceList inputLines)
        .| C.mapM
          ( \(i, uid) -> do
              when (i `mod` 100 == 0) $
                Log.info l (Log.field "linesProcessed" i)
              liftIO $ repairUser l brig repairData uid
          )
        .| C.map ((<> "\n") . BS.toStrict . Aeson.encode)
        .| sinkFile outputFile

pageSize :: Int32
pageSize = 1000

data EmailInfo = EmailInfo
  { userId :: UserId,
    status :: WithWritetime AccountStatus,
    -- | Email in the user table
    userEmail :: WithWritetime Email,
    -- | Email in the user_keys table
    emailKey :: Maybe (WithWritetime UserId),
    inconsistencyCase :: Text
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

getUserDetails :: UserId -> Client (Maybe UserDetailsRow)
getUserDetails uid = retry x5 $ query1 cql (params LocalQuorum (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) UserDetailsRow
    cql = "SELECT id, status, writetime(status), email, writetime(email), activated from user where id = ?"

getUsers :: ConduitM () [UserDetailsRow] Client ()
getUsers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () UserDetailsRow
    cql = "SELECT id, status, writetime(status), email, writetime(email), activated from user"

type UserDetailsRow = (UserId, Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe Email, Maybe (Writetime Email), Bool)

insertMissingEmail :: Logger -> ClientState -> Email -> UserId -> IO ()
insertMissingEmail l brig email uid = do
  runClient brig $ K.insertKey l uid (mkEmailKey email)

userWithEmailAndStatus :: UserDetailsRow -> Maybe (UserId, AccountStatus, Writetime AccountStatus, Email, Writetime Email)
userWithEmailAndStatus (uid, mStatus, mStatusWritetime, mEmail, mEmailWritetime, activated) = do
  let act = if activated then Just True else Nothing
  case (,,,,) <$> mStatus <*> mStatusWritetime <*> mEmail <*> mEmailWritetime <*> act of
    Nothing -> Nothing
    Just (status, statusWritetime, email, emailWritetime, _) -> Just (uid, status, statusWritetime, email, emailWritetime)

repairUser :: Logger -> ClientState -> Bool -> UserId -> IO (Maybe EmailInfo)
repairUser l brig repairData uid = do
  user <- runClient brig $ getUserDetails uid
  let u = userWithEmailAndStatus =<< user
  case u of
    Nothing -> pure Nothing
    Just x -> checkUser l brig repairData x

checkUser :: Logger -> ClientState -> Bool -> (UserId, AccountStatus, Writetime AccountStatus, Email, Writetime Email) -> IO (Maybe EmailInfo)
checkUser l brig repairData (uid, statusValue, statusWritetime, userEmailValue, userEmailWriteTime) = do
  let status = WithWritetime statusValue statusWritetime
      userEmail = WithWritetime userEmailValue userEmailWriteTime
  mKeyDetails <- runClient brig $ K.getKey (mkEmailKey userEmailValue)
  case mKeyDetails of
    Nothing -> do
      let emailKey = Nothing
          inconsistencyCase = if statusValue == Active then "1-missing-email" else "2-missing-email-but-not-active"
      when (repairData && (statusValue == Active)) $ do
        insertMissingEmail l brig userEmailValue uid
      pure . Just $ EmailInfo {userId = uid, ..}
    Just (emailKeyValue, emailClaimTime) -> do
      let emailKey = Just $ WithWritetime emailKeyValue emailClaimTime
      let inconsistencyCase = "3-wrong-email"
      if emailKeyValue == uid
        then pure Nothing
        else pure . Just $ EmailInfo {userId = uid, ..}
