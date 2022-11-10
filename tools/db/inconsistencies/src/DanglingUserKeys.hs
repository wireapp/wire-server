{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module DanglingUserKeys where

import Brig.Data.Instances ()
import Brig.Data.UserKey
import Brig.Email (EmailKey (..), mkEmailKey)
import Brig.Phone (PhoneKey (..), mkPhoneKey)
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
import Wire.API.User hiding (userEmail, userPhone)

runCommand :: Logger -> ClientState -> FilePath -> IO ()
runCommand l brig inconsistenciesFile = do
  runResourceT $
    runConduit $
      zipSources
        (C.sourceList [(1 :: Int32) ..])
        (transPipe (runClient brig) getKeys)
        .| C.mapM
          ( \(i, userKeys) -> do
              Log.info l (Log.field "keys" (show ((i - 1) * pageSize + fromIntegral (length userKeys))))
              pure userKeys
          )
        .| C.mapM (liftIO . pooledMapConcurrentlyN 48 (\(key, userId, claimTime) -> checkUser l brig key userId claimTime False))
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (cs . Aeson.encode) . catMaybes)
        .| sinkFile inconsistenciesFile

-- examineKeys :: Logger -> ClientState -> FilePath -> FilePath -> Bool -> IO ()
-- examineKeys l brig keysFile errorsFile repairData = do
--   keys <- mapMaybe parseHandle . Text.lines <$> Text.readFile keysFile
--   runResourceT $
--     runConduit $
--       zipSources
--         (C.sourceList [(1 :: Int32) ..])
--         (C.sourceList keys)
--         .| C.mapM
--           ( \(i, key) -> do
--               when (i `mod` 100 == 0) $
--                 Log.info l (Log.field "keysProcesses" i)
--               liftIO $ checkKey l brig key repairData
--           )
--         .| C.map ((<> "\n") . cs . Aeson.encode)
--         .| sinkFile errorsFile

pageSize :: Int32
pageSize = 1000

data Inconsistency = Inconsistency
  { -- | Handle in the user_handle table
    key :: UserKey,
    userId :: UserId,
    time :: Writetime UserId,
    status :: Maybe (WithWritetime AccountStatus),
    userEmail :: Maybe (WithWritetime Email),
    userPhone :: Maybe (WithWritetime Phone)
  }
  deriving (Generic)

instance Aeson.ToJSON Inconsistency

data WithWritetime a = WithWritetime
  { value :: a,
    writetime :: Writetime a
  }
  deriving (Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (WithWritetime a)

----------------------------------------------------------------------------
-- Queries

-- getKey :: UserKey -> Client (Maybe (UserId, Writetime UserId))
-- getKey key = retry x1 $ query1 cql (params LocalQuorum (Identity key))
--   where
--     cql :: PrepQuery R (Identity UserKey) (UserId, Writetime UserId)
--     cql = "SELECT user, writetime(user) from user_keys where key = ?"

getKeys :: ConduitM () [(UserKey, UserId, Writetime UserId)] Client ()
getKeys = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (UserKey, UserId, Writetime UserId)
    cql = "SELECT key, user, writetime(user) from user_keys"

instance Cql UserKey where
  ctype = Tagged TextColumn

  fromCql (CqlText t) =
    maybe
      (Left $ "invalid userkey: " <> show t)
      Right
      ((userEmailKey <$> parseEmail t) <|> (userPhoneKey <$> parsePhone t))
  fromCql _ = Left "userkey: expected text"

  toCql k = toCql $ keyText k

instance Aeson.ToJSON UserKey where
  toJSON = Aeson.toJSON . keyText

type UserDetailsRow = (Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe Email, Maybe (Writetime Email), Maybe Phone, Maybe (Writetime Phone))

getUserDetails :: UserId -> Client (Maybe UserDetailsRow)
getUserDetails uid = retry x1 $ query1 cql (params LocalQuorum (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) UserDetailsRow
    cql = "SELECT status, writetime(status), email, writetime(email), phone, writetime(phone) from user where id = ?"

-- checkKey :: Logger -> ClientState -> UserKey -> Bool -> IO (Maybe HandleInfo)
-- checkKey l brig key repairData = do
--   mUser <- runClient brig $ getKey key
--   case mUser of
--     Nothing -> do
--       Log.warn l (Log.msg (Log.val "No user found for key") . Log.field "key" (keyText key))
--       pure Nothing
--     Just (uid, claimTime) -> checkUser l brig key uid claimTime repairData

-- freeHandle :: Logger -> Handle -> Client ()
-- freeHandle l handle = do
--   Log.info l $ Log.msg (Log.val "Freeing handle") . Log.field "handle" (fromHandle handle)
--   retry x5 $ write handleDelete (params LocalQuorum (Identity handle))
--   where
--     handleDelete :: PrepQuery W (Identity Handle) ()
--     handleDelete = "DELETE FROM user_handle WHERE handle = ?"

checkUser :: Logger -> ClientState -> UserKey -> UserId -> Writetime UserId -> Bool -> IO (Maybe Inconsistency)
checkUser _l brig key userId time _repairData = do
  maybeDetails <- runClient brig $ getUserDetails userId
  case maybeDetails of
    Nothing -> do
      let status = Nothing
          userEmail = Nothing
          userPhone = Nothing
      -- when repairData $
      --   runClient brig $
      --     freeKey l key
      pure . Just $ Inconsistency {..}
    Just (mStatus, mStatusWriteTime, mEmail, mEmailWriteTime, mPhone, mPhoneWriteTime) -> do
      let status = WithWritetime <$> mStatus <*> mStatusWriteTime
          userEmail = WithWritetime <$> mEmail <*> mEmailWriteTime
          userPhone = WithWritetime <$> mPhone <*> mPhoneWriteTime
          statusError = case mStatus of
            Nothing -> True
            Just Deleted -> True
            _ -> False
          compareEmail e = (emailKeyUniq . mkEmailKey <$> mEmail) /= Just (fromEmail e)
          comparePhone p = (phoneKeyUniq . mkPhoneKey <$> mPhone) /= Just (fromPhone p)
          keyError = foldKey compareEmail comparePhone key
      if statusError || keyError
        then do
          -- when repairData $
          --   runClient brig $
          --     freeKey l key
          pure . Just $ Inconsistency {..}
        else pure Nothing
