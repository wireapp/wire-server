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

import Brig.Email (EmailKey (..), mkEmailKey)
import Cassandra
import Cassandra.Util
import Conduit
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Id
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Imports
import System.Logger
import System.Logger qualified as Log
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
        .| C.mapM (liftIO . pooledMapConcurrentlyN 48 (\(key, uid, claimTime) -> checkUser l brig key uid claimTime False))
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (BS.toStrict . Aeson.encode) . catMaybes)
        .| sinkFile inconsistenciesFile

runRepair :: Logger -> ClientState -> FilePath -> FilePath -> Bool -> IO ()
runRepair l brig inputFile outputFile repairData = do
  keys <- mapMaybe parseKey . Text.lines <$> Text.readFile inputFile
  runResourceT $
    runConduit $
      zipSources
        (C.sourceList [(1 :: Int32) ..])
        (C.sourceList keys)
        .| C.mapM
          ( \(i, key) -> do
              when (i `mod` 100 == 0) $
                Log.info l (Log.field "keysProcesses" i)
              liftIO $ checkKey l brig key repairData
          )
        .| C.map ((<> "\n") . BS.toStrict . Aeson.encode)
        .| sinkFile outputFile

pageSize :: Int32
pageSize = 1000

data Inconsistency = Inconsistency
  { -- | Key in the user_keys table
    key :: EmailKey,
    userId :: UserId,
    time :: Writetime UserId,
    status :: Maybe (WithWritetime AccountStatus),
    userEmail :: Maybe (WithWritetime Email),
    userPhone :: Maybe (WithWritetime Phone),
    inconsistencyCase :: Text
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

getKey :: EmailKey -> Client (Maybe (UserId, Writetime UserId))
getKey key = retry x5 $ query1 cql (params LocalQuorum (Identity key))
  where
    cql :: PrepQuery R (Identity EmailKey) (UserId, Writetime UserId)
    cql = "SELECT user, writetime(user) from user_keys where key = ?"

getKeys :: ConduitM () [(EmailKey, UserId, Writetime UserId)] Client ()
getKeys = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (EmailKey, UserId, Writetime UserId)
    cql = "SELECT key, user, writetime(user) from user_keys"

parseKey :: Text -> Maybe EmailKey
parseKey t = mkEmailKey <$> parseEmail t

instance Cql EmailKey where
  ctype = Tagged TextColumn

  fromCql (CqlText t) =
    maybe
      (Left $ "invalid userkey: " <> show t)
      Right
      (parseKey t)
  fromCql _ = Left "userkey: expected text"

  toCql k = toCql $ emailKeyUniq k

instance Aeson.ToJSON EmailKey where
  toJSON = Aeson.toJSON . emailKeyUniq

type UserDetailsRow = (Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe Email, Maybe (Writetime Email), Maybe Phone, Maybe (Writetime Phone))

getUserDetails :: UserId -> Client (Maybe UserDetailsRow)
getUserDetails uid = retry x5 $ query1 cql (params LocalQuorum (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) UserDetailsRow
    cql = "SELECT status, writetime(status), email, writetime(email), phone, writetime(phone) from user where id = ?"

checkKey :: Logger -> ClientState -> EmailKey -> Bool -> IO (Maybe Inconsistency)
checkKey l brig key repairData = do
  mUser <- runClient brig $ getKey key
  case mUser of
    Nothing -> do
      Log.warn l (Log.msg (Log.val "No user found for key") . Log.field "key" (emailKeyUniq key))
      pure Nothing
    Just (uid, writeTime) -> checkUser l brig key uid writeTime repairData

-- mostly copied from Brig to not need a Brig Env/ReaderT
freeEmailKey :: Logger -> EmailKey -> Client ()
freeEmailKey l k = do
  Log.info l $ Log.msg (Log.val "Freeing key") . Log.field "key" (emailKeyUniq k)
  retry x5 $ write keyDelete (params LocalQuorum (Identity $ emailKeyUniq k))
  where
    keyDelete :: PrepQuery W (Identity Text) ()
    keyDelete = "DELETE FROM user_keys WHERE key = ?"

insertKey :: Logger -> UserId -> EmailKey -> Client ()
insertKey l u k = do
  Log.info l $ Log.msg (Log.val "Inserting key") . Log.field "key" (emailKeyUniq k) . Log.field "userId" (show u)
  retry x5 $ write keyInsert (params LocalQuorum (emailKeyUniq k, u))
  where
    keyInsert :: PrepQuery W (Text, UserId) ()
    keyInsert = "INSERT INTO user_keys (key, user) VALUES (?, ?)"

-- repair these inconsistent data cases:
-- - 1. user deleted (or with status=null) in user table, data left in user_keys -> delete records in user_keys
-- - 2. user not found in user table, data left in user_keys -> delete records in user_keys
-- - 3. the user to which this key points to has a different email in the user table, and
--     3.a. this user *also* has a correct entry in the user_keys table -> delete record of the wrong pointer inside user_keys
--     3.b. this user's email, when searched for points to another user -> do nothing; log this issue
--     3.c  this user's email, when searched for does not exist in user_keys. Do nothing, let this be handled by the other module EmailLessUsers.hs
--   4. user has an email in user_keys but no email inside user table -> do nothing. How to resolve?
checkUser :: Logger -> ClientState -> EmailKey -> UserId -> Writetime UserId -> Bool -> IO (Maybe Inconsistency)
checkUser l brig key uid time repairData = do
  maybeDetails <- runClient brig $ getUserDetails uid
  case maybeDetails of
    Nothing -> do
      let status = Nothing
          userEmail = Nothing
          userPhone = Nothing
          inconsistencyCase = "2."
      when repairData $ -- case 2.
        runClient brig $
          freeEmailKey l key
      pure . Just $ Inconsistency {userId = uid, ..}
    Just (mStatus, mStatusWriteTime, mEmail, mEmailWriteTime, mPhone, mPhoneWriteTime) -> do
      let status = WithWritetime <$> mStatus <*> mStatusWriteTime
          userEmail = WithWritetime <$> mEmail <*> mEmailWriteTime
          userPhone = WithWritetime <$> mPhone <*> mPhoneWriteTime
          statusError = case mStatus of
            Nothing -> True
            Just Deleted -> True
            _ -> False
          compareEmail e = (emailKeyUniq . mkEmailKey <$> mEmail) /= Just (fromEmail e)
          keyError = compareEmail (emailKeyOrig key)
      if statusError
        then do
          let inconsistencyCase = "1."
          when repairData $ runClient brig (freeEmailKey l key)
          pure . Just $ Inconsistency {userId = uid, ..}
        else
          if keyError
            then do
              case mEmail of
                Nothing -> do
                  Log.warn l (Log.msg (Log.val "Subcase 4: user has no email") . Log.field "userId" (show uid))
                  let inconsistencyCase = "4."
                  pure . Just $ Inconsistency {userId = uid, ..}
                Just email -> do
                  validKeysEntry <- runClient brig $ getKey (mkEmailKey email)
                  case validKeysEntry of
                    Just (keyEntryUserId, _) ->
                      if keyEntryUserId == uid
                        then do
                          -- there is a valid matching user_key entry for a user in the user table; just *also* an extra entry that can be cleaned up (case 3.a.)
                          Log.warn l (Log.msg (Log.val "Subcase 3a: entry can be repaired by removing entry") . Log.field "key" (emailKeyUniq key))
                          let inconsistencyCase = "3.a."
                          when repairData $
                            runClient brig $
                              freeEmailKey l key
                          pure . Just $ Inconsistency {userId = uid, ..}
                        else do
                          let inconsistencyCase = "3.b."
                          Log.warn l (Log.msg (Log.val "Subcase 3b: double mismatch entry in user_keys") . Log.field "userId" (show uid))
                          pure . Just $ Inconsistency {userId = uid, ..}
                    Nothing -> do
                      let inconsistencyCase = "3.c."
                      Log.warn l (Log.msg (Log.val "Subcase 3c: missing entry in user_keys") . Log.field "userId" (show uid))
                      pure . Just $ Inconsistency {userId = uid, ..}
            else pure Nothing
