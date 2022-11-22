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
import qualified Data.Multihash.Digest as MH
import Brig.Data.UserKey
import Brig.Email (EmailKey (..), mkEmailKey)
import Brig.Phone (PhoneKey (..), mkPhoneKey)
import OpenSSL.EVP.Digest (digestBS, getDigestByName)
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
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)

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
        .| C.map ((<> "\n") . cs . Aeson.encode)
        .| sinkFile outputFile


pageSize :: Int32
pageSize = 1000

data Inconsistency = Inconsistency
  { -- | Key in the user_keys table
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

getKey :: UserKey -> Client (Maybe (UserId, Writetime UserId))
getKey key = retry x1 $ query1 cql (params LocalQuorum (Identity key))
  where
    cql :: PrepQuery R (Identity UserKey) (UserId, Writetime UserId)
    cql = "SELECT user, writetime(user) from user_keys where key = ?"

getKeys :: ConduitM () [(UserKey, UserId, Writetime UserId)] Client ()
getKeys = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (UserKey, UserId, Writetime UserId)
    cql = "SELECT key, user, writetime(user) from user_keys"

parseKey :: Text -> Maybe UserKey
parseKey t = (userEmailKey <$> parseEmail t) <|> (userPhoneKey <$> parsePhone t)

instance Cql UserKey where
  ctype = Tagged TextColumn

  fromCql (CqlText t) =
    maybe
      (Left $ "invalid userkey: " <> show t)
      Right (parseKey t)
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

checkKey :: Logger -> ClientState -> UserKey -> Bool -> IO (Maybe Inconsistency)
checkKey l brig key repairData = do
  mUser <- runClient brig $ getKey key
  case mUser of
    Nothing -> do
      Log.warn l (Log.msg (Log.val "No user found for key") . Log.field "key" (keyText key))
      pure Nothing
    Just (uid, writeTime) -> checkUser l brig key uid writeTime repairData

-- mostly copied from Brig to not need a Brig Env/ReaderT
freeUserKey :: Logger -> UserKey -> Client ()
freeUserKey l k = do
  Log.info l $ Log.msg (Log.val "Freeing key") . Log.field "key" (keyText k)
  hk <- liftIO $ hashKey k
  retry x5 $ write deleteHashed (params LocalQuorum (Identity hk))
  retry x5 $ write keyDelete (params LocalQuorum (Identity $ keyText k))
  where
    keyDelete :: PrepQuery W (Identity Text) ()
    keyDelete = "DELETE FROM user_keys WHERE key = ?"

    hashKey :: UserKey -> IO UserKeyHash
    hashKey uk = do
      Just d <- getDigestByName "SHA256"
      let d' = digestBS d $ T.encodeUtf8 (keyText uk)
      pure . UserKeyHash $
        MH.MultihashDigest MH.SHA256 (B.length d') d'

    deleteHashed :: PrepQuery W (Identity UserKeyHash) ()
    deleteHashed = "DELETE FROM user_keys_hash WHERE key = ?"

newtype UserKeyHash = UserKeyHash MH.MultihashDigest
instance Cql UserKeyHash where
  ctype = Tagged BlobColumn

  fromCql (CqlBlob lbs) = case MH.decode (toStrict lbs) of
    Left e -> Left ("userkeyhash: " ++ e)
    Right h -> pure $ UserKeyHash h
  fromCql _ = Left "userkeyhash: expected blob"

  toCql (UserKeyHash d) = CqlBlob $ MH.encode (MH.algorithm d) (MH.digest d)


-- repair these inconsistent data cases:
-- - 1. user deleted (or with status=null) in user table, data left in user_keys -> delete records in user_keys
-- - 2. user not found in user table, data left in user_keys -> delete records in user_keys
-- - 3. ... TODO
checkUser :: Logger -> ClientState -> UserKey -> UserId -> Writetime UserId -> Bool -> IO (Maybe Inconsistency)
checkUser l brig key userId time repairData = do
  maybeDetails <- runClient brig $ getUserDetails userId
  case maybeDetails of
    Nothing -> do
      let status = Nothing
          userEmail = Nothing
          userPhone = Nothing
      when repairData $ -- case 2.
        runClient brig $
          freeUserKey l key
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
      when (statusError && repairData) $ -- case 1.
        runClient brig $ freeUserKey l key
      if statusError || keyError
        then do
          -- when repairData $
          --   runClient brig $
          --     freeKey l key
          pure . Just $ Inconsistency {..}
        else pure Nothing
