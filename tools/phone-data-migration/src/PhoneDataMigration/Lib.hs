{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module PhoneDataMigration.Lib where

import Cassandra as C
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import Imports
import Options.Applicative
import PhoneDataMigration.Types
import System.Logger.Message ((.=), (~~))
import qualified System.Logger.Class as Log
import Data.Id
import Wire.API.User (Email)
import System.Logger.Class (MonadLogger)

pageSize :: Int32
pageSize = 1000

dropExcludedPhonesTable :: (MonadClient m) => m ()
dropExcludedPhonesTable = retry x1 . write cql $ paramsP LocalQuorum () pageSize
  where
    cql :: PrepQuery W () ()
    cql = "TRUNCATE TABLE excluded_phones"

getUsers :: (MonadClient m) => ConduitM () [User] m ()
getUsers =
  paginateC cql (paramsP LocalQuorum () pageSize) x5
    .| Conduit.map (fmap (\(uid, phone, email, ssoId) -> User uid (phone >>= parsePhone) email (isJust ssoId)))
  where
    cql :: PrepQuery R () (UserId, Maybe Text, Maybe Email, Maybe Text)
    cql = "SELECT id, phone, email, sso_id FROM user"

removePhoneData :: (MonadClient m) => Phone -> UserId -> m ()
removePhoneData phone uid = do
  dropPhoneFromUser uid
  deleteKey phone
  where
    dropPhoneFromUser :: (MonadClient m) => UserId -> m ()
    dropPhoneFromUser =
      retry x5
        . write setPhoneToNull
        . params LocalQuorum
        . Identity

    setPhoneToNull :: PrepQuery W (Identity UserId) ()
    setPhoneToNull = "UPDATE user SET phone = null WHERE id = ?"

    deleteKey :: (MonadClient m) => Phone -> m ()
    deleteKey p = do
      retry x5 $ write keyDelete (params LocalQuorum (Identity $ fromPhone p))

    keyDelete :: PrepQuery W (Identity Text) ()
    keyDelete = "DELETE FROM user_keys WHERE key = ?"

handlePhoneUser :: (MonadClient m, MonadLogger m) => User -> m Result
handlePhoneUser = \case
  User _uid Nothing Nothing False ->
    -- this case should not happen
    pure $ mempty { total = 1, noIdentity = 1}
  User _uid Nothing (Just _email) False ->
    pure $ mempty { total = 1, emailIdentity = 1}
  User uid (Just phone) Nothing False -> do
    removePhoneData phone uid
    pure $ mempty { total = 1, phoneIdentity = 1}
  User uid (Just phone) (Just _email) False -> do
    removePhoneData phone uid
    pure $ mempty { total = 1, fullIdentity = 1}
  User _uid Nothing Nothing True -> do
    pure $ mempty { total = 1, ssoIdentity = 1}
  User _uid Nothing (Just _email) True -> do
    pure $ mempty { total = 1, ssoIdentityEmail = 1}
  User uid (Just _phone) Nothing True -> do
    Log.warn $
      "uid" .= show uid
        ~~ Log.msg (Log.val "user with sso id has a phone but no email. phone number was not removed. please check manually")
    pure $ mempty { total = 1, ssoIdentityPhone = 1}
  User uid (Just phone) (Just _email) True -> do
    removePhoneData phone uid
    pure $ mempty { total = 1, ssoIdentityFull = 1}

removePhoneDataStream :: ConduitT () o (AppT IO) Result
removePhoneDataStream  = do
  getUsers
    .| Conduit.concat
    .| Conduit.mapM handlePhoneUser
    .| Conduit.scanl (<>) mempty
    .| Conduit.mapM_ (logEvery 100000)
    .| Conduit.lastDef mempty
  where
    logEvery :: Int -> Result -> AppT IO ()
    logEvery i r =
      when (r.total `mod` i == 0) $
        Log.info $ "intermediate_result" .= show r

run :: AppT IO ()
run = do
  dropExcludedPhonesTable
  result <- runConduit removePhoneDataStream
  Log.info $ "result" .= show result

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  env <- mkEnv opts
  runReaderT (unAppT run) env
  where
    desc = header "phone-data-migration" <> progDesc "Remove phone data from wire-server" <> fullDesc
