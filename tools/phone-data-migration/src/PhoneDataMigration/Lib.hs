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
import qualified System.Logger.Class as Log
import Data.Id
import Wire.API.User (Email)
import System.Logger.Class (MonadLogger)
import Data.ByteString.Conversion (toByteString')

-- | Drop the data from the 'excluded_phones' Brig table
dropExcludedPhonesTable :: (MonadClient m) => m ()
dropExcludedPhonesTable = retry x1 . write cql $ paramsP LocalQuorum () pageSize
  where
    cql :: PrepQuery W () ()
    cql = "TRUNCATE TABLE excluded_phones"

pageSize :: Int32
pageSize = 1000

getUsers :: (MonadClient m) => ConduitM () [User] m ()
getUsers =
  paginateC cql (paramsP LocalQuorum () pageSize) x5
    .| Conduit.map (fmap (\(uid, phone, email, ssoId) -> User uid (phone >>= parsePhone) email (isJust ssoId)))
  where
    cql :: PrepQuery R () (UserId, Maybe Text, Maybe Email, Maybe Text)
    cql = "SELECT id, phone, email, sso_id FROM user"

process :: AppT IO ()
process = do
  dropExcludedPhonesTable
  runConduit
    $ getUsers
    .| Conduit.concat
    .| Conduit.mapM_ handlePhoneUser

handlePhoneUser :: (MonadClient m, MonadLogger m) => User -> m ()
handlePhoneUser = \case
  User uid (Just phone) (Just _email) _hasSsoId ->
    removePhone phone uid
  User uid (Just _phone) Nothing True -> do
    Log.warn $
      Log.field "uid" (Log.val (toByteString' uid))
        . Log.msg (Log.val "User has phone but no email and has SSO ID")
  User uid (Just phone) Nothing False ->
    -- delete user: call `DELETE i/user/<uid>`
    -- then remove the phone from the user and user key table
    removePhone phone uid
    -- throttle
  User _uid Nothing _email _hasSsoId -> pure ()

removePhone :: (MonadClient m) => Phone -> UserId -> m ()
removePhone phone uid = do
  dropPhoneFromUser uid
  deleteKey phone
  where
    -- Drop the phone column in the user table. The rows for which this is done
    -- are selected by a filter on rows that have data in the column.
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

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  env <- mkEnv opts
  runReaderT (unAppT process) env
  where
    desc = header "phone-data-migration" <> progDesc "Remove phone data from wire-server" <> fullDesc
