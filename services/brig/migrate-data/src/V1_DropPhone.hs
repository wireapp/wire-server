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

module V1_DropPhone (migration) where

import Brig.DataMigration.Types
import Cassandra
import Control.Applicative (optional)
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.Conduit
import Data.Conduit.Combinators qualified as C
import Data.Id
import Data.Text qualified as Text
import Imports
import System.Logger.Class (MonadLogger)
import System.Logger.Class qualified as Log
import Wire.API.User.Identity (Email)

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 1,
      text = "Clear the excluded_phones table",
      action = do
        dropExcludedPhonesTable
        runConduit
          ( getUsers
              .| C.concat
              .| C.mapM_ handlePhoneUser
          )
    }

pageSize :: Int32
pageSize = 1000

handlePhoneUser :: (MonadClient m, MonadLogger m) => User -> m ()
handlePhoneUser = \case
  User uid (Just phone) (Just _email) _hasSsoId ->
    removePhone phone uid
  User uid (Just _phone) Nothing True ->
    Log.warn $
      Log.field "uid" (Log.val (toByteString' uid))
        . Log.msg (Log.val "User has phone but no email and has SSO ID")
  User uid (Just phone) Nothing False ->
    -- delete user: call `DELETE i/user/<uid>`
    -- then remove the phone from the user and user key table
    removePhone phone uid
  User _uid Nothing _email _hasSsoId -> pure ()

removePhone :: (MonadClient m) => Phone -> UserId -> m ()
removePhone phone uid = do
  dropPhoneFromUser uid
  deleteKey $ mkPhoneUniqueKey phone
  pure ()
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

    deleteKey :: (MonadClient m) => Text -> m ()
    deleteKey key = do
      retry x5 $ write keyDelete (params LocalQuorum (Identity $ key))

    keyDelete :: PrepQuery W (Identity Text) ()
    keyDelete = "DELETE FROM user_keys WHERE key = ?"

mkPhoneUniqueKey :: Phone -> Text
mkPhoneUniqueKey = Text.filter (not . isSpace) . fromPhone

-- | Drop the data from the 'excluded_phones' Brig table
dropExcludedPhonesTable :: (MonadClient m) => m ()
dropExcludedPhonesTable = retry x1 . write cql $ paramsP LocalQuorum () pageSize
  where
    cql :: PrepQuery W () ()
    cql = "TRUNCATE TABLE excluded_phones"

-- | Get users
getUsers :: (MonadClient m) => ConduitM () [User] m ()
getUsers =
  paginateC cql (paramsP LocalQuorum () pageSize) x5
    .| C.map (fmap (\(uid, phone, email, ssoId) -> User uid (phone >>= parsePhone . Text.filter (not . isSpace)) email (isJust ssoId)))
  where
    cql :: PrepQuery R () (UserId, Maybe Text, Maybe Email, Maybe Text)
    cql = "SELECT id, phone, email, sso_id FROM user"

data User = User
  { id :: UserId,
    phone :: Maybe Phone,
    email :: Maybe Email,
    hasSsoId :: Bool
  }
  deriving (Generic)

newtype Phone = Phone {fromPhone :: Text}
  deriving stock (Eq, Ord, Show, Generic)

-- | Parses a phone number in E.164 format with a mandatory leading '+'.
parsePhone :: Text -> Maybe Phone
parsePhone p
  | isValidPhone p = Just $! Phone p
  | otherwise = Nothing

-- | Checks whether a phone number is valid, i.e. it is in E.164 format
-- with a mandatory leading '+' followed by 10-15 digits.
isValidPhone :: Text -> Bool
isValidPhone = either (const False) (const True) . parseOnly e164
  where
    e164 = char '+' *> count 8 digit *> count 7 (optional digit) *> endOfInput
