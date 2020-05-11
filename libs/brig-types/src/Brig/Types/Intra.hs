{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

-- | API data types used only for intra-environment communication.
-- TODO: Move to Brig.Types.User.Intra / Internal
module Brig.Types.Intra
  ( AccountStatus (..),
    AccountStatusUpdate (..),
    ConnectionStatus (..),
    UserAccount (..),
    UserSet (..),
    ReAuthUser (..),
  )
where

import Brig.Types.Connection
import Brig.Types.User
import Data.Aeson
import qualified Data.HashMap.Strict as M
import Data.Id (UserId)
import Data.Misc (PlainTextPassword (..))
import qualified Data.Text as Text
import Imports

-------------------------------------------------------------------------------
-- AccountStatus

data AccountStatus
  = Active
  | Suspended
  | Deleted
  | Ephemeral
  deriving (Eq, Show, Generic)

instance FromJSON AccountStatus where
  parseJSON = withText "account-status" $ \s -> case Text.toLower s of
    "active" -> pure Active
    "suspended" -> pure Suspended
    "deleted" -> pure Deleted
    "ephemeral" -> pure Ephemeral
    _ -> fail $ "Invalid account status: " ++ Text.unpack s

instance ToJSON AccountStatus where
  toJSON Active = String "active"
  toJSON Suspended = String "suspended"
  toJSON Deleted = String "deleted"
  toJSON Ephemeral = String "ephemeral"

newtype AccountStatusUpdate = AccountStatusUpdate
  {suStatus :: AccountStatus}
  deriving (Generic)

instance FromJSON AccountStatusUpdate where
  parseJSON = withObject "account-status-update" $ \o ->
    AccountStatusUpdate <$> o .: "status"

instance ToJSON AccountStatusUpdate where
  toJSON s = object ["status" .= suStatus s]

-------------------------------------------------------------------------------
-- ConnectionStatus

data ConnectionStatus = ConnectionStatus
  { csFrom :: !UserId,
    csTo :: !UserId,
    csStatus :: !Relation
  }
  deriving (Eq, Show, Generic)

instance FromJSON ConnectionStatus where
  parseJSON = withObject "connection-status" $ \o ->
    ConnectionStatus <$> o .: "from"
      <*> o .: "to"
      <*> o .: "status"

instance ToJSON ConnectionStatus where
  toJSON cs =
    object
      [ "from" .= csFrom cs,
        "to" .= csTo cs,
        "status" .= csStatus cs
      ]

-------------------------------------------------------------------------------
-- UserAccount

-- | A UserAccount is targeted to be used by our \"backoffice\" and represents
-- all the data related to a user in our system, regardless of whether they
-- are active or not, their status, etc.
data UserAccount = UserAccount
  { accountUser :: !User,
    accountStatus :: !AccountStatus
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserAccount where
  parseJSON j@(Object o) = do
    u <- parseJSON j
    s <- o .: "status"
    return $ UserAccount u s
  parseJSON _ = mzero

instance ToJSON UserAccount where
  toJSON (UserAccount u s) =
    let Object o = toJSON u
     in Object $ M.insert "status" (toJSON s) o

-------------------------------------------------------------------------------
-- UserList

-- | Set of user ids, can be used for different purposes (e.g., used on the internal
-- APIs for auto-connections, listing user's clients)
data UserSet = UserSet
  { usUsrs :: !(Set UserId)
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserSet where
  parseJSON = withObject "user-set" $ \o ->
    UserSet <$> o .: "users"

instance ToJSON UserSet where
  toJSON ac =
    object
      [ "users" .= usUsrs ac
      ]

-------------------------------------------------------------------------------
-- ReAuthUser

-- | Certain operations might require reauth of the user. These are available
-- only for users that have already set a password.
newtype ReAuthUser = ReAuthUser
  {reAuthPassword :: Maybe PlainTextPassword}
  deriving (Eq, Show, Generic)

instance FromJSON ReAuthUser where
  parseJSON = withObject "reauth-user" $ \o ->
    ReAuthUser <$> o .:? "password"

instance ToJSON ReAuthUser where
  toJSON ru =
    object
      [ "password" .= reAuthPassword ru
      ]
