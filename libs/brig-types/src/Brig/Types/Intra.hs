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

-- | API data types used only for intra-environment communication.
-- TODO: Move to Brig.Types.User.Intra / Internal
module Brig.Types.Intra
  ( NewUserScimInvitation (..),
    UserSet (..),
    ReAuthUser (..),
  )
where

import Data.Aeson as A
import Data.Code as Code
import Data.Id (TeamId)
import Data.Misc (PlainTextPassword (..))
import Imports
import Wire.API.User

-------------------------------------------------------------------------------
-- NewUserScimInvitation

data NewUserScimInvitation = NewUserScimInvitation
  { newUserScimInvTeamId :: TeamId,
    newUserScimInvLocale :: Maybe Locale,
    newUserScimInvName :: Name,
    newUserScimInvEmail :: Email
  }
  deriving (Eq, Show, Generic)

instance FromJSON NewUserScimInvitation where
  parseJSON = withObject "NewUserScimInvitation" $ \o ->
    NewUserScimInvitation
      <$> o .: "team_id"
      <*> o .:? "locale"
      <*> o .: "name"
      <*> o .: "email"

instance ToJSON NewUserScimInvitation where
  toJSON (NewUserScimInvitation tid loc name email) =
    object
      [ "team_id" .= tid,
        "locale" .= loc,
        "name" .= name,
        "email" .= email
      ]

-------------------------------------------------------------------------------
-- ReAuthUser

-- | Certain operations might require reauth of the user. These are available
-- only for users that have already set a password.
data ReAuthUser = ReAuthUser
  { reAuthPassword :: Maybe PlainTextPassword,
    reAuthCode :: Maybe Code.Value,
    reAuthCodeAction :: Maybe VerificationAction
  }
  deriving (Eq, Show, Generic)

instance FromJSON ReAuthUser where
  parseJSON = withObject "reauth-user" $ \o ->
    ReAuthUser <$> o .:? "password" <*> o .:? "verification_code" <*> o .:? "action"

instance ToJSON ReAuthUser where
  toJSON ru =
    object
      [ "password" .= reAuthPassword ru,
        "verification_code" .= reAuthCode ru,
        "action" .= reAuthCodeAction ru
      ]
