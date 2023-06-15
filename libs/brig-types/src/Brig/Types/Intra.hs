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
  ( UserAccount (..),
    NewUserScimInvitation (..),
    UserSet (..),
  )
where

import Data.Aeson as A
import Data.Id (TeamId)
import qualified Data.Schema as Schema
import qualified Data.Swagger as S
import Imports
import Test.QuickCheck (Arbitrary)
import Wire.API.Team.Role
import Wire.API.User
import Wire.Arbitrary (GenericUniform (..))

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
  deriving (Arbitrary) via (GenericUniform UserAccount)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema.Schema UserAccount

instance Schema.ToSchema UserAccount where
  schema =
    Schema.object "UserAccount" $
      UserAccount
        <$> accountUser Schema..= userObjectSchema
        <*> accountStatus Schema..= Schema.field "status" Schema.schema

-------------------------------------------------------------------------------
-- NewUserScimInvitation

data NewUserScimInvitation = NewUserScimInvitation
  { newUserScimInvTeamId :: TeamId,
    newUserScimInvLocale :: Maybe Locale,
    newUserScimInvName :: Name,
    newUserScimInvEmail :: Email,
    newUserScimInvRole :: Role
  }
  deriving (Eq, Show, Generic)

instance FromJSON NewUserScimInvitation where
  parseJSON = withObject "NewUserScimInvitation" $ \o ->
    NewUserScimInvitation
      <$> o .: "team_id"
      <*> o .:? "locale"
      <*> o .: "name"
      <*> o .: "email"
      <*> o .: "role"

instance ToJSON NewUserScimInvitation where
  toJSON (NewUserScimInvitation tid loc name email role) =
    object
      [ "team_id" .= tid,
        "locale" .= loc,
        "name" .= name,
        "email" .= email,
        "role" .= role
      ]
