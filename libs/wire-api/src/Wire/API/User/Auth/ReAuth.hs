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

module Wire.API.User.Auth.ReAuth
  ( -- * ReAuth
    ReAuthUser (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import Data.Code
import Data.Misc
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.User

-- | Certain operations might require reauth of the user. These are available
-- only for users that have already set a password.
data ReAuthUser = ReAuthUser
  { reAuthPassword :: Maybe PlainTextPassword6,
    reAuthCode :: Maybe Value,
    reAuthCodeAction :: Maybe VerificationAction
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ReAuthUser

instance ToSchema ReAuthUser where
  schema =
    object "ReAuthUser" $
      ReAuthUser
        <$> reAuthPassword .= optField "password" (maybeWithDefault A.Null schema)
        <*> reAuthCode .= optField "verification_code" (maybeWithDefault A.Null schema)
        <*> reAuthCodeAction .= optField "action" (maybeWithDefault A.Null schema)
