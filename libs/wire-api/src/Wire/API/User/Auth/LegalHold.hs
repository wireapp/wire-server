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

module Wire.API.User.Auth.LegalHold where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import Data.Id
import Data.Misc
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.User.Auth

-- | A special kind of login that is only used for an internal endpoint.
-- This kind of login returns restricted 'LegalHoldUserToken's instead of regular
-- tokens.
data LegalHoldLogin = LegalHoldLogin
  { lhlUserId :: !UserId,
    lhlPassword :: !(Maybe PlainTextPassword6),
    lhlLabel :: !(Maybe CookieLabel)
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema LegalHoldLogin

instance ToSchema LegalHoldLogin where
  schema =
    object "LegalHoldLogin" $
      LegalHoldLogin
        <$> lhlUserId .= field "user" schema
        <*> lhlPassword .= optField "password" (maybeWithDefault A.Null schema)
        <*> lhlLabel .= optField "label" (maybeWithDefault A.Null schema)
