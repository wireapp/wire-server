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

-- | > docs/reference/user/activation.md {#RefActivationAllowlist}
--
-- Email/phone whitelist.
module Wire.API.Allowlists
  ( AllowlistEmailDomains (..),
    AllowlistPhonePrefixes (..),
    verify,
  )
where

import Data.Aeson
import Data.Text qualified as Text
import Imports
import Wire.API.User.Identity

-- | A service providing a whitelist of allowed email addresses and phone numbers
data AllowlistEmailDomains = AllowlistEmailDomains [Text]
  deriving (Show, Generic)

instance FromJSON AllowlistEmailDomains

data AllowlistPhonePrefixes = AllowlistPhonePrefixes [Text]
  deriving (Show, Generic)

instance FromJSON AllowlistPhonePrefixes

-- | Consult the whitelist settings in brig's config file and verify that the provided
-- email/phone address is whitelisted.
verify :: Maybe AllowlistEmailDomains -> Maybe AllowlistPhonePrefixes -> Either Email Phone -> Bool
verify (Just (AllowlistEmailDomains allowed)) _ (Left email) = emailDomain email `elem` allowed
verify _ (Just (AllowlistPhonePrefixes allowed)) (Right phone) = any (`Text.isPrefixOf` fromPhone phone) allowed
verify Nothing _ (Left _) = True
verify _ Nothing (Right _) = True
