-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserSubsystem.UserSubsystemConfig where

import Data.Default
import Data.Domain
import Data.HashSet
import Data.LanguageCodes
import Imports
import Util.Timeout
import Wire.API.User
import Wire.Arbitrary

data UserSubsystemConfig = UserSubsystemConfig
  { emailVisibilityConfig :: EmailVisibilityConfig,
    defaultLocale :: Locale,
    searchSameTeamOnly :: Bool,
    maxTeamSize :: Word32,
    activationCodeTimeout :: Timeout,
    blockedDomains :: HashSet Domain
  }
  deriving (Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserSubsystemConfig)

-- | Some defaults to simplify test setups - Not meant for usage in production
instance Default UserSubsystemConfig where
  def =
    UserSubsystemConfig
      { emailVisibilityConfig = EmailVisibleIfOnSameTeam (),
        defaultLocale =
          Locale
            { lLanguage = Language EN,
              lCountry = Nothing
            },
        searchSameTeamOnly = False,
        maxTeamSize = 100,
        activationCodeTimeout = Timeout . fromIntegral @Word $ 24 * 3600,
        blockedDomains = empty
      }
