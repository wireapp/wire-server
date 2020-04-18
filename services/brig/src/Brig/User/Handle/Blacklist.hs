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

module Brig.User.Handle.Blacklist (isBlacklistedHandle) where

import Data.Handle (Handle (Handle))
import qualified Data.HashSet as HashSet
import Imports

-- | A blacklisted handle cannot be chosen by a (regular) user.
isBlacklistedHandle :: Handle -> Bool
isBlacklistedHandle = (`HashSet.member` blacklist)

blacklist :: HashSet Handle
blacklist =
  HashSet.fromList $
    map
      Handle
      [ "account",
        "admin",
        "administrator",
        "all",
        "android",
        "anna",
        "avs",
        "backend",
        "bot",
        "cs",
        "design",
        "dev",
        "developer",
        "development",
        "everyone",
        "help",
        "helpdesk",
        "hr",
        "info",
        "ios",
        "legal",
        "management",
        "news",
        "otto",
        "payment",
        "product",
        "purchase",
        "qa",
        "support",
        "team",
        "user",
        "web",
        "wire",
        "wirebot",
        "wireteam"
      ]
