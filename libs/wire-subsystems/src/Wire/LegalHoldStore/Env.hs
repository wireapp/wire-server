-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.LegalHoldStore.Env where

import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Misc
import Imports
import Network.HTTP.Client qualified as Http

data LegalHoldEnv = LegalHoldEnv
  { makeVerifiedRequest :: Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> IO (Http.Response LC8.ByteString),
    makeVerifiedRequestFreshManager :: Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> IO (Http.Response LC8.ByteString)
  }
