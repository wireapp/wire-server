-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Network.DNS.A where

import Data.IP
import Imports
import Network.DNS

-- | A response to lookupA, i.e., a reponse to a lookup of IPv4 addresses of a
-- domain.
data AResponse
  = AIPv4s [IPv4]
  | AResponseError DNSError
  deriving (Eq)

interpretResponse :: Either DNSError [IPv4] -> AResponse
interpretResponse = either AResponseError AIPv4s
