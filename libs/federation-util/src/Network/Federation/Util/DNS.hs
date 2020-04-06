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

module Network.Federation.Util.DNS
  ( srvLookup,
  )
where

import Imports
import Network.DNS
import Network.Federation.Util.Internal

-- | Looks up a SRV record given a domain, returning A(AAA) records with their
-- ports (ordered by priority and weight according to RFC 2782). Connection
-- attempts should be made to the returned result list in order.
--
-- Example:
--
-- > import Network.DNS.Resolver
-- > import Network.Federation.Util
-- >
-- > main :: IO ()
-- > main = do
-- >   rs <- makeResolvSeed defaultResolvConf
-- >   x <- srvLookup "staging.zinfra.io" rs
srvLookup :: Text -> ResolvSeed -> IO (Maybe [(Domain, Word16)])
srvLookup = srvLookup' srvDefaultPrefix

srvDefaultPrefix :: Text
srvDefaultPrefix = "_wire-server"
