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

module API.Federator where

import Data.Function
import GHC.Stack
import qualified Network.HTTP.Client as HTTP
import Testlib.Prelude

getMetrics ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  (ServiceMap -> HostPort) ->
  App Response
getMetrics domain service = do
  req <- rawBaseRequestF domain service "i/metrics"
  submit "GET" req

rawBaseRequestF :: (HasCallStack, MakesValue domain) => domain -> (ServiceMap -> HostPort) -> String -> App HTTP.Request
rawBaseRequestF domain getService path = do
  domainV <- objDomain domain
  serviceMap <- getServiceMap domainV

  liftIO . HTTP.parseRequest $
    let HostPort h p = getService serviceMap
     in "http://" <> h <> ":" <> show p <> ("/" <> joinHttpPath (splitHttpPath path))
