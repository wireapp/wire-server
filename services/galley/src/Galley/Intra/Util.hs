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

module Galley.Intra.Util
  ( IntraComponent (..),
    call,
  )
where

import Bilge hiding (getHeader, host, options, port, statusCode)
import Bilge qualified as B
import Bilge.RPC (rpc)
import Bilge.Retry
import Cassandra.Options (Endpoint (..))
import Control.Lens (view)
import Control.Retry
import Data.ByteString.Lazy qualified as LB
import Data.Misc (portNumber)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LT
import Galley.Env hiding (brig)
import Galley.Monad
import Galley.Options
import Imports hiding (log)

data IntraComponent = Brig | Spar
  deriving (Show)

componentName :: IntraComponent -> String
componentName Brig = "brig"
componentName Spar = "spar"

componentRequest :: IntraComponent -> Opts -> Request -> Request
componentRequest Brig o =
  B.host (encodeUtf8 . host $ o._brig)
    . B.port (portNumber $ fromIntegral . port $ o._brig)
componentRequest Spar o =
  B.host (encodeUtf8 o._spar.host)
    . B.port (portNumber $ fromIntegral . port $ o._spar)

componentRetryPolicy :: IntraComponent -> RetryPolicy
componentRetryPolicy Brig = x1
componentRetryPolicy Spar = x1

call ::
  IntraComponent ->
  (Request -> Request) ->
  App (Response (Maybe LB.ByteString))
call comp r = do
  o <- view options
  let r0 = componentRequest comp o
  let n = LT.pack (componentName comp)
  recovering (componentRetryPolicy comp) rpcHandlers (const (rpc n (r . r0)))

x1 :: RetryPolicy
x1 = limitRetries 1
