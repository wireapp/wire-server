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
    callWithServant,
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
import Data.Sequence (Seq (..))
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LT
import Galley.Env hiding (brig)
import Galley.Monad
import Galley.Options
import Imports hiding (log)
import Network.HTTP.Types (statusIsServerError)
import Servant.Client qualified as Servant
import Servant.Client.Core qualified as Servant
import Wire.OpenTelemetry.Servant (otelClientMiddleware)

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

componentServantClient :: IntraComponent -> App Servant.ClientEnv
componentServantClient comp = do
  mgr <- view manager
  brigep <- case comp of
    Brig -> view $ options . brig
    Spar -> view $ options . spar
  RequestId rId <- view reqId
  let baseurl = Servant.BaseUrl Servant.Http (Text.unpack brigep.host) (fromIntegral brigep.port) ""
      addRequestIdHeader app req = do
        let reqWithId = req {Servant.requestHeaders = (requestIdName, rId) :<| req.requestHeaders}
        app reqWithId
      traceInfo = "intra-call-to-" <> Text.pack (componentName comp)

  pure $
    Servant.ClientEnv
      { Servant.manager = mgr,
        Servant.baseUrl = baseurl,
        Servant.cookieJar = Nothing,
        Servant.makeClientRequest = Servant.defaultMakeClientRequest,
        Servant.middleware =
          otelClientMiddleware traceInfo
            . addRequestIdHeader
      }

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

callWithServant :: IntraComponent -> Servant.ClientM a -> App (Either Servant.ClientError a)
callWithServant comp action = do
  env <- componentServantClient comp
  let makeRequest = liftIO $ Servant.runClientM action env
      shouldRetry (Right _) = False
      shouldRetry (Left (Servant.FailureResponse _ resp)) = statusIsServerError resp.responseStatusCode
      shouldRetry (Left (Servant.ConnectionError _)) = True
      shouldRetry (Left _) = False
  retrying (componentRetryPolicy comp) (const $ pure . shouldRetry) (const $ makeRequest)
