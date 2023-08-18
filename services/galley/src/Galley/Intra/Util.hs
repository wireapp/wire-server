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
    asyncCall,
  )
where

import Bilge (Request, Response, expect2xx, method, path)
import Bilge qualified as B
import Bilge.RPC (RPCException, rpc, rpcExceptionMsg)
import Bilge.Retry
import Control.Lens (view, (^.))
import Control.Monad.Catch
  ( Handler (Handler),
    SomeException,
    catches,
  )
import Control.Retry (RetryPolicy, limitRetries, recovering)
import Data.ByteString.Lazy qualified as LB
import Data.Misc (portNumber)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LT
import Galley.Env (options)
import Galley.Monad (App)
import Galley.Options (Opts, brig, gundeck, spar)
import Imports
  ( Maybe,
    Show (show),
    String,
    const,
    forkIO,
    fromIntegral,
    void,
    ($),
    (.),
  )
import Network.HTTP.Types (StdMethod (POST))
import System.Logger (msg, (.=), (~~))
import System.Logger.Class qualified as LC
import Util.Options (host, port)

data IntraComponent = Brig | Spar | Gundeck
  deriving (Show)

componentName :: IntraComponent -> String
componentName Brig = "brig"
componentName Spar = "spar"
componentName Gundeck = "gundeck"

componentRequest :: IntraComponent -> Opts -> Request -> Request
componentRequest Brig o =
  B.host (encodeUtf8 (o ^. brig . host))
    . B.port (portNumber (fromIntegral (o ^. brig . port)))
componentRequest Spar o =
  B.host (encodeUtf8 (o ^. spar . host))
    . B.port (portNumber (fromIntegral (o ^. spar . port)))
componentRequest Gundeck o =
  B.host (encodeUtf8 $ o ^. gundeck . host)
    . B.port (portNumber $ fromIntegral (o ^. gundeck . port))
    . method POST
    . path "/i/push/v2"
    . expect2xx

componentRetryPolicy :: IntraComponent -> RetryPolicy
componentRetryPolicy Brig = x1
componentRetryPolicy Spar = x1
componentRetryPolicy Gundeck = limitRetries 0

call ::
  IntraComponent ->
  (Request -> Request) ->
  App (Response (Maybe LB.ByteString))
call comp r = do
  o <- view options
  let r0 = componentRequest comp o
  let n = LT.pack (componentName comp)
  recovering (componentRetryPolicy comp) rpcHandlers (const (rpc n (r . r0)))

asyncCall :: IntraComponent -> (Request -> Request) -> App ()
asyncCall comp req = void $ do
  let n = LT.pack (componentName comp)
  forkIO $ catches (void (call comp req)) (handlers n)
  where
    handlers n =
      [ Handler $ \(x :: RPCException) -> LC.err (rpcExceptionMsg x),
        Handler $ \(x :: SomeException) -> LC.err $ "remote" .= n ~~ msg (show x)
      ]

x1 :: RetryPolicy
x1 = limitRetries 1
