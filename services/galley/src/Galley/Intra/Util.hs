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

import Bilge hiding (getHeader, options, statusCode)
import Bilge.RPC
import Bilge.Retry
import Control.Lens (view, (^.))
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Lazy qualified as LB
import Data.Misc (portNumber)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LT
import Galley.Env
import Galley.Monad
import Galley.Options
import Imports hiding (log)
import Network.HTTP.Types
import System.Logger
import System.Logger.Class qualified as LC
import Util.Options

data IntraComponent = Brig | Spar | Gundeck
  deriving (Show)

componentName :: IntraComponent -> String
componentName Brig = "brig"
componentName Spar = "spar"
componentName Gundeck = "gundeck"

componentRequest :: IntraComponent -> Opts -> Request -> Request
componentRequest Brig o =
  host (encodeUtf8 (o ^. optBrig . epHost))
    . port (portNumber (fromIntegral (o ^. optBrig . epPort)))
componentRequest Spar o =
  host (encodeUtf8 (o ^. optSpar . epHost))
    . port (portNumber (fromIntegral (o ^. optSpar . epPort)))
componentRequest Gundeck o =
  host (encodeUtf8 $ o ^. optGundeck . epHost)
    . port (portNumber $ fromIntegral (o ^. optGundeck . epPort))
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
