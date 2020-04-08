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

module Galley.Intra.Util
  ( brigReq,
    sparReq,
    call,
    x1,
  )
where

import Bilge hiding (getHeader, options, statusCode)
import Bilge.RPC
import Bilge.Retry
import Control.Lens (view)
import Control.Retry
import qualified Data.ByteString.Lazy as LB
import Data.Misc (portNumber)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import Galley.App
import Galley.Options
import Imports
import Util.Options

brigReq :: Galley (ByteString, Word16)
brigReq = do
  h <- encodeUtf8 <$> view (options . optBrig . epHost)
  p <- portNumber . fromIntegral <$> view (options . optBrig . epPort)
  return (h, p)

sparReq :: Galley (ByteString, Word16)
sparReq = do
  h <- encodeUtf8 <$> view (options . optSpar . epHost)
  p <- portNumber . fromIntegral <$> view (options . optSpar . epPort)
  return (h, p)

-- gundeckReq lives in Galley.Intra.Push

call :: LT.Text -> (Request -> Request) -> Galley (Response (Maybe LB.ByteString))
call n r = recovering x1 rpcHandlers (const (rpc n r))

x1 :: RetryPolicy
x1 = limitRetries 1
