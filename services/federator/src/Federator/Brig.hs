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

module Federator.Brig where

-- Is there is a point in creating an effect for each service?
--
-- FUTUREWORK(federation): Once we authenticate the call, we should send authentication data
-- to brig so brig can do some authorization as required.

import qualified Bilge as RPC
import Bilge.RPC (rpc')
import Control.Lens (view)
import Federator.App (Federator, liftAppIOToFederator)
import Federator.Env (brig)
import Imports
import qualified Network.HTTP.Types as HTTP
import Polysemy

newtype BrigError = BrigErrorInvalidStatus HTTP.Status
  deriving (Eq, Show)

data Brig m a where
  -- | Returns status and body, 'HTTP.Response' is not nice to work with in tests
  BrigCall :: ByteString -> ByteString -> Brig m (Either BrigError (Maybe LByteString))

makeSem ''Brig

-- FUTUREWORK(federation): Do we want to use servant client here? May make
-- everything typed and safe
--
-- FUTUREWORK: Avoid letting the IO errors escape into `Embed Federator` and
-- return them as `Left`
interpretBrig ::
  Member (Embed Federator) r =>
  Sem (Brig ': r) a ->
  Sem r a
interpretBrig = interpret $ \case
  BrigCall path body -> embed @Federator . liftAppIOToFederator $ do
    brigReq <- view brig <$> ask
    res <-
      rpc' "brig" brigReq $
        RPC.method HTTP.POST
          . RPC.path path -- FUTUREWORK(federation): Protect against arbitrary paths
          . RPC.body (RPC.RequestBodyBS body)
    pure $
      -- TODO: Test this.
      if HTTP.statusCode (RPC.responseStatus res) == 200
        then Right $ RPC.responseBody res
        else Left $ BrigErrorInvalidStatus (RPC.responseStatus res)
