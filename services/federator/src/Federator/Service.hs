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

module Federator.Service where

-- FUTUREWORK(federation): Once we authenticate the call, we should send authentication data
-- to brig so brig can do some authorization as required.

import qualified Bilge as RPC
import Bilge.RPC (rpc')
import Control.Lens (view)
import Data.Domain
import Data.String.Conversions (cs)
import qualified Data.Text.Lazy as LText
import Federator.App
import Federator.Env
import Imports
import qualified Network.HTTP.Types as HTTP
import Polysemy
import Polysemy.Input
import Wire.API.Federation.Component
import Wire.API.Federation.Domain (originDomainHeaderName)

newtype ServiceError = ServiceErrorInvalidStatus HTTP.Status
  deriving (Eq, Show)

type ServiceLBS = Service (Maybe LByteString)

data Service body m a where
  -- | Returns status and body, 'HTTP.Response' is not nice to work with in tests
  ServiceCall :: Component -> ByteString -> LByteString -> Domain -> Service body m (HTTP.Status, body)

makeSem ''Service

-- FUTUREWORK(federation): Do we want to use servant client here? May make
-- everything typed and safe
--
-- FUTUREWORK: Avoid letting the IO errors escape into `Embed IO` and
-- return them as `Left`
--
-- FUTUREWORK: unify this interpretation with similar ones in Galley
--
-- FUTUREWORK: does it make sense to use a lower level abstraction instead of bilge here?
interpretService ::
  Members '[Embed IO, Input Env] r =>
  Sem (ServiceLBS ': r) a ->
  Sem r a
interpretService = interpret $ \case
  ServiceCall component path body domain -> embedApp @IO $ do
    serviceReq <- view service <$> ask
    res <-
      rpc' (LText.pack (show component)) (serviceReq component) $
        RPC.method HTTP.POST
          . RPC.path path
          . RPC.body (RPC.RequestBodyLBS body)
          . RPC.contentJson
          . RPC.header originDomainHeaderName (cs (domainText domain))
    pure (RPC.responseStatus res, RPC.responseBody res)
