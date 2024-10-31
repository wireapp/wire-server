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

module CargoHold.Federation where

import CargoHold.App
import CargoHold.Options
import Control.Error
import Control.Exception (throw)
import Control.Monad.Codensity
import Data.Id
import Data.Qualified
import Imports hiding (head)
import Servant.API
import Servant.Types.SourceT
import Wire.API.Asset
import Wire.API.Federation.API
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.Client
import Wire.API.Federation.Error

-- [Note]
-- There are several ways a remote asset could be streamed to the client:
--
-- "all-the-way": the remote federator streams the asset back to us in the RPC
--    itself, and we forward it to the client
-- "two-step": the remote federator returns a redirect response, we follow it
--    and stream the data to the client
-- "one-step": the remote federator returns a redirect response, and we simply
--    forward it to the client
--
-- For now, only the "all-the-way" solution is implemented. Note that the asset
-- is streamed back through our outward federator, as well as the remote one.

downloadRemoteAsset ::
  () =>
  Local UserId ->
  Remote AssetKey ->
  Maybe AssetToken ->
  Handler (Maybe (SourceIO ByteString))
downloadRemoteAsset usr rkey tok = do
  let ga =
        GetAsset
          { key = tUnqualified rkey,
            user = tUnqualified usr,
            token = tok
          }
  exists <-
    fmap available . executeFederated rkey $
      fedClient @'Cargohold @"get-asset" ga
  if exists
    then
      Just
        <$> executeFederatedStreaming
          rkey
          ( toSourceIO
              <$> fedClient @'Cargohold @"stream-asset" ga
          )
    else pure Nothing

mkFederatorClientEnv :: Remote x -> Handler FederatorClientEnv
mkFederatorClientEnv remote = do
  loc <- asks (.localUnit)
  endpoint <-
    asks (.options.federator)
      >>= maybe (throwE federationNotConfigured) pure
  mgr <- asks (.http2Manager)
  rid <- asks (.requestId)
  pure
    FederatorClientEnv
      { ceOriginDomain = tDomain loc,
        ceTargetDomain = tDomain remote,
        ceFederator = endpoint,
        ceHttp2Manager = mgr,
        ceOriginRequestId = rid
      }

executeFederated :: Remote x -> FederatorClient 'Cargohold a -> Handler a
executeFederated remote c = do
  env <- mkFederatorClientEnv remote
  liftIO (runFederatorClient @'Cargohold env c)
    >>= either (throwE . federationErrorToWai . FederationCallFailure) pure

executeFederatedStreaming ::
  Remote x ->
  FederatorClient 'Cargohold (SourceIO ByteString) ->
  Handler (SourceIO ByteString)
executeFederatedStreaming remote c = do
  env <- mkFederatorClientEnv remote
  -- To clean up resources correctly, we exploit the Codensity wrapper around
  -- StepT to embed the result of @runFederatorClientToCodensity@. This works, but
  -- using this within a Servant handler has the effect of delaying exceptions to
  -- the point where response streaming has already started (i.e. we have already
  -- committed to a successful response).
  -- Fortunately, Warp does not actually send out the response headers until it
  -- sees at least one chunk, so by throwing the exception in IO and having a
  -- catch middleware in place, we make sure that the correct error response
  -- ends up being generated.
  pure $
    SourceT $ \k ->
      runCodensity
        (runFederatorClientToCodensity @'Cargohold env c)
        (either (throw . federationErrorToWai . FederationCallFailure) (flip unSourceT k))
