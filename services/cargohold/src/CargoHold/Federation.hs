-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
import Control.Lens
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
  Local UserId ->
  Remote AssetKey ->
  Maybe AssetToken ->
  Handler (Maybe (SourceIO ByteString))
downloadRemoteAsset usr rkey tok = do
  let ga =
        GetAsset
          { gaKey = tUnqualified rkey,
            gaUser = tUnqualified usr,
            gaToken = tok
          }
  exists <-
    fmap gaAvailable . executeFederated rkey $
      getAsset clientRoutes ga
  if exists
    then Just <$> executeFederatedStreaming rkey (toSourceIO <$> streamAsset clientRoutes ga)
    else pure Nothing

mkFederatorClientEnv :: Remote x -> Handler FederatorClientEnv
mkFederatorClientEnv remote = do
  loc <- view localUnit
  endpoint <-
    view (options . optFederator)
      >>= maybe (throwE federationNotConfigured) pure
  pure
    FederatorClientEnv
      { ceOriginDomain = tDomain loc,
        ceTargetDomain = tDomain remote,
        ceFederator = endpoint
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
  pure $
    SourceT $ \k ->
      runCodensity
        (runFederatorClientToCodensity @'Cargohold env c)
        (either throw (flip unSourceT k))
