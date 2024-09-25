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

module API.Util
  ( downloadAsset,
    withMockFederator,
  )
where

import Bilge hiding (body, host, port)
import CargoHold.Options
import CargoHold.Run
import Control.Lens hiding ((.=))
import Control.Monad.Codensity
import Data.ByteString.Conversion
import Data.Default
import Data.Id
import Data.Qualified
import qualified Data.UUID as UUID
import Federator.MockServer
import Imports hiding (head)
import qualified Network.HTTP.Media as HTTP
import Network.Wai.Utilities.MockServer
import TestSetup
import Util.Options
import Wire.API.Asset

-- Copied wholesale from gundeck/test/integration/API.hs
-- This is needed because it sets up the email on the user, verifiying it.
-- The changes to the asset routes forbidding non-verified users from uploading
-- assets breaks a lot of existing tests.
--

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . UUID.toASCIIBytes . toUUID

class IsAssetLocation key where
  locationPath :: key -> Request -> Request

instance IsAssetLocation AssetKey where
  locationPath k =
    apiVersion "v1"
      . paths ["assets", "v3", toByteString' k]

instance IsAssetLocation (Qualified AssetKey) where
  locationPath k =
    apiVersion "v2"
      . paths ["assets", toByteString' (qDomain k), toByteString' (qUnqualified k)]

instance IsAssetLocation ByteString where
  locationPath = path

class IsAssetToken tok where
  tokenParam :: tok -> Request -> Request

instance IsAssetToken () where
  tokenParam _ = id

instance IsAssetToken (Maybe AssetToken) where
  tokenParam = maybe id (header "Asset-Token" . toByteString')

instance IsAssetToken (Request -> Request) where
  tokenParam = id

downloadAssetWith ::
  (IsAssetLocation loc, IsAssetToken tok) =>
  (Request -> Request) ->
  UserId ->
  loc ->
  tok ->
  TestM (Response (Maybe LByteString))
downloadAssetWith r uid loc tok = do
  c <- viewUnversionedCargohold
  get $
    c
      . r
      . zUser uid
      . locationPath loc
      . tokenParam tok
      . noRedirect

downloadAsset ::
  (IsAssetLocation loc, IsAssetToken tok) =>
  UserId ->
  loc ->
  tok ->
  TestM (Response (Maybe LByteString))
downloadAsset = downloadAssetWith id

--------------------------------------------------------------------------------
-- Mocking utilities

withSettingsOverrides :: (Opts -> Opts) -> TestM a -> TestM a
withSettingsOverrides f action = do
  ts <- ask
  let opts = f (view tsOpts ts)
  liftIO . lowerCodensity $ do
    (app, _) <- mkApp opts
    p <- withMockServer app
    liftIO $ runTestM (ts & tsEndpoint %~ setLocalEndpoint p) action

setLocalEndpoint :: Word16 -> Endpoint -> Endpoint
setLocalEndpoint port endpoint = endpoint {port = port, host = "127.0.0.1"}

withMockFederator ::
  (FederatedRequest -> IO (HTTP.MediaType, LByteString)) ->
  TestM a ->
  TestM (a, [FederatedRequest])
withMockFederator respond action = do
  withTempMockFederator def {handler = respond} $ \p ->
    withSettingsOverrides
      (federator . _Just %~ setLocalEndpoint (fromIntegral p))
      action
