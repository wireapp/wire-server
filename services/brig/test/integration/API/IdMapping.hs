{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module API.IdMapping where

import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Options (enableFederationWithDomain, optionSettings)
import qualified Brig.Options as Opt
import Control.Lens ((?~))
import Data.ByteString.Conversion (toByteString')
import Data.Coerce (coerce)
import Data.Domain (Domain, mkDomain)
import Data.Id (Id)
import qualified Data.Id as Id
import Data.Qualified (Qualified (Qualified))
import Galley.Types.IdMapping (PostIdMappingRequest (PostIdMappingRequest), PostIdMappingResponse (PostIdMappingResponse))
import Imports
import qualified Network.Wai.Test as WaiTest
import Test.Tasty (TestTree, testGroup)
import Util (Brig, test, withSettingsOverrides)

tests :: Opt.Opts -> Manager -> Brig -> TestTree
tests opts mgr brig =
  testGroup
    "IdMapping"
    [ test mgr "endpoints are disabled by default" $ endpointsAreDisabledByDefault brig,
      test mgr "endpoints can be enabled and work" $ endpointsCanBeEnabled opts brig
    ]

endpointsAreDisabledByDefault :: Brig -> Http ()
endpointsAreDisabledByDefault brig = do
  opaqueId <- Id.randomId
  remoteId <- Id.randomId
  let Right domain = mkDomain "integration.com"
      qualifiedId = Qualified remoteId domain
  getIdMapping brig opaqueId
    !!! const 403 === statusCode
  createIdMapping brig qualifiedId
    !!! const 403 === statusCode

endpointsCanBeEnabled :: Opt.Opts -> Brig -> Http ()
endpointsCanBeEnabled opts brig = do
  let Right domain = mkDomain "integration.com"

  -- a random mapping doesn't exist
  withFederationEnabled opts domain $ do
    randomOpaqueId <- Id.randomId
    getIdMapping brig randomOpaqueId
      !!! const 404 === statusCode

  -- but we can add a mapping
  qualifiedId <- flip Qualified domain <$> Id.randomId
  PostIdMappingResponse createdMappedId <-
    withFederationEnabled opts domain $ do
      res <-
        createIdMapping brig qualifiedId
          <!! const 200 === statusCode
      pure (responseJsonUnsafeWithMsg "IdMapping" res)

  -- and this then exists
  withFederationEnabled opts domain $ do
    getIdMapping brig createdMappedId
      !!! const 200 === statusCode

withFederationEnabled :: Opt.Opts -> Domain -> WaiTest.Session a -> Http a
withFederationEnabled opts domain action = do
  let newOpts = opts & optionSettings . enableFederationWithDomain ?~ domain
  withSettingsOverrides newOpts action

-- NOTE:
-- Once we start creating mappings in actual business logic, we want to test that
-- this creates them in both services.
-- However, that won't be easy, as federation is disabled by default and our way
-- to override config only works for one service at a time.
-- We will probably need a separate federation integration test binary that we run
-- with federation enabled.

--------------------------------------------------------------------------------
-- API calls

createIdMapping :: (MonadIO m, MonadHttp m) => Brig -> Qualified (Id a) -> m (Response (Maybe LByteString))
createIdMapping brig qualifiedId =
  post
    ( brig
        . paths ["i", "id-mapping"]
        . json (PostIdMappingRequest (coerce qualifiedId))
    )

getIdMapping :: (MonadIO m, MonadHttp m) => Brig -> Id a -> m (Response (Maybe LByteString))
getIdMapping brig opaqueId =
  get
    ( brig
        . paths ["i", "id-mapping", toByteString' opaqueId]
    )
