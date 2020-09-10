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

import API.Util (withSettingsOverrides)
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Lens (view, (?~))
import Data.ByteString.Conversion (toByteString')
import Data.Coerce (coerce)
import Data.Domain (Domain, mkDomain)
import Data.Id (Id)
import qualified Data.Id as Id
import Data.Qualified (Qualified (Qualified))
import Galley.Options (optSettings, setEnableFederationWithDomain)
import Galley.Types.IdMapping (PostIdMappingRequest (PostIdMappingRequest), PostIdMappingResponse (PostIdMappingResponse))
import Imports
import qualified Network.Wai.Test as WaiTest
import Test.Tasty (TestTree, testGroup)
import TestHelpers (test)
import TestSetup (TestM, TestSetup, tsGConf, tsGalley)

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "IdMapping"
    [ test s "endpoints are disabled by default" endpointsAreDisabledByDefault,
      test s "endpoints can be enabled and work" endpointsCanBeEnabled
    ]

endpointsAreDisabledByDefault :: TestM ()
endpointsAreDisabledByDefault = do
  g <- view tsGalley
  opaqueId <- Id.randomId
  remoteId <- Id.randomId
  let Right domain = mkDomain "integration.com"
      qualifiedId = Qualified remoteId domain
  getIdMapping g opaqueId
    !!! const 403 === statusCode
  createIdMapping g qualifiedId
    !!! const 403 === statusCode

endpointsCanBeEnabled :: TestM ()
endpointsCanBeEnabled = do
  let Right domain = mkDomain "integration.com"
  g <- view tsGalley

  -- a random mapping doesn't exist
  withFederationEnabled domain $ do
    randomOpaqueId <- Id.randomId
    getIdMapping g randomOpaqueId
      !!! const 404 === statusCode

  -- but we can add a mapping
  qualifiedId <- flip Qualified domain <$> Id.randomId
  PostIdMappingResponse createdMappedId <-
    withFederationEnabled domain $ do
      res <-
        createIdMapping g qualifiedId
          <!! const 200 === statusCode
      pure (responseJsonUnsafeWithMsg "IdMapping" res)

  -- and this then exists
  withFederationEnabled domain $ do
    getIdMapping g createdMappedId
      !!! const 200 === statusCode

withFederationEnabled :: Domain -> WaiTest.Session a -> TestM a
withFederationEnabled domain action = do
  opts <- view tsGConf
  let newOpts = opts & optSettings . setEnableFederationWithDomain ?~ domain
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

createIdMapping :: (MonadIO m, MonadHttp m) => (Request -> Request) -> Qualified (Id a) -> m (Response (Maybe LByteString))
createIdMapping g qualifiedId =
  post
    ( g
        . paths ["i", "id-mapping"]
        . json (PostIdMappingRequest (coerce qualifiedId))
    )

getIdMapping :: (MonadIO m, MonadHttp m) => (Request -> Request) -> Id a -> m (Response (Maybe LByteString))
getIdMapping g opaqueId =
  get
    ( g
        . paths ["i", "id-mapping", toByteString' opaqueId]
    )
