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

module API.Version (tests) where

import Bilge
import Bilge.Assert
import Data.Domain
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.Routes.Version

tests :: Manager -> Brig -> TestTree
tests p brig =
  testGroup
    "version"
    [ test p "GET /api-version" $ testVersion brig,
      test p "GET /v1/api-version" $ testVersionV1 brig,
      test p "GET /v500/api-version" $ testUnsupportedVersion brig,
      test p "GET /api-version (federation info)" $ testFederationDomain brig
    ]

testVersion :: Brig -> Http ()
testVersion brig = do
  vinfo <-
    responseJsonError =<< get (brig . path "/api-version")
      <!! const 200 === statusCode
  liftIO $
    vinfoSupported vinfo @?= [V0, V1]

testVersionV1 :: Brig -> Http ()
testVersionV1 brig = do
  vinfo <-
    responseJsonError =<< get (brig . path "/v1/api-version")
      <!! const 200 === statusCode
  liftIO $
    vinfoSupported vinfo @?= [V0, V1]

testUnsupportedVersion :: Brig -> Http ()
testUnsupportedVersion brig = do
  e <-
    responseJsonError =<< get (brig . path "/v500/api-version")
      <!! const 404 === statusCode
  liftIO $ Wai.label e @?= "unsupported-version"

testFederationDomain :: Brig -> Http ()
testFederationDomain brig = do
  vinfo <-
    responseJsonError =<< get (brig . path "/api-version")
      <!! const 200 === statusCode
  liftIO $ do
    vinfoFederation vinfo @?= True
    vinfoDomain vinfo @?= Domain "example.com"
