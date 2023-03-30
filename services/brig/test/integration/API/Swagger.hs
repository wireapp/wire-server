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

module API.Swagger (tests) where

import Bilge
import Brig.Options
import Control.Lens
import Data.Aeson.Lens
import Data.String.Conversions
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util

tests :: Manager -> Opts -> Brig -> TestTree
tests p _opts brigNoImplicitVersion =
  testGroup "version" $
    [ test p "GET /api/swagger.json" $ testSwaggerJson brigNoImplicitVersion "",
      test p "GET /api/swagger-ui" $ testSwaggerUI brigNoImplicitVersion "",
      test p "GET /v2/api/swagger.json" $ testSwaggerJson brigNoImplicitVersion "/v2",
      test p "GET /v2/api/swagger-ui" $ testSwaggerUI brigNoImplicitVersion "/v2"
    ]

testSwaggerJson :: Brig -> ByteString -> Http ()
testSwaggerJson brig version = do
  r <- get (brig . path (version <> "/api/swagger.json") . expect2xx)
  liftIO $ assertBool "json body" (isJust $ ((^? _Object) <=< responseBody) r)

testSwaggerUI :: Brig -> ByteString -> Http ()
testSwaggerUI brig version = do
  r <- get (brig . path (version <> "/api/swagger-ui") . expect2xx)
  liftIO $ assertBool "HTML body" ("<div id=\"swagger-ui\"></div>" `isInfixOf` (cs . fromJust . responseBody $ r))
