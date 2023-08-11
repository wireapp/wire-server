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
import Data.ByteString.Conversion (toByteString')
import Imports
import Servant.API (toQueryParam)
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.Routes.Version (Version)

tests :: Manager -> Opts -> Brig -> TestTree
tests p _opts brigNoImplicitVersion =
  testGroup "swagger" $
    [ testGroup "public" $
        join
          [ [ test p ("GET /" <> show v <> "/api/swagger.json") $ testSwaggerJson brigNoImplicitVersion (toByteString' v),
              test p ("GET /" <> show v <> "/api/swagger-ui") $ testSwaggerUI brigNoImplicitVersion (toByteString' v)
            ]
            | v <- [minBound :: Version ..]
          ],
      testGroup "internal" $
        [ test p "GET /v2/api-internal/swagger-ui/brig" $ void (get (brigNoImplicitVersion . path "/v2/api-internal/swagger-ui/brig" . expect4xx)),
          test p "GET /v2/api-internal/swagger-ui/gundeck" $ void (get (brigNoImplicitVersion . path "/v2/api-internal/swagger-ui/gundeck" . expect4xx)),
          test p "GET /v2/i/status" $ void (get (brigNoImplicitVersion . path "/v2/i/status" . expect4xx))
        ],
      testGroup "toc" $
        [ test p "toc" $ do
            forM_ ["/api/swagger-ui", "/api/swagger-ui/index.html", "/api/swagger.json"] $ \pth -> do
              r <- get (brigNoImplicitVersion . path pth . expect2xx)
              liftIO $ assertEqual "toc is intact" (responseBody r) (Just "<html><head></head><body><h2>please pick an api version</h2><a href=\"/v0/api/swagger-ui/\">/v0/api/swagger-ui/</a><br><a href=\"/v1/api/swagger-ui/\">/v1/api/swagger-ui/</a><br><a href=\"/v2/api/swagger-ui/\">/v2/api/swagger-ui/</a><br><a href=\"/v3/api/swagger-ui/\">/v3/api/swagger-ui/</a><br><a href=\"/v4/api/swagger-ui/\">/v4/api/swagger-ui/</a><br></body>")
              -- are all versions listed?
              forM_ [minBound :: Version ..] $ \v -> liftIO $ assertBool (show v) ((cs (toQueryParam v) :: String) `isInfixOf` (cs . fromJust . responseBody $ r))
              -- FUTUREWORK: maybe test that no invalid versions are listed?  (that wouldn't
              -- be too terrible, though, just some dead links that aren't supposed to be
              -- there to begin with)
        ]
    ]

testSwaggerJson :: Brig -> ByteString -> Http ()
testSwaggerJson brig version = do
  -- if you change the request paths here, make sure to change test group "toc" below to contain live links!
  r <- get (brig . path (version <> "/api/swagger.json") . expect2xx)
  liftIO $ assertBool "json body" (isJust $ ((^? _Object) <=< responseBody) r)

testSwaggerUI :: Brig -> ByteString -> Http ()
testSwaggerUI brig version = do
  r <- get (brig . path (version <> "/api/swagger-ui") . expect2xx)
  liftIO $ assertBool "HTML body" ("<div id=\"swagger-ui\"></div>" `isInfixOf` (cs . fromJust . responseBody $ r))
