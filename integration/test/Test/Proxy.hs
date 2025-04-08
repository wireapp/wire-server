{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Proxy where

import API.Proxy
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Network.Wai as Wai
import Servant
import Testlib.Mock
import Testlib.Prelude

----------------------------------------------------------------------
-- giphy

type GiphyAPI =
  "v1"
    :> "gifs"
    :> Capture "path" String
    :> QueryParam "api_key" String -- (we could also use `QueryString` here, no deep reason why we don't)
    :> QueryParam "q" String
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] Value

giphyApp :: Wai.Application
giphyApp = serve (Proxy :: Proxy GiphyAPI) server
  where
    server :: Server GiphyAPI
    server mbPathSegment (Just apiKey) (Just q) (Just limit) (Just offset) =
      pure
        $ A.object
          [ "pathSegment" .= mbPathSegment,
            "apiKey" .= apiKey,
            "q" .= q,
            "limit" .= limit,
            "offset" .= offset
          ]
    server mbPathSegment mbApiKey mbQ mbLimit mbOffset =
      error $ "unexpected: " <> show (mbPathSegment, mbApiKey, mbQ, mbLimit, mbOffset)

testProxyGiphy :: App ()
testProxyGiphy = do
  lowerCodensity $ do
    port <- startMockServer def giphyApp
    lift
      $ withModifiedBackend
        def
          { wireProxyCfg =
              (setField "giphyEndpoint" (A.object ["host" .= "localhost", "port" .= port]))
                . (setField "disableTlsForTest" True)
          }
        ( \domain -> do
            getGiphy domain "search" [("q", "monday"), ("limit", "100"), ("offset", "0")] `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200
              -- the response from mock giphy is just passed through to the wire client.
              resp.json %. "pathSegment" `shouldMatch` (Just "search")
              resp.json %. "apiKey" `shouldMatch` "my-giphy-secret"
              resp.json %. "q" `shouldMatch` "monday"
              resp.json %. "limit" `shouldMatchInt` 100
              resp.json %. "offset" `shouldMatchInt` 0

            getGiphy domain "storch" [("q", "monday"), ("limit", "100"), ("offset", "0")] `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200

            getGiphy domain "search/more" [("q", "monday"), ("limit", "100"), ("offset", "0")] `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 404

            getGiphy domain "search" [("q", "monday"), ("limit", "true"), ("offset", "0")] `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 400
        )

----------------------------------------------------------------------
-- youtube

type YoutubeAPI =
  "youtube"
    :> "v3"
    :> Capture "path" String
    :> QueryString
    :> Get '[JSON] Value

youtubeApp :: Wai.Application
youtubeApp = serve (Proxy :: Proxy YoutubeAPI) server
  where
    server :: Server YoutubeAPI
    server pathSegment queryString =
      pure
        $ A.object
          [ "pathSegment" .= pathSegment,
            "queryString" .= show queryString
          ]

testProxyYoutube :: App ()
testProxyYoutube = do
  lowerCodensity $ do
    port <- startMockServer def youtubeApp
    lift
      $ withModifiedBackend
        def
          { wireProxyCfg =
              (setField "youtubeEndpoint" (A.object ["host" .= "localhost", "port" .= port]))
                . (setField "disableTlsForTest" True)
          }
        ( \domain -> do
            getYoutube domain "wef" [("gnarz", "true")] `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200
              -- the response from mock youtube is just passed through to the wire client.
              resp.json %. "pathSegment" `shouldMatch` "wef"
              resp.json %. "queryString" `shouldMatch` "[(\"key\",Just \"my-youtube-secret\"),(\"gnarz\",Just \"true\")]"
        )

----------------------------------------------------------------------
-- google maps

type GoogleMapsAPI =
  "maps"
    :> ( "api"
           :> "staticmap"
           :> QueryString
           :> Get '[JSON] Value
           :<|> "api"
             :> "geocode"
             :> Capture "path" String
             :> QueryString
             :> Get '[JSON] Value
       )

googleMapsApp :: Wai.Application
googleMapsApp = serve (Proxy :: Proxy GoogleMapsAPI) (server1 :<|> server2)
  where
    server1 queryString =
      pure
        $ A.object
          [ "queryString" .= show queryString
          ]

    server2 pathSegment queryString =
      pure
        $ A.object
          [ "pathSegment" .= pathSegment,
            "queryString" .= show queryString
          ]

testProxyGoogleMaps :: App ()
testProxyGoogleMaps = do
  lowerCodensity $ do
    port <- startMockServer def googleMapsApp
    lift
      $ withModifiedBackend
        def
          { wireProxyCfg =
              (setField "googleMapsEndpoint" (A.object ["host" .= "localhost", "port" .= port]))
                . (setField "disableTlsForTest" True)
          }
        ( \domain -> do
            getGoogleMaps domain "maps/api/geocode/path_segment" [("geocode", "true")] `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200
              -- the response from mock googleMaps is just passed through to the wire client.
              resp.json %. "pathSegment" `shouldMatch` "path_segment"
              resp.json %. "queryString" `shouldMatch` "[(\"key\",Just \"my-googlemaps-secret\"),(\"geocode\",Just \"true\")]"

            getGoogleMaps domain "maps/api/geocode/path_segment/invalid" [("geocode", "true")] `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 404

            getGoogleMaps domain "api/staticmap" [("staticmap", "true")] `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200
              -- the response from mock googleMaps is just passed through to the wire client.
              resp.json %. "queryString" `shouldMatch` "[(\"key\",Just \"my-googlemaps-secret\"),(\"staticmap\",Just \"true\")]"

            getGoogleMaps domain "api/staticmap/invalid" [("staticmap", "true")] `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 404
        )
