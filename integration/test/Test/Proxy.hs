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
import qualified Network.Wai as Wai
import Servant
import Testlib.Mock
import Testlib.Prelude

testProxyGiphy :: App ()
testProxyGiphy = do
  lowerCodensity $ do
    port <- startMockServer def app
    lift
      $ withModifiedBackend
        def
          { wireProxyCfg =
              (setField "giphyEndpoint" (object ["host" .= "localhost", "port" .= port]))
                . (setField "disableTlsForTest" True)
          }
        ( \domain -> do
            getGiphy domain `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200
              -- the response from mock giphy is just passed through to the wire client.
              resp.json %. "apiKey" `shouldMatch` "my-giphy-secret"
              resp.json %. "q" `shouldMatch` "monday"
              resp.json %. "limit" `shouldMatchInt` 100
              resp.json %. "offset" `shouldMatchInt` 0

            getGiphyError domain `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 404
        )
  where
    app :: Wai.Application
    app = serve (Proxy :: Proxy GiphyAPI) server

    server :: Server GiphyAPI
    server (Just apiKey) (Just q) (Just limit) (Just offset) = pure (GiphyResponse {..})
    server _ _ _ _ = error "Unexpected"

data GiphyResponse = GiphyResponse
  { apiKey :: String,
    q :: String,
    limit :: Int,
    offset :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON GiphyResponse

instance ToJSON GiphyResponse

type GiphyAPI =
  "v1"
    :> "gifs"
    :> "search"
    :> QueryParam "api_key" String
    :> QueryParam "q" String
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] GiphyResponse
