{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module API (tests) where

import API.Util
import Bilge hiding (body)
import Bilge.Assert
import CargoHold.API.Error
import CargoHold.Types
import qualified CargoHold.Types.V3 as V3
import Control.Exception (throw)
import Control.Lens hiding (sets, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C8
import Data.Domain
import Data.Id
import Data.Qualified
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy.Encoding as LText
import Data.Time.Clock
import Data.Time.Format
import Data.UUID.V4
import Federator.MockServer
import Imports hiding (head)
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Media ((//))
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Utilities.Error as Wai
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.Component

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API Integration"
    [ testGroup
        "remote"
        [ test s "remote download no asset" testRemoteDownloadNoAsset,
          test s "federator failure on remote download" testRemoteDownloadFederationFailure,
          test s "remote download" (testRemoteDownload "asset content"),
          test s "large remote download" $
            testRemoteDownload
              ( toLazyByteString
                  (mconcat (replicate 20000 (byteString "hello world\n")))
              )
        ]
    ]

--------------------------------------------------------------------------------
-- Federation behaviour

testRemoteDownloadNoAsset :: TestM ()
testRemoteDownloadNoAsset = do
  assetId <- liftIO $ Id <$> nextRandom
  uid <- liftIO $ Id <$> nextRandom
  let key = AssetKeyV3 assetId AssetPersistent
      qkey = Qualified key (Domain "faraway.example.com")
      respond req
        | frRPC req == "get-asset" =
            pure ("application" // "json", Aeson.encode (GetAssetResponse False))
        | otherwise =
            throw
              . MockErrorResponse HTTP.status404
              . LText.decodeUtf8With Text.lenientDecode
              . Aeson.encode
              $ assetNotFound
  (_, reqs) <- withMockFederator respond $ do
    downloadAsset uid qkey () !!! do
      const 404 === statusCode
  localDomain <- viewFederationDomain
  liftIO $
    reqs
      @?= [ FederatedRequest
              { frOriginDomain = localDomain,
                frTargetDomain = Domain "faraway.example.com",
                frComponent = Cargohold,
                frRPC = "get-asset",
                frBody = Aeson.encode (GetAsset uid key Nothing)
              }
          ]

testRemoteDownloadFederationFailure :: TestM ()
testRemoteDownloadFederationFailure = do
  assetId <- liftIO $ Id <$> nextRandom
  uid <- liftIO $ Id <$> nextRandom
  let key = AssetKeyV3 assetId AssetPersistent
      qkey = Qualified key (Domain "faraway.example.com")
      respond req
        | frRPC req == "get-asset" =
            pure ("application" // "json", Aeson.encode (GetAssetResponse True))
        | otherwise = throw (MockErrorResponse HTTP.status500 "mock error")
  (resp, _) <-
    withMockFederator respond $ do
      responseJsonError
        =<< downloadAsset uid qkey () <!! do
          const 500 === statusCode
  liftIO $ do
    Wai.label resp @?= "mock-error"
    Wai.message resp @?= "mock error"

testRemoteDownload :: LByteString -> TestM ()
testRemoteDownload assetContent = do
  assetId <- liftIO $ Id <$> nextRandom
  uid <- liftIO $ Id <$> nextRandom

  let key = AssetKeyV3 assetId AssetPersistent
      qkey = Qualified key (Domain "faraway.example.com")
      respond req
        | frRPC req == "get-asset" =
            pure ("application" // "json", Aeson.encode (GetAssetResponse True))
        | otherwise = pure ("application" // "octet-stream", assetContent)
  (_, reqs) <- withMockFederator respond $ do
    downloadAsset uid qkey () !!! do
      const 200 === statusCode
      const (Just assetContent) === responseBody

  localDomain <- viewFederationDomain
  let ga = Aeson.encode (GetAsset uid key Nothing)
  liftIO $
    reqs
      @?= [ FederatedRequest
              { frOriginDomain = localDomain,
                frTargetDomain = Domain "faraway.example.com",
                frComponent = Cargohold,
                frRPC = "get-asset",
                frBody = ga
              },
            FederatedRequest
              { frOriginDomain = localDomain,
                frTargetDomain = Domain "faraway.example.com",
                frComponent = Cargohold,
                frRPC = "stream-asset",
                frBody = ga
              }
          ]
