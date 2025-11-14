{-# OPTIONS_GHC -Wno-unused-do-bind #-}

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

module App (tests) where

import CargoHold.App (newEnv)
import CargoHold.CloudFront
import CargoHold.Options as Opts
import Control.Exception
import Control.Lens
import Data.ByteString.Conversion
import qualified Data.Map as Map
import qualified Data.Text as T
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Util.Options

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Configuration sanity checks"
    -- The way that the `integration` tests are setup means that the error strings these
    -- tests look for are suppressed in a general time out message when the service can't start.
    [ test s "multiIngress and cloudFront cannot be combined" testMultiIngressCloudFrontFails,
      test s "multiIngress and s3DownloadEndpoint cannot be combined" testMultiIngressS3DownloadEndpointFails
    ]

testMultiIngressCloudFrontFails :: TestM ()
testMultiIngressCloudFrontFails = do
  ts <- ask
  let opts =
        view optsLens ts
          & (Opts.awsLens . Opts.cloudFrontLens) ?~ cloudFrontOptions
          & (Opts.awsLens . Opts.multiIngressLens) ?~ multiIngressMap
  msg <-
    liftIO $
      catch
        (newEnv opts >> pure "No exception")
        (\(SomeException e) -> pure $ displayException e)
  liftIO $
    assertBool
      "Check error message"
      (containsString "Invalid configuration: multiIngress and cloudFront cannot be combined!" msg)
  where
    cloudFrontOptions :: CloudFrontOpts
    cloudFrontOptions =
      CloudFrontOpts
        { domain = Domain (T.pack "example.com"),
          keyPairId = KeyPairId (T.pack "anyId"),
          privateKey = "any/path"
        }

multiIngressMap :: Map String AWSEndpoint
multiIngressMap =
  Map.singleton
    "red.example.com"
    (toAWSEndpoint "http://s3-download.red.example.com")

toAWSEndpoint :: ByteString -> AWSEndpoint
toAWSEndpoint = fromJust . fromByteString

testMultiIngressS3DownloadEndpointFails :: TestM ()
testMultiIngressS3DownloadEndpointFails = do
  ts <- ask
  let opts =
        view optsLens ts
          & (Opts.awsLens . Opts.s3DownloadEndpointLens) ?~ toAWSEndpoint "http://fake-s3:4570"
          & (Opts.awsLens . Opts.multiIngressLens) ?~ multiIngressMap
  msg <-
    liftIO $
      catch
        (newEnv opts >> pure "No exception")
        (\(SomeException e) -> pure $ displayException e)
  liftIO $
    assertBool
      "Check error message"
      (containsString "Invalid configuration: multiIngress and s3DownloadEndpoint cannot be combined!" msg)

containsString :: String -> String -> Bool
xs `containsString` ys = any (xs `isPrefixOf`) (tails ys)
