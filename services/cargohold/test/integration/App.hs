{-# OPTIONS_GHC -Wno-unused-do-bind #-}

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
    [ test s "multiIngress and cloudFront cannot be combined" testMultiIngressCloudFrontFails,
      test s "multiIngress and s3DownloadEndpoint cannot be combined" testMultiIngressS3DownloadEndpointFails
    ]

testMultiIngressCloudFrontFails :: TestM ()
testMultiIngressCloudFrontFails = do
  ts <- ask
  let opts =
        view tsOpts ts
          & (Opts.optAws . Opts.awsCloudFront) ?~ cloudFrontOptions
          & (Opts.optAws . Opts.optMultiIngress) ?~ multiIngressMap
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
        { _cfDomain = Domain (T.pack "example.com"),
          _cfKeyPairId = KeyPairId (T.pack "anyId"),
          _cfPrivateKey = "any/path"
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
        view tsOpts ts
          & (Opts.optAws . Opts.awsS3DownloadEndpoint) ?~ toAWSEndpoint "http://fake-s3:4570"
          & (Opts.optAws . Opts.optMultiIngress) ?~ multiIngressMap
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
