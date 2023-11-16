{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.Cargohold.App where

import Control.Exception
import Control.Lens
import Data.ByteString.Conversion
import Data.Map qualified as Map
import Data.Text qualified as T
import Testlib.Prelude

testMultiIngressCloudFrontFails :: HasCallStack => App ()
testMultiIngressCloudFrontFails = do
  ts <- ask
  let opts =
        view tsOpts ts
          & (Opts.aws . Opts.cloudFront) ?~ cloudFrontOptions
          & (Opts.aws . Opts.multiIngress) ?~ multiIngressMap
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
        { _domain = Domain (T.pack "example.com"),
          _keyPairId = KeyPairId (T.pack "anyId"),
          _privateKey = "any/path"
        }

multiIngressMap :: Map String AWSEndpoint
multiIngressMap =
  Map.singleton
    "red.example.com"
    (toAWSEndpoint "http://s3-download.red.example.com")

toAWSEndpoint :: ByteString -> AWSEndpoint
toAWSEndpoint = fromJust . fromByteString

testMultiIngressS3DownloadEndpointFails :: HasCallStack => App ()
testMultiIngressS3DownloadEndpointFails = do
  ts <- ask
  let opts =
        view tsOpts ts
          & (Opts.aws . Opts.s3DownloadEndpoint) ?~ toAWSEndpoint "http://fake-s3:4570"
          & (Opts.aws . Opts.multiIngress) ?~ multiIngressMap
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
