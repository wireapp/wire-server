{-# LANGUAGE RecordWildCards #-}

module Test.Wire.Defederation where

import Test.Wire.Util
import qualified Data.Aeson as Aeson
import Federator.MockServer
import Imports
import qualified Network.AMQP as Q
import Test.Hspec
import Wire.API.Federation.API.Common
import Wire.Defederation
import Wire.API.Federation.BackendNotifications
import Data.Domain
import Test.Tasty
import Test.Tasty.HUnit

deleteFederationDomainSpec :: IO TestSetup -> TestTree
deleteFederationDomainSpec setup = testGroup "Wire.BackendNotificationPusher.deleteFederationDomain"
  [ testCase "should fail on message decoding" $ do
      s <- setup
      envelope <- newFakeEnvelope
      let msg = Q.newMsg {Q.msgBody = Aeson.encode @[()] [], Q.msgContentType = Just "application/json"}
          respSuccess _ = pure ("application/json", Aeson.encode EmptyResponse)
      resps <- withTempMockFederator [] respSuccess .
        runTestAppT s $ deleteFederationDomainInner (msg, envelope)
      case resps of
        ((), []) -> pure ()
        _ -> assertFailure "Expected call to federation"
      readIORef envelope.acks `shouldReturn` 0
      readIORef envelope.rejections `shouldReturn` [True]
  , testCase "should succeed on message decoding" $ do
      s <- setup
      envelope <- newFakeEnvelope
      let msg = Q.newMsg
            { Q.msgBody = Aeson.encode @DefederationDomain (Domain "far-away.example.com")
            , Q.msgContentType = Just "application/json"
            }
          respSuccess _ = pure ("application/json", Aeson.encode EmptyResponse)
      resps <- withTempMockFederator [] respSuccess .
        runTestAppT s $ deleteFederationDomainInner (msg, envelope)
      case resps of
        ((), []) -> pure ()
        _ -> assertFailure "Expected call to federation"
      readIORef envelope.acks `shouldReturn` 1
      readIORef envelope.rejections `shouldReturn` []
  ]