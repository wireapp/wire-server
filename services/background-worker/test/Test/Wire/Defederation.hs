module Test.Wire.Defederation where

import qualified Data.Aeson as Aeson
import Data.Domain
import Federator.MockServer
import Imports
import qualified Network.AMQP as Q
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Wire.Util
import Wire.API.Federation.API.Common
import Wire.API.Federation.BackendNotifications
import Wire.Defederation

deleteFederationDomainSpec :: IO TestSetup -> TestTree
deleteFederationDomainSpec setup =
  testGroup
    "Wire.BackendNotificationPusher.deleteFederationDomain"
    [ testCase "should fail on message decoding" $ do
        s <- setup
        envelope <- newFakeEnvelope
        let msg = Q.newMsg {Q.msgBody = Aeson.encode @[()] [], Q.msgContentType = Just "application/json"}
            respSuccess _ = pure ("application/json", Aeson.encode EmptyResponse)
        resps <-
          withTempMockFederator [] respSuccess
            . runTestAppT s
            $ deleteFederationDomainInner (msg, envelope)
        case resps of
          ((), []) -> pure ()
          _ -> assertFailure "Expected call to federation"
        readIORef envelope.acks `shouldReturn` 0
        -- Fail to decode should not be requeued
        readIORef envelope.rejections `shouldReturn` [False],
      testCase "should succeed on message decoding" $ do
        s <- setup
        envelope <- newFakeEnvelope
        let msg =
              Q.newMsg
                { Q.msgBody = Aeson.encode @DefederationDomain (Domain "far-away.example.com"),
                  Q.msgContentType = Just "application/json"
                }
            respSuccess _ = pure ("application/json", Aeson.encode EmptyResponse)
        resps <-
          withTempMockFederator [] respSuccess
            . runTestAppT s
            $ deleteFederationDomainInner (msg, envelope)
        case resps of
          ((), []) -> pure ()
          _ -> assertFailure "Expected call to federation"
        readIORef envelope.acks `shouldReturn` 1
        readIORef envelope.rejections `shouldReturn` []
    ]
