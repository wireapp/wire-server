{-# LANGUAGE RecordWildCards #-}

module Test.Wire.BackendNotificationPusherSpec where

import Test.Wire.Util
import qualified Data.Aeson as Aeson
import Data.Domain
import Data.Range
import Federator.MockServer
import Imports
import qualified Network.AMQP as Q
import Test.Hspec
import Test.QuickCheck
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Federation.BackendNotifications
import Wire.API.RawJson
import Test.Tasty
import Test.Tasty.HUnit
import Wire.BackendNotificationPusher

spec :: IO TestSetup -> TestTree
spec setup = testGroup "Wire.BackendNotificationPusher" 
  [ testCase "should push notifications" $ do
      s <- setup
      let returnSuccess _ = pure ("application/json", Aeson.encode EmptyResponse)
      let origDomain = Domain "origin.example.com"
          targetDomain = Domain "target.example.com"
      -- Just using 'arbitrary' could generate a very big list, making tests very
      -- slow. Make me wonder if notification pusher should even try to parse the
      -- actual content, seems like wasted compute power.
      notifContent <- generate $ UserDeletedConnectionsNotification <$> arbitrary <*> (unsafeRange . (: []) <$> arbitrary)
      let notif =
            BackendNotification
              { targetComponent = Brig,
                ownDomain = origDomain,
                path = "/on-user-deleted-connections",
                body = RawJson $ Aeson.encode notifContent
              }
      envelope <- newFakeEnvelope
      let msg =
            Q.newMsg
              { Q.msgBody = Aeson.encode notif,
                Q.msgContentType = Just "application/json"
              }

      (_, fedReqs) <-
        withTempMockFederator [] returnSuccess . runTestAppT s $ do
          pushNotification targetDomain (msg, envelope)

      readIORef envelope.acks `shouldReturn` 1
      readIORef envelope.rejections `shouldReturn` []
      fedReqs
        `shouldBe` [ FederatedRequest
                       { frTargetDomain = targetDomain,
                         frOriginDomain = origDomain,
                         frComponent = Brig,
                         frRPC = "on-user-deleted-connections",
                         frBody = Aeson.encode notifContent
                       }
                   ]
  , testCase "should reject invalid notifications" $ do
      s <- setup
      let returnSuccess _ = pure ("application/json", Aeson.encode EmptyResponse)
      envelope <- newFakeEnvelope
      let msg =
            Q.newMsg
              { Q.msgBody = "unparseable notification",
                Q.msgContentType = Just "application/json"
              }
      (_, fedReqs) <-
        withTempMockFederator [] returnSuccess . runTestAppT s $
          pushNotification (Domain "target.example.com") (msg, envelope)

      readIORef envelope.acks `shouldReturn` 0
      readIORef envelope.rejections `shouldReturn` [False]
      fedReqs `shouldBe` []
  , testCase "should retry failed deliveries" $ do
      s <- setup
      isFirstReqRef <- newIORef True
      let returnSuccessSecondTime _ =
            atomicModifyIORef isFirstReqRef $ \isFirstReq ->
              if isFirstReq
                then (False, ("text/html", "<marquee>down for maintenance</marquee>"))
                else (False, ("application/json", Aeson.encode EmptyResponse))
          origDomain = Domain "origin.example.com"
          targetDomain = Domain "target.example.com"
      notifContent <- generate $ UserDeletedConnectionsNotification <$> arbitrary <*> (unsafeRange . (: []) <$> arbitrary)
      let notif =
            BackendNotification
              { targetComponent = Brig,
                ownDomain = origDomain,
                path = "/on-user-deleted-connections",
                body = RawJson $ Aeson.encode notifContent
              }
      envelope <- newFakeEnvelope
      let msg =
            Q.newMsg
              { Q.msgBody = Aeson.encode notif,
                Q.msgContentType = Just "application/json"
              }

      (_, fedReqs) <-
        withTempMockFederator [] returnSuccessSecondTime . runTestAppT s $ do
          pushNotification targetDomain (msg, envelope)

      readIORef envelope.acks `shouldReturn` 1
      readIORef envelope.rejections `shouldReturn` []
      let expectedReq =
            FederatedRequest
              { frTargetDomain = targetDomain,
                frOriginDomain = origDomain,
                frComponent = Brig,
                frRPC = "on-user-deleted-connections",
                frBody = Aeson.encode notifContent
              }
      fedReqs `shouldBe` [expectedReq, expectedReq]
  ]