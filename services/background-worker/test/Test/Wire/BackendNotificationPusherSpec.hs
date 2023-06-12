{-# LANGUAGE RecordWildCards #-}

module Test.Wire.BackendNotificationPusherSpec where

import qualified Cassandra as Cass
import qualified Cassandra.Settings as Cass
import qualified Data.Aeson as Aeson
import Data.Domain
import Data.Range
import Federator.MockServer
import Imports
import qualified Network.AMQP as Q
import Network.HTTP.Client
import qualified System.Logger as Logger
import Test.Hspec
import Test.QuickCheck
import Util.Options
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Federation.BackendNotifications
import Wire.API.RawJson
import Wire.BackendNotificationPusher
import Wire.BackgroundWorker.Env
import URI.ByteString
import Data.Misc
import qualified Data.Proxy as P
import Galley.Types.Teams
import Data.Id
import Galley.Env (ExtEnv (..), initExtEnv)

runTestAppT :: AppT IO a -> Int -> IO a
runTestAppT app port = do
  http2Manager <- initHttp2Manager
  logger <- Logger.new Logger.defSettings
  manager <- newManager defaultManagerSettings
  cassandra' <- Cass.init Cass.defSettings
  extGetManager' <- _extGetManager <$> initExtEnv
  let federatorInternal = Endpoint "localhost" (fromIntegral port)
      localDomain = Domain "example.com"
      -- TODO: Check these vales
      galleyConversationCodeUri = HttpsUrl $ URI (Scheme "https") (pure $ Authority Nothing (Host "example.com") Nothing) "/" mempty Nothing
      legalHoldFlag = FeatureLegalHoldDisabledByDefault
      awsEnv = Nothing
      currentFanoutLimit = toRange $ P.Proxy @10
      brig' = Endpoint "" 0
      spar' = Endpoint "" 0
      gundeck' = Endpoint "" 0
      requestId' = RequestId ""
      deleteConvThrottle = Nothing
      env = Env {..}
  runAppT env app

spec :: Spec
spec = describe "Wire.BackendNotificationPusher" $ do
  it "should push notifications" $ do
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
      withTempMockFederator [] returnSuccess . runTestAppT $ do
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

  it "should reject invalid notifications" $ do
    let returnSuccess _ = pure ("application/json", Aeson.encode EmptyResponse)
    envelope <- newFakeEnvelope
    let msg =
          Q.newMsg
            { Q.msgBody = "unparseable notification",
              Q.msgContentType = Just "application/json"
            }
    (_, fedReqs) <-
      withTempMockFederator [] returnSuccess . runTestAppT $
        pushNotification (Domain "target.example.com") (msg, envelope)

    readIORef envelope.acks `shouldReturn` 0
    readIORef envelope.rejections `shouldReturn` [False]
    fedReqs `shouldBe` []

  it "should retry failed deliveries" $ do
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
      withTempMockFederator [] returnSuccessSecondTime . runTestAppT $ do
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

instance RabbitMQEnvelope FakeEnvelope where
  ack e = atomicModifyIORef' e.acks $ \a -> (a + 1, ())
  reject e requeueFlag = atomicModifyIORef' e.rejections $ \r -> (r <> [requeueFlag], ())

data FakeEnvelope = FakeEnvelope
  { rejections :: IORef [Bool],
    acks :: IORef Int
  }

newFakeEnvelope :: IO FakeEnvelope
newFakeEnvelope =
  FakeEnvelope
    <$> newIORef []
    <*> newIORef 0
