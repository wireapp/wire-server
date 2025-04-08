{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Wire.BackendNotificationPusherSpec where

import Control.Exception
import Control.Monad.Trans.Except
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Default
import Data.Domain
import Data.Id
import Data.Range
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Federator.MockServer
import Imports
import Network.AMQP qualified as Q
import Network.HTTP.Client (defaultManagerSettings, newManager, responseTimeoutNone)
import Network.HTTP.Media
import Network.HTTP.Types
import Network.RabbitMqAdmin
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as Wai
import Network.Wai.Utilities qualified as Wai
import Prometheus
import Servant hiding (respond)
import Servant.Client
import Servant.Client.Core
import Servant.Client.Internal.HttpClient (mkFailureResponse)
import Servant.Server.Generic
import Servant.Types.SourceT
import System.Logger.Class qualified as Logger
import Test.Hspec
import Test.QuickCheck
import Test.Wire.Util
import UnliftIO.Async
import Util.Options
import Wire.API.Conversation.Action
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import Wire.API.Federation.BackendNotifications
import Wire.API.RawJson
import Wire.BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options
import Wire.BackgroundWorker.Util

spec :: Spec
spec = do
  describe "pushNotification" $ do
    it "should push notifications" $ do
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
                body = RawJson $ Aeson.encode notifContent,
                bodyVersions = Nothing,
                requestId = Just $ RequestId defRequestId
              }
      envelope <- newMockEnvelope
      let msg =
            Q.newMsg
              { Q.msgBody = Aeson.encode notif,
                Q.msgContentType = Just "application/json"
              }
      runningFlag <- newMVar ()
      (env, fedReqs) <-
        withTempMockFederator def . runTestAppT $ do
          wait =<< pushNotification runningFlag targetDomain (msg, envelope)
          ask

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
      getVectorWith env.backendNotificationMetrics.pushedCounter getCounter
        `shouldReturn` [(domainText targetDomain, 1)]

    it "bad request should not clog the queue" $ do
      let origDomain = Domain "origin.example.com"
          targetDomain = Domain "target.example.com"
          badRequest = Wai.mkError status400 "bad-request" "Aeson parsing error"
          fedError =
            ( Wai.mkError
                (mkStatus 533 "federation-remote-error")
                "federation-remote-error"
                "A remote federator failed with status code: 400"
            )
              { Wai.innerError = Just badRequest
              }

          mockRemote :: p -> IO MockResponse
          mockRemote _ = do
            pure $ MockResponse (mkStatus 533 "federation-remote-error") "application/json" (Aeson.encode fedError)

          notifContent = Aeson.object ["foo" .= ("bar" :: Text)]
          notif =
            BackendNotification
              { targetComponent = Brig,
                ownDomain = origDomain,
                path = "/on-user-deleted-connections",
                body = RawJson $ Aeson.encode notifContent,
                bodyVersions = Nothing,
                requestId = Just $ RequestId defRequestId
              }
      envelope <- newMockEnvelope
      let msg =
            Q.newMsg
              { Q.msgBody = Aeson.encode notif,
                Q.msgContentType = Just "application/json"
              }
      runningFlag <- newMVar ()
      (env, _) <- withTempMockFederator def {handler = mockRemote} . runTestAppT $ do
        notification <- pushNotification runningFlag targetDomain (msg, envelope)
        void . liftIO . try @SomeException $ wait notification
        ask

      readIORef envelope.acks `shouldReturn` 0
      readIORef envelope.rejections `shouldReturn` [False]

      getVectorWith env.backendNotificationMetrics.pushedCounter getCounter
        `shouldReturn` []

      getVectorWith env.backendNotificationMetrics.stuckQueuesGauge getGauge
        `shouldReturn` [(domainText targetDomain, 0)]

      getVectorWith env.backendNotificationMetrics.errorCounter getCounter
        `shouldReturn` [(domainText targetDomain, 1)]

    it "should push notification bundles" $ do
      let origDomain = Domain "origin.example.com"
          targetDomain = Domain "target.example.com"
      -- Just using 'arbitrary' could generate a very big list, making tests very
      -- slow. Make me wonder if notification pusher should even try to parse the
      -- actual content, seems like wasted compute power.
      notifContent <-
        generate $
          ClientRemovedRequest <$> arbitrary <*> arbitrary <*> arbitrary
      let bundle = toBundle @'OnClientRemovedTag (RequestId defRequestId) origDomain notifContent
      envelope <- newMockEnvelope
      let msg =
            Q.newMsg
              { Q.msgBody = Aeson.encode bundle,
                Q.msgContentType = Just "application/json"
              }
      runningFlag <- newMVar ()
      (env, fedReqs) <-
        withTempMockFederator def . runTestAppT $ do
          wait =<< pushNotification runningFlag targetDomain (msg, envelope)
          ask

      readIORef envelope.acks `shouldReturn` 1
      readIORef envelope.rejections `shouldReturn` []
      fedReqs
        `shouldBe` [ FederatedRequest
                       { frTargetDomain = targetDomain,
                         frOriginDomain = origDomain,
                         frComponent = Galley,
                         frRPC = "on-client-removed",
                         frBody = Aeson.encode notifContent
                       }
                   ]
      getVectorWith env.backendNotificationMetrics.pushedCounter getCounter
        `shouldReturn` [(domainText targetDomain, 1)]

    it "should negotiate the best version" $ do
      let origDomain = Domain "origin.example.com"
          targetDomain = Domain "target.example.com"
      update <- generate $ do
        now <- arbitrary
        user <- arbitrary
        convId <- arbitrary
        pure
          ConversationUpdate
            { time = now,
              origUserId = user,
              convId = convId,
              alreadyPresentUsers = [],
              action = SomeConversationAction SConversationLeaveTag ()
            }
      let update0 = conversationUpdateToV0 update
      let bundle =
            toBundle (RequestId defRequestId) origDomain update
              <> toBundle (RequestId defRequestId) origDomain update0
      envelope <- newMockEnvelope
      let msg =
            Q.newMsg
              { Q.msgBody = Aeson.encode bundle,
                Q.msgContentType = Just "application/json"
              }
      runningFlag <- newMVar ()
      (env, fedReqs) <-
        withTempMockFederator def {versions = [0, 999999]} . runTestAppT $ do
          wait =<< pushNotification runningFlag targetDomain (msg, envelope)
          ask

      readIORef envelope.acks `shouldReturn` 1
      readIORef envelope.rejections `shouldReturn` []
      fedReqs
        `shouldBe` [ FederatedRequest
                       { frTargetDomain = targetDomain,
                         frOriginDomain = origDomain,
                         frComponent = Galley,
                         frRPC = "on-conversation-updated",
                         frBody = Aeson.encode update0
                       }
                   ]
      getVectorWith env.backendNotificationMetrics.pushedCounter getCounter
        `shouldReturn` [(domainText targetDomain, 1)]

    it "should reject invalid notifications" $ do
      envelope <- newMockEnvelope
      let msg =
            Q.newMsg
              { Q.msgBody = "unparseable notification",
                Q.msgContentType = Just "application/json"
              }
      runningFlag <- newMVar ()
      (env, fedReqs) <-
        withTempMockFederator def . runTestAppT $ do
          wait =<< pushNotification runningFlag (Domain "target.example.com") (msg, envelope)
          ask

      readIORef envelope.acks `shouldReturn` 0
      readIORef envelope.rejections `shouldReturn` [False]
      fedReqs `shouldBe` []
      getVectorWith env.backendNotificationMetrics.pushedCounter getCounter
        `shouldReturn` []

    it "should retry failed deliveries" $ do
      isRemoteBrokenRef <- newIORef True
      fedCalls <- newIORef (0 :: Int)
      let mockRemote :: req -> IO MockResponse
          mockRemote _ = do
            isRemoteBroken <- readIORef isRemoteBrokenRef
            atomicModifyIORef fedCalls $ \c -> (c + 1, ())
            pure $
              if isRemoteBroken
                then MockResponse status200 "text/html" "<marquee>down for maintenance</marquee>"
                else MockResponse status200 "application/json" (Aeson.encode EmptyResponse)
          origDomain = Domain "origin.example.com"
          targetDomain = Domain "target.example.com"
      notifContent <- generate $ UserDeletedConnectionsNotification <$> arbitrary <*> (unsafeRange . (: []) <$> arbitrary)
      let notif =
            BackendNotification
              { targetComponent = Brig,
                ownDomain = origDomain,
                path = "/on-user-deleted-connections",
                body = RawJson $ Aeson.encode notifContent,
                bodyVersions = Nothing,
                requestId = Just $ RequestId defRequestId
              }
      envelope <- newMockEnvelope
      let msg =
            Q.newMsg
              { Q.msgBody = Aeson.encode notif,
                Q.msgContentType = Just "application/json"
              }
      runningFlag <- newMVar ()
      env <- testEnv
      pushThread <-
        async $ withTempMockFederator def {handler = mockRemote} . runTestAppTWithEnv env $ do
          wait =<< pushNotification runningFlag targetDomain (msg, envelope)

      -- Wait for two calls, so we can be sure that the metric about stuck
      -- queues has been updated
      untilM $ (>= 2) <$> readIORef fedCalls
      getVectorWith env.backendNotificationMetrics.stuckQueuesGauge getGauge
        `shouldReturn` [(domainText targetDomain, 1)]

      -- Unstuck the queue
      writeIORef isRemoteBrokenRef False
      (_, fedReqs) <- wait pushThread

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
      forM_ fedReqs (`shouldBe` expectedReq)
      length fedReqs `shouldSatisfy` (>= 3)
      getVectorWith env.backendNotificationMetrics.stuckQueuesGauge getGauge
        `shouldReturn` [(domainText targetDomain, 0)]

  describe "getRemoteDomains" $ do
    it "should parse remoteDomains from queues with name starting with 'backend-notifications.' and ignore the other queues" $ do
      mockAdmin <-
        newMockRabbitMqAdmin
          False
          [ "backend-notifications.foo.example",
            "backend-notifications.bar.example",
            "some-other-queue",
            "backend-notifications.baz.example",
            "backend-notifications.not-a-domain"
          ]
      logger <- Logger.new Logger.defSettings
      httpManager <- newManager defaultManagerSettings
      let cassandra = undefined
      let federatorInternal = Endpoint "localhost" 8097
          http2Manager = undefined
          statuses = undefined
          rabbitmqAdminClient = Just $ mockRabbitMqAdminClient mockAdmin
          rabbitmqVHost = "test-vhost"
          defederationTimeout = responseTimeoutNone
          backendNotificationsConfig = BackendNotificationsConfig 1000 500000 1000

      backendNotificationMetrics <- mkBackendNotificationMetrics
      domains <- runAppT Env {..} $ getRemoteDomains (fromJust rabbitmqAdminClient)
      domains `shouldBe` map Domain ["foo.example", "bar.example", "baz.example"]
      readTVarIO mockAdmin.listQueuesVHostCalls `shouldReturn` ["test-vhost"]

    it "should retry fetching domains if a request fails" $ do
      mockAdmin <- newMockRabbitMqAdmin True ["backend-notifications.foo.example"]
      logger <- Logger.new Logger.defSettings
      let cassandra = undefined
      httpManager <- newManager defaultManagerSettings
      let federatorInternal = Endpoint "localhost" 8097
          http2Manager = undefined
          statuses = undefined
          rabbitmqAdminClient = Just $ mockRabbitMqAdminClient mockAdmin
          rabbitmqVHost = "test-vhost"
          defederationTimeout = responseTimeoutNone
          backendNotificationsConfig = BackendNotificationsConfig 1000 500000 1000
      backendNotificationMetrics <- mkBackendNotificationMetrics
      domainsThread <- async $ runAppT Env {..} $ getRemoteDomains (fromJust rabbitmqAdminClient)

      -- Wait for first call
      untilM (readTVarIO mockAdmin.listQueuesVHostCalls >>= \calls -> pure $ not $ null calls)

      -- Unbreak the API
      atomically $ writeTVar mockAdmin.broken False

      -- Now the thread can finish
      wait domainsThread `shouldReturn` [Domain "foo.example"]

      calls <- readTVarIO mockAdmin.listQueuesVHostCalls

      -- This test cannot guarantee how many times retries happened due to the
      -- concurrency, so just assert that there were at least 2 calls.
      calls `shouldSatisfy` (\c -> length c >= 2)
      mapM_ (\vhost -> vhost `shouldBe` rabbitmqVHost) calls

untilM :: (Monad m) => m Bool -> m ()
untilM action = do
  b <- action
  unless b $ untilM action

instance RabbitMQEnvelope MockEnvelope where
  ack e = atomicModifyIORef' e.acks $ \a -> (a + 1, ())
  reject e requeueFlag = atomicModifyIORef' e.rejections $ \r -> (r <> [requeueFlag], ())

data MockEnvelope = MockEnvelope
  { rejections :: IORef [Bool],
    acks :: IORef Int
  }

newMockEnvelope :: IO MockEnvelope
newMockEnvelope =
  MockEnvelope
    <$> newIORef []
    <*> newIORef 0

data MockRabbitMqAdmin = MockRabbitMqAdmin
  { broken :: TVar Bool,
    queues :: [Text],
    listQueuesVHostCalls :: TVar [Text]
  }

newMockRabbitMqAdmin :: Bool -> [Text] -> IO MockRabbitMqAdmin
newMockRabbitMqAdmin isBroken queues = do
  broken <- newTVarIO isBroken
  listQueuesVHostCalls <- newTVarIO []
  pure $ MockRabbitMqAdmin {..}

mockApi :: MockRabbitMqAdmin -> AdminAPI (AsServerT Servant.Handler)
mockApi mockAdmin =
  AdminAPI
    { listQueuesByVHost = mockListQueuesByVHost mockAdmin,
      deleteQueue = mockListDeleteQueue mockAdmin,
      listConnectionsByVHost = mockListConnectionsByVHost mockAdmin,
      deleteConnection = mockDeleteConnection mockAdmin
    }

mockListQueuesByVHost :: MockRabbitMqAdmin -> Text -> Text -> Bool -> Int -> Int -> Servant.Handler (Page Queue)
mockListQueuesByVHost MockRabbitMqAdmin {..} vhost _ _ _ _ = do
  atomically $ modifyTVar listQueuesVHostCalls (<> [vhost])
  readTVarIO broken >>= \case
    True -> throwError $ Servant.err500
    False ->
      pure
        Page
          { items = map (\n -> Queue n vhost) queues,
            pageCount = 1,
            page = 1
          }

mockListDeleteQueue :: MockRabbitMqAdmin -> Text -> Text -> Servant.Handler NoContent
mockListDeleteQueue _ _ _ = do
  pure NoContent

mockListConnectionsByVHost :: MockRabbitMqAdmin -> Text -> Servant.Handler [Connection]
mockListConnectionsByVHost _ _ = pure []

mockDeleteConnection :: MockRabbitMqAdmin -> Text -> Servant.Handler NoContent
mockDeleteConnection _ _ = pure NoContent

mockRabbitMqAdminApp :: MockRabbitMqAdmin -> Application
mockRabbitMqAdminApp mockAdmin = genericServe (mockApi mockAdmin)

mockRabbitMqAdminClient :: forall api. (api ~ ToServant AdminAPI AsApi) => MockRabbitMqAdmin -> AdminAPI (AsClientT IO)
mockRabbitMqAdminClient mockAdmin = fromServant $ hoistClient (Proxy @api) (flip runReaderT (mockRabbitMqAdminApp mockAdmin) . runWaiClient) (waiClient @api)

-- | Create servant client for an API, this can be run using 'hoistClient'.
waiClient :: forall api. (HasClient WaiClient api) => Client WaiClient api
waiClient = clientIn (Proxy @api) (Proxy @WaiClient)

-- | Runs a servant client by directly calling a wai application, instead of
-- talking to an HTTP server.
--
-- In theory, it should be possible to call a servant server directly, but this
-- requires too much internal knowledge of servant.
newtype WaiClient a = WaiClient {runWaiClient :: ReaderT Application IO a}
  deriving (Functor, Applicative, Monad, MonadReader Application, MonadIO)

instance RunClient WaiClient where
  runRequestAcceptStatus :: Maybe [Status] -> Request -> WaiClient Response
  runRequestAcceptStatus mAcceptedStatuses req = do
    let waiReq =
          Wai.defaultRequest
            { Wai.requestMethod = req.requestMethod,
              Wai.pathInfo = maybe [] (Text.splitOn "/") $ Text.stripPrefix "/" $ Text.decodeUtf8 $ LBS.toStrict $ Builder.toLazyByteString $ req.requestPath,
              Wai.queryString = toList $ req.requestQueryString,
              Wai.requestHeaders =
                (hAccept, renderHeader (toList req.requestAccept)) : toList req.requestHeaders,
              Wai.requestBody = case req.requestBody of
                Nothing -> pure mempty
                Just (RequestBodyLBS lbs, _) -> pure $ LBS.toStrict lbs
                Just (RequestBodyBS bs, _) -> pure bs
                Just (RequestBodySource src, _) ->
                  fmap (LBS.toStrict . mconcat)
                    . either error pure
                    =<< runExceptT (runSourceT src)
            }
    app <- ask
    respMVar <- newEmptyMVar
    void $ liftIO $ app waiReq $ \res -> do
      putMVar respMVar res
      pure Wai.ResponseReceived
    res <- waiResponseToServant =<< takeMVar respMVar
    let isGoodStatus = case mAcceptedStatuses of
          Nothing -> statusIsSuccessful res.responseStatusCode
          Just acceptedStatuses -> res.responseStatusCode `elem` acceptedStatuses
    if isGoodStatus
      then pure res
      else
        let burl =
              BaseUrl
                { baseUrlScheme = Http,
                  baseUrlHost = "localhost",
                  baseUrlPort = 80,
                  baseUrlPath = "/"
                }
         in throwClientError $ mkFailureResponse burl req res

  throwClientError :: ClientError -> WaiClient a
  throwClientError = liftIO . throwIO

waiResponseToServant :: (MonadIO m) => Wai.Response -> m Response
waiResponseToServant res = do
  let (status, hdrs, contBody) = Wai.responseToStream res
  body <- liftIO $ contBody $ \streamingBody -> do
    builderRef <- newIORef mempty
    streamingBody (\b -> modifyIORef' builderRef (b <>)) (pure ())
    Builder.toLazyByteString <$> readIORef builderRef
  pure
    Response
      { responseStatusCode = status,
        responseHeaders = Seq.fromList hdrs,
        responseHttpVersion = http11,
        responseBody = body
      }
