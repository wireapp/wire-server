{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Wire.BackendNotificationPusherSpec where

import Control.Exception
import Control.Monad.Trans.Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Range
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Federator.MockServer
import Imports
import qualified Network.AMQP as Q
import Network.HTTP.Media
import Network.HTTP.Types
import Network.RabbitMqAdmin
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import Servant hiding (respond)
import Servant.Client
import Servant.Client.Core
import Servant.Client.Internal.HttpClient (mkFailureResponse)
import Servant.Server.Generic
import Servant.Types.SourceT
import qualified System.Logger as Logger
import Test.Hspec
import Test.QuickCheck
import UnliftIO.Async
import Util.Options
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Federation.BackendNotifications
import Wire.API.RawJson
import Wire.BackendNotificationPusher
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options

runTestAppT :: AppT IO a -> Int -> IO a
runTestAppT app port = do
  http2Manager <- initHttp2Manager
  logger <- Logger.new Logger.defSettings
  statuses <- newIORef mempty
  let federatorInternal = Endpoint "localhost" (fromIntegral port)
      rabbitmqAdminClient = undefined
      rabbitmqVHost = undefined
      metrics = undefined
      backendNotificationPusher = BackendNotificationPusherOpts 1
      env = Env {..}
  runAppT env app

spec :: Spec
spec = do
  describe "pushNotification" $ do
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
      envelope <- newMockEnvelope
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
      envelope <- newMockEnvelope
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
      envelope <- newMockEnvelope
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
      let federatorInternal = undefined
          http2Manager = undefined
          statuses = undefined
          metrics = undefined
          backendNotificationPusher = BackendNotificationPusherOpts 1
          rabbitmqAdminClient = mockRabbitMqAdminClient mockAdmin
          rabbitmqVHost = "test-vhost"
      domains <- runAppT Env {..} getRemoteDomains
      domains `shouldBe` map Domain ["foo.example", "bar.example", "baz.example"]
      readTVarIO mockAdmin.listQueuesVHostCalls `shouldReturn` ["test-vhost"]

    it "should retry fetching domains if a request fails" $ do
      mockAdmin <- newMockRabbitMqAdmin True ["backend-notifications.foo.example"]
      logger <- Logger.new Logger.defSettings
      let federatorInternal = undefined
          http2Manager = undefined
          statuses = undefined
          metrics = undefined
          backendNotificationPusher = BackendNotificationPusherOpts 1
          rabbitmqAdminClient = mockRabbitMqAdminClient mockAdmin
          rabbitmqVHost = "test-vhost"
      domainsThread <- async $ runAppT Env {..} getRemoteDomains

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

untilM :: Monad m => m Bool -> m ()
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
    { listQueuesByVHost = mockListQueuesByVHost mockAdmin
    }

mockListQueuesByVHost :: MockRabbitMqAdmin -> Text -> Servant.Handler [Queue]
mockListQueuesByVHost MockRabbitMqAdmin {..} vhost = do
  atomically $ modifyTVar listQueuesVHostCalls (<> [vhost])
  readTVarIO broken >>= \case
    True -> throwError $ Servant.err500
    False -> pure $ map (\n -> Queue n vhost) queues

mockRabbitMqAdminApp :: MockRabbitMqAdmin -> Application
mockRabbitMqAdminApp mockAdmin = genericServe (mockApi mockAdmin)

mockRabbitMqAdminClient :: forall api. (api ~ ToServant AdminAPI AsApi) => MockRabbitMqAdmin -> AdminAPI (AsClientT IO)
mockRabbitMqAdminClient mockAdmin = fromServant $ hoistClient (Proxy @api) (flip runReaderT (mockRabbitMqAdminApp mockAdmin) . runWaiClient) (waiClient @api)

-- | Create servant client for an API, this can be run using 'hoistClient'.
waiClient :: forall api. HasClient WaiClient api => Client WaiClient api
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

waiResponseToServant :: MonadIO m => Wai.Response -> m Response
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
