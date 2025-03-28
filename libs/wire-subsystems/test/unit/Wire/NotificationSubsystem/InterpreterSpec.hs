module Wire.NotificationSubsystem.InterpreterSpec (spec) where

import Control.Concurrent.Async (async, wait)
import Control.Exception (throwIO)
import Data.Data (Proxy (Proxy))
import Data.Default
import Data.Id
import Data.List1 qualified as List1
import Data.Range (toRange)
import Data.Set qualified as Set
import Data.String.Conversions
import Data.Time.Clock.DiffTime
import Imports
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.Async (Async, asyncToIOFinal, await)
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Wire.API.Push.V2 qualified as V2
import Wire.GundeckAPIAccess
import Wire.GundeckAPIAccess qualified as GundeckAPIAccess
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Interpreter
import Wire.Sem.Delay
import Wire.Sem.Logger.TinyLog

spec :: Spec
spec = describe "NotificationSubsystem.Interpreter" do
  describe "pushImpl" do
    it "chunks and sends all notifications" do
      let mockConfig =
            NotificationSubsystemConfig
              { fanoutLimit = toRange $ Proxy @30,
                chunkSize = 12,
                slowPushDelay = 0,
                requestId = RequestId defRequestId
              }

      connId2 <- generate arbitrary
      origin2 <- generate arbitrary
      (user1, user21, user22) <- generate arbitrary
      (payload1, payload2) <- generate $ resize 1 arbitrary
      clients1 <- generate $ resize 3 arbitrary
      lotOfRecipients <- generate $ resize 24 arbitrary
      apsData <- generate arbitrary
      let push1 =
            def
              { transient = True,
                route = V2.RouteDirect,
                recipients = [Recipient user1 (V2.RecipientClientsSome clients1)],
                json = payload1
              }
          push2 =
            def
              { conn = Just connId2,
                transient = True,
                nativePriority = Just V2.LowPriority,
                origin = Just origin2,
                recipients =
                  [ Recipient user21 V2.RecipientClientsAll,
                    Recipient user22 V2.RecipientClientsAll
                  ],
                json = payload2,
                apsData = Just apsData
              }
          duplicatePush = push2
          duplicatePushWithPush1Recipients = push2 {recipients = push1.recipients}
          largePush = push2 {recipients = lotOfRecipients}
          pushes :: [Push] =
            [ push1,
              push2,
              duplicatePush,
              duplicatePushWithPush1Recipients,
              largePush
            ]

      (_, actualPushes) <- runMiniStack mockConfig $ pushImpl pushes

      let expectedPushes =
            map toV2Push
              <$>
              -- It's ok to use chunkPushes here because we're testing
              -- that separately
              chunkPushes mockConfig.chunkSize pushes
      actualPushes `shouldBe` expectedPushes

    it "respects maximum fanout limit" do
      let mockConfig =
            NotificationSubsystemConfig
              { fanoutLimit = toRange $ Proxy @30,
                chunkSize = 12,
                slowPushDelay = 0,
                requestId = RequestId defRequestId
              }

      connId2 <- generate arbitrary
      origin2 <- generate arbitrary
      (user21, user22) <- generate arbitrary
      (payload1, payload2) <- generate $ resize 1 arbitrary
      lotOfRecipients <- replicateM 31 (generate arbitrary)
      apsData <- generate arbitrary
      let pushBiggerThanFanoutLimit =
            def
              { transient = True,
                route = V2.RouteDirect,
                recipients = lotOfRecipients,
                json = payload1
              }
          pushSmallerThanFanoutLimit =
            def
              { conn = Just connId2,
                transient = True,
                nativePriority = Just V2.LowPriority,
                origin = Just origin2,
                recipients =
                  [ Recipient user21 V2.RecipientClientsAll,
                    Recipient user22 V2.RecipientClientsAll
                  ],
                json = payload2,
                apsData = Just apsData
              }
          pushes =
            [ pushBiggerThanFanoutLimit,
              pushSmallerThanFanoutLimit
            ]

      (_, actualPushes) <- runMiniStack mockConfig $ pushImpl pushes

      let expectedPushes =
            map toV2Push
              <$>
              -- It's ok to use chunkPushes here because we're testing
              -- that separately
              chunkPushes mockConfig.chunkSize [pushSmallerThanFanoutLimit]
      actualPushes `shouldBe` expectedPushes

  describe "pushSlowlyImpl" do
    it "sends each push one by one with a delay" do
      let mockConfig =
            NotificationSubsystemConfig
              { fanoutLimit = toRange $ Proxy @30,
                chunkSize = 12,
                slowPushDelay = 1,
                requestId = RequestId defRequestId
              }

      connId2 <- generate arbitrary
      origin2 <- generate arbitrary
      (user1, user21, user22) <- generate arbitrary
      (payload1, payload2) <- generate $ resize 1 arbitrary
      clients1 <- generate $ resize 3 arbitrary
      let push1 =
            def
              { transient = True,
                route = V2.RouteDirect,
                recipients = [Recipient user1 (V2.RecipientClientsSome clients1)],
                json = payload1
              }
          push2 =
            def
              { conn = Just connId2,
                transient = True,
                nativePriority = Just V2.LowPriority,
                origin = Just origin2,
                recipients =
                  [ Recipient user21 V2.RecipientClientsAll,
                    Recipient user22 V2.RecipientClientsAll
                  ],
                json = payload2
              }
          pushes = [push1, push2]

      actualPushesRef <- newIORef []
      delayControl <- newEmptyMVar
      slowPushThread <-
        async $
          runMiniStackWithControlledDelay mockConfig delayControl actualPushesRef $
            pushSlowlyImpl pushes

      putMVar delayControl (diffTimeToFullMicroseconds mockConfig.slowPushDelay)
      actualPushes1 <- timeout 100_000 $ (waitUntilPushes actualPushesRef 1)
      actualPushes1 `shouldBe` Just [[toV2Push push1]]

      putMVar delayControl (diffTimeToFullMicroseconds mockConfig.slowPushDelay)
      actualPushes2 <- timeout 100_000 $ (waitUntilPushes actualPushesRef 2)
      actualPushes2 `shouldBe` Just [[toV2Push push1], [toV2Push push2]]

      timeout 100_000 (wait slowPushThread) `shouldReturn` Just ()

  describe "pushAsyncImpl" do
    it "logs errors" do
      let mockConfig =
            NotificationSubsystemConfig
              { fanoutLimit = toRange $ Proxy @30,
                chunkSize = 12,
                slowPushDelay = 1,
                requestId = RequestId defRequestId
              }

      user1 <- generate arbitrary
      payload1 <- generate $ resize 1 arbitrary
      clients1 <- generate $ resize 3 arbitrary
      let push1 =
            def
              { transient = True,
                route = V2.RouteDirect,
                recipients = [Recipient user1 (V2.RecipientClientsSome clients1)],
                json = payload1
              }
      (_, attemptedPushes, logs) <- runMiniStackAsync mockConfig $ do
        thread <- pushAsyncImpl push1
        await thread

      attemptedPushes `shouldBe` [[toV2Push push1]]
      map fst logs `shouldBe` [Error]
      cs (head (map snd logs)) `shouldContain` "error=TestException"

  describe "toV2Push" do
    it "does the transformation correctly" $ property \(pushToUser :: Push) ->
      let v2Push = toV2Push pushToUser
       in -- Statically determined
          v2Push._pushConnections === mempty
            .&&. v2Push._pushNativeIncludeOrigin === True
            .&&. v2Push._pushNativeEncrypt === True
            .&&. v2Push._pushNativeAps === Nothing
            -- derived from pushToUser
            .&&. v2Push._pushOrigin === pushToUser.origin
            .&&. v2Push._pushOriginConnection === pushToUser.conn
            .&&. v2Push._pushTransient === pushToUser.transient
            .&&. v2Push._pushNativePriority === fromMaybe V2.HighPriority pushToUser.nativePriority
            .&&. v2Push._pushPayload === List1.singleton pushToUser.json
            .&&. Set.map V2._recipientRoute v2Push._pushRecipients
              === Set.fromList (map (const pushToUser.route) (toList v2Push._pushRecipients))
            .&&. Set.map (\r -> Recipient r._recipientId r._recipientClients) v2Push._pushRecipients
              === Set.fromList (toList pushToUser.recipients)

  describe "chunkPushes" do
    it "allows empty push" $ property \limit ->
      chunkPushes limit [] === []
    it "produces no empty chunks" $ property \limit pushes ->
      not (any null (chunkPushes limit pushes))
    it "allows concatenation if number was non-zero" $ property \(Positive limit) pushes ->
      (chunkPushes limit pushes >>= reverse >>= normalisePush)
        === (pushes >>= normalisePush)
    it "respects the chunkSize limit" $ property \limit pushes ->
      all ((<= limit) . sizeOfChunks) (chunkPushes limit pushes)

runMiniStack :: NotificationSubsystemConfig -> Sem [Input NotificationSubsystemConfig, Delay, GundeckAPIAccess, Embed IO, Async, Final IO] a -> IO (a, [[V2.Push]])
runMiniStack mockConfig action = do
  actualPushesRef <- newIORef []
  x <-
    runFinal
      . asyncToIOFinal
      . embedToFinal @IO
      . runGundeckAPIAccessIORef actualPushesRef
      . runDelayInstantly
      . runInputConst mockConfig
      $ action
  (x,) <$> readIORef actualPushesRef

runMiniStackAsync :: NotificationSubsystemConfig -> Sem [Input NotificationSubsystemConfig, Delay, GundeckAPIAccess, P.TinyLog, Embed IO, Async, Final IO] a -> IO (a, [[V2.Push]], [(Level, LByteString)])
runMiniStackAsync mockConfig action = do
  actualPushesRef <- newIORef []
  lr <- newLogRecorder
  x <-
    runFinal
      . asyncToIOFinal
      . embedToFinal @IO
      . recordLogs lr
      . runGundeckAPIAccessFailure actualPushesRef
      . runDelayInstantly
      . runInputConst mockConfig
      $ action
  (x,,) <$> readIORef actualPushesRef <*> readIORef lr.recordedLogs

runMiniStackWithControlledDelay ::
  NotificationSubsystemConfig ->
  MVar Int ->
  IORef [[V2.Push]] ->
  Sem [Input NotificationSubsystemConfig, Delay, GundeckAPIAccess, Embed IO, Async, Final IO] a ->
  IO a
runMiniStackWithControlledDelay mockConfig delayControl actualPushesRef = do
  runFinal
    . asyncToIOFinal
    . embedToFinal @IO
    . runGundeckAPIAccessIORef actualPushesRef
    . runControlledDelay delayControl
    . runInputConst mockConfig

runGundeckAPIAccessFailure :: forall r a. (Member (Embed IO) r) => IORef [[V2.Push]] -> Sem (GundeckAPIAccess : r) a -> Sem r a
runGundeckAPIAccessFailure pushesRef =
  interpret $ \action -> do
    let unexpectedCall :: forall x. Sem r x
        unexpectedCall = do
          liftIO $ expectationFailure $ "Unexpected call to GundeckAPI: " <> show action
          error "impossible"
    case action of
      PushV2 pushes -> liftIO $ do
        modifyIORef pushesRef (<> [pushes])
        throwIO TestException
      GundeckAPIAccess.UserDeleted {} -> unexpectedCall
      GundeckAPIAccess.UnregisterPushClient {} -> unexpectedCall
      GundeckAPIAccess.GetPushTokens {} -> unexpectedCall
      GundeckAPIAccess.RegisterConsumableNotificationsClient {} -> unexpectedCall

data TestException = TestException
  deriving (Show)

instance Exception TestException

runGundeckAPIAccessIORef :: forall r a. (Member (Embed IO) r) => IORef [[V2.Push]] -> Sem (GundeckAPIAccess : r) a -> Sem r a
runGundeckAPIAccessIORef pushesRef =
  interpret \action -> do
    let unexpectedCall :: forall x. Sem r x
        unexpectedCall = do
          liftIO $ expectationFailure $ "Unexpected call to GundeckAPI " <> show action
          error "impossible"
    case action of
      PushV2 pushes -> modifyIORef pushesRef (<> [pushes])
      GundeckAPIAccess.UserDeleted {} -> unexpectedCall
      GundeckAPIAccess.UnregisterPushClient {} -> unexpectedCall
      GundeckAPIAccess.GetPushTokens {} -> unexpectedCall
      GundeckAPIAccess.RegisterConsumableNotificationsClient {} -> unexpectedCall

waitUntilPushes :: IORef [a] -> Int -> IO [a]
waitUntilPushes pushesRef n = do
  ps <- readIORef pushesRef
  -- This thread delay ensures that this function yields to other work as it
  -- is really just waiting for other threads to do work.
  if length ps >= n
    then pure ps
    else threadDelay 1000 >> waitUntilPushes pushesRef n

normalisePush :: Push -> [Push]
normalisePush p =
  map
    (\r -> p {recipients = [r]})
    (toList p.recipients)

sizeOfChunks :: [Push] -> Natural
sizeOfChunks = fromIntegral . sum . map (length . (.recipients))
