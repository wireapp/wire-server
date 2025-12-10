module Test.NotificationsBenchmark where

import API.Brig
import API.BrigCommon
import API.Common
import API.GundeckInternal
import Control.Concurrent
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TBQueue
import Control.Monad.Codensity (Codensity (..))
import Control.Monad.Reader (MonadReader (ask), asks)
import Control.Monad.Reader.Class (local)
import Control.Retry
import qualified Data.Map.Strict as Map
import Data.Time
import Debug.Trace
import GHC.Conc.Sync
import GHC.Stack
import SetupHelpers
import qualified Streamly.Data.Fold.Prelude as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import System.Random
import qualified Test.Events as TestEvents
import Testlib.Prekeys
import Testlib.Prelude
import UnliftIO (async, waitAnyCancel)

data TestRecipient = TestRecipient
  { user :: Value,
    clientIds :: [String]
  }
  deriving (Show)

testBench :: (HasCallStack) => App ()
testBench = do
  shardingGroupCount <- asks (.shardingGroupCount)
  shardingGroup <- asks (.shardingGroup)
  maxUserNo <- asks (.maxUserNo)
  env <- ask

  -- Preparation
  let threadCount = min numCapabilities (fromIntegral maxUserNo)
      parCfg = Stream.maxThreads threadCount . Stream.ordered False
      toMap = Fold.foldl' (\kv (k, v) -> Map.insert k v kv) Map.empty
  -- Later, we only read from this map. Thus, it doesn't have to be thread-safe.
  userMap :: Map Word TestRecipient <-
    Stream.fromList [0 :: Word .. maxUserNo]
      & Stream.filter ((shardingGroup ==) . (`mod` shardingGroupCount))
      & Stream.parMapM parCfg (\i -> generateTestRecipient >>= \r -> pure (i, r))
      & Stream.fold toMap

  now <- liftIO getCurrentTime

  -- TODO: To be replaced with real data from the file. (See
  -- https://wearezeta.atlassian.net/wiki/spaces/PET/pages/2118680620/Simulating+production-like+data)
  let fakeData = zip (plusDelta now <$> [0 :: Word ..]) (cycle [1 .. maxUserNo])

  tbchans :: [(Int, TBQueue (UTCTime, Word))] <- liftIO $ forM [1 .. threadCount] $ \i -> do
    q <- atomically $ newTBQueue 1 -- capacity 100, adjust as needed
    pure (i, q)

  workers :: [Async ()] <- forM tbchans $ \(i, chan) -> liftIO
    . async
    -- . handle (\(e :: SomeException) -> traceM $ "Caught exception in worker id=" <> show i <> " e=" <> show e)
    . forever
    . runAppWithEnv env
    $ do
      traceM $ "worker taking from id=" <> show i
      (t, uNo) <- liftIO $ atomically $ readTBQueue chan
      traceM $ "worker took from id=" <> show i <> " val=" <> show (t, uNo)
      sendAndReceive uNo userMap
      traceM $ "worker end id=" <> show i

  producer <- async $ forM_ fakeData $ \(t, uNo) ->
    when ((uNo `mod` shardingGroupCount) == shardingGroup)
      $ let workerShard = fromIntegral uNo `mod` threadCount
            (i, chan) = tbchans !! workerShard
         in do
              waitForTimeStamp t
              traceM $ "producer putting to shard=" <> show workerShard <> " id=" <> show i <> " val=" <> show (t, uNo)
              liftIO $ atomically $ writeTBQueue chan (t, uNo)

  liftIO . void . waitAnyCancel $ producer : workers

waitForTimeStamp :: UTCTime -> App ()
waitForTimeStamp timestamp = liftIO $ do
  now <- getCurrentTime
  when (now < timestamp)
    $
    -- Event comes from the simulated future: Wait here until now and timestamp are aligned.
    let delta = diffTimeToMicroSeconds $ diffUTCTime timestamp now
     in print ("Waiting " ++ show delta ++ " microseconds. (timestamp, now)" ++ show (timestamp, now))
          >> threadDelay delta
  where
    diffTimeToMicroSeconds :: NominalDiffTime -> Int
    diffTimeToMicroSeconds dt = floor @Double (realToFrac dt * 1_000_000)

plusDelta :: UTCTime -> Word -> UTCTime
plusDelta timestamp deltaMilliSeconds = addUTCTime (fromIntegral deltaMilliSeconds / 1000) timestamp

sendAndReceive :: Word -> Map Word TestRecipient -> App ()
sendAndReceive userNo userMap = do
  print $ "pushing to user" ++ show userNo
  let testRecipient = userMap Map.! (fromIntegral userNo)
      alice = testRecipient.user

  r <- recipient alice
  payload :: Value <- toJSON <$> liftIO randomPayload
  now <- liftIO $ getCurrentTime
  let push =
        object
          [ "recipients" .= [r],
            "payload"
              .= [ object
                     [ "foo" .= payload,
                       "sent_at" .= now
                     ]
                 ]
          ]

  void $ postPush alice [push] >>= getBody 200

  traceM $ "pushed to userNo=" <> show userNo <> " push=" <> show push

  messageDeliveryTimeout <- asks $ fromIntegral . (.maxDeliveryDelay)
  traceM $ "XXX all clientIds " <> show testRecipient.clientIds
  forM_ (testRecipient.clientIds) $ \(cid :: String) -> do
    traceM $ "XXX using clientId=" <> show cid
    runCodensity (TestEvents.createEventsWebSocket alice (Just cid)) $ \ws -> do
      -- TODO: Tweak this value to the least acceptable event delivery duration
      local (setTimeoutTo messageDeliveryTimeout) $ TestEvents.assertFindsEvent ws $ \e -> do
        receivedAt <- liftIO getCurrentTime
        p <- e %. "data.event.payload.0"
        sentAt <-
          (p %. "sent_at" >>= asString)
            <&> ( \sentAtStr ->
                    fromMaybe (error ("Cannot parse timestamp: " <> sentAtStr)) $ parseUTC sentAtStr
                )
        print $ "Message sent/receive delta: " ++ show (diffUTCTime receivedAt sentAt)

        p %. "foo" `shouldMatch` payload
      traceM $ "XXX Succeeded for clientId=" <> show cid
  where
    -- \| Generate a random string with random length up to 2048 bytes
    randomPayload :: IO String
    randomPayload =
      -- Measured with
      -- `kubectl exec --namespace databases -it gundeck-gundeck-eks-eu-west-1a-sts-0 -- sh -c 'cqlsh -e "select  blobAsText(payload) from gundeck.notifications LIMIT 5000;" ' | sed 's/^[ \t]*//;s/[ \t]*$//' | wc`
      let len :: Int = 884 -- measured in prod
       in mapM (\_ -> randomRIO ('\32', '\126')) [1 .. len] -- printable ASCII
    parseUTC :: String -> Maybe UTCTime
    parseUTC = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

setTimeoutTo :: Int -> Env -> Env
setTimeoutTo tSecs env = env {timeOutSeconds = tSecs}

generateTestRecipient :: (HasCallStack) => App TestRecipient
generateTestRecipient = do
  print "generateTestRecipient"
  user <- recover $ (randomUser OwnDomain def)
  r <- randomRIO @Int (1, 8)
  clientIds <- forM [0 .. r] $ \i -> do
    client <-
      recover
        $ addClient
          user
          def
            { acapabilities = Just ["consumable-notifications"],
              prekeys = Nothing,
              lastPrekey = Just $ (someLastPrekeysRendered !! i),
              clabel = "Test Client No. " <> show i,
              model = "Test Model No. " <> show i
            }
        >>= getJSON 201
    objId client

  pure $ TestRecipient user clientIds
  where
    recover :: App a -> App a
    recover = recoverAll (limitRetriesByCumulativeDelay 300 (exponentialBackoff 1_000_000)) . const
