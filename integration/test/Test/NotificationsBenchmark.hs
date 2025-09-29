module Test.NotificationsBenchmark where

import API.Brig
import API.BrigCommon
import API.Common
import API.GundeckInternal
import Control.Concurrent
import Control.Monad.Codensity (Codensity (..))
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (local)
import qualified Data.Map.Strict as Map
import Data.Time
import Debug.Trace
import GHC.Conc (numCapabilities)
import GHC.Stack
import SetupHelpers
import qualified Streamly.Data.Fold.Prelude as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import System.Random
import qualified Test.Events as TestEvents
import Testlib.Prekeys
import Testlib.Prelude

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

  -- Preparation
  let parCfg = Stream.maxThreads (numCapabilities * 2) . Stream.ordered False
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
  let fakeData = zip (plusDelta now <$> [0 :: Word ..]) (cycle [0 .. 1000])

  Stream.fromList fakeData
    & Stream.filter (\(_t, uNo) -> (uNo `mod` shardingGroupCount) == shardingGroup)
    & Stream.parMapM parCfg (\(t, uNo) -> waitForTimeStamp t >> sendAndReceive uNo userMap)
    & Stream.fold Fold.drain

-- TODO: Add a speed factor to the simulation as we want to simulate faster than real time
waitForTimeStamp :: UTCTime -> App ()
waitForTimeStamp timestamp = liftIO $ do
  now <- getCurrentTime
  print $ "(timestamp, now)" ++ show (timestamp, now)
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
  let push =
        object
          [ "recipients" .= [r],
            "payload" .= [object ["foo" .= payload]]
          ]

  void $ postPush alice [push] >>= getBody 200

  forM_ (testRecipient.clientIds) $ \(cid :: String) ->
    runCodensity (TestEvents.createEventsWebSocket alice (Just cid)) $ \ws -> do
      -- TODO: Tweak this value to the least acceptable event delivery duration
      local (setTimeoutTo 120) $ TestEvents.assertFindsEvent ws $ \e -> do
        print "Event received"
        printJSON e
        e %. "payload" `shouldMatch` [object ["foo" .= payload]]
  where
    -- \| Generate a random string with random length up to 2048 bytes
    randomPayload :: IO String
    randomPayload = do
      -- TODO: 1 to 2028 chars is a guess. We could adjust it to the real distribution.
      len <- randomRIO @Int (1, 2048) -- random length between 1 and 2048
      mapM (\_ -> randomRIO ('\32', '\126')) [1 .. len] -- printable ASCII

setTimeoutTo :: Int -> Env -> Env
setTimeoutTo tSecs env = env {timeOutSeconds = tSecs}

generateTestRecipient :: (HasCallStack) => App TestRecipient
generateTestRecipient = do
  print "generateTestRecipient"
  user <- randomUser OwnDomain def
  r <- randomRIO @Word (1, 8)
  clientIds <- forM [0 .. r] $ \_ -> do
    client <-
      addClient
        user
        def
          { acapabilities = Just ["consumable-notifications"],
            prekeys = Just $ take 10 somePrekeysRendered,
            lastPrekey = Just $ head someLastPrekeysRendered
          }
        >>= getJSON 201
    objId client

  pure $ TestRecipient user clientIds
