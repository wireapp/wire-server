module Test.NotificationsBenchmark where

import API.Brig
import API.BrigCommon
import API.Common
import API.GundeckInternal
import qualified Data.Map as Map
import GHC.Conc (numCapabilities)
import GHC.Stack
import SetupHelpers
import qualified Streamly.Data.Fold.Prelude as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import System.Random
import Testlib.Prekeys
import Testlib.Prelude

data TestRecipient = TestRecipient
  { user :: Value,
    clientIds :: [String]
  }
  deriving (Show)

testBench :: (HasCallStack) => App ()
testBench = do
  -- Preparation
  let parCfg = Stream.maxThreads (numCapabilities * 2) . Stream.ordered False
      toMap = Fold.foldl' (\kv (k, v) -> Map.insert k v kv) Map.empty
  -- Later, we only read from this map. Thus, it doesn't have to be thread-safe.
  userMap :: Map Word TestRecipient <- Stream.fromList [0 :: Word .. 1000] & Stream.parMapM parCfg (\i -> generateTestRecipient >>= \r -> pure (i, r)) & Stream.fold toMap

  -- To be replaced with real data from the file. (See
  -- https://wearezeta.atlassian.net/wiki/spaces/PET/pages/2118680620/Simulating+production-like+data)
  let fakeData = zip [0 :: Word ..] (cycle [0 .. 1000])

  -- TODO: Use the timestamp a calculate a delay
  Stream.fromList fakeData & Stream.parMapM parCfg (\(t, uNo) -> sendAndReceive uNo userMap) & Stream.fold Fold.drain

sendAndReceive :: Int -> Map Word TestRecipient -> App ()
sendAndReceive userNo userMap = do
  print $ "pushing to user" ++ show userNo
  let alice = (.user) $ userMap Map.! (fromIntegral userNo)
  r <- recipient alice
  let push =
        object
          [ "recipients" .= [r],
            "payload" .= [object ["foo" .= "bar"]]
          ]

  void $ postPush alice [push] >>= getBody 200

--  void $ withWebSocket alice $ \ws -> do
--    awaitMatch (\e -> printJSON e >> pure True) ws

generateTestRecipient :: (HasCallStack) => App TestRecipient
generateTestRecipient = do
  user <- randomUser OwnDomain def
  r <- randomRIO @Word (0, 8)
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
