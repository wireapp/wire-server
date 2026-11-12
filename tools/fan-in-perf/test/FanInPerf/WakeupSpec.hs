module FanInPerf.WakeupSpec (spec) where

import Control.Exception (AsyncException (ThreadKilled), ErrorCall (..), throwIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import FanInPerf.Service (wakeupPayload)
import FanInPerf.Wakeup
import Imports
import Test.Hspec

spec :: Spec
spec = do
  describe "participant routing" $ do
    let members = [("alice", "phone"), ("bob", "phone")] :: [(Text, Text)]
        route = participantDestinations members
    it "deduplicates a Cannon shared by multiple participants" $
      route [("alice", Just "phone", "cannon-a" :: Text), ("bob", Just "phone", "cannon-a")] `shouldBe` ["cannon-a"]
    it "routes to both Cannons and filters unrelated or unidentified devices" $
      route [("alice", Just "phone", "cannon-a" :: Text), ("bob", Just "phone", "cannon-b"), ("alice", Just "desktop", "cannon-c"), ("other", Just "phone", "cannon-d"), ("alice", Nothing, "cannon-e")] `shouldBe` ["cannon-a", "cannon-b"]
    it "has no destinations for offline members" $
      route ([] :: [(Text, Maybe Text, Text)]) `shouldBe` []
    it "uses the current presence after a client moves" $ do
      route [("alice", Just "phone", "cannon-a" :: Text)] `shouldBe` ["cannon-a"]
      route [("alice", Just "phone", "cannon-b" :: Text)] `shouldBe` ["cannon-b"]

  describe "forwardWakeup" $ do
    it "succeeds only after every destination succeeds" $ do
      calls <- newIORef (0 :: Int)
      let send = modifyIORef' calls (+ 1) $> True
      forwardWakeup [send, send] (encode wakeupPayload) `shouldReturn` Delivered
      readIORef calls `shouldReturn` 2
    it "retries if any destination fails, and still attempts the others" $ do
      calls <- newIORef False
      forwardWakeup [throwIO (ErrorCall "synthetic failure"), writeIORef calls True $> True] (encode wakeupPayload) `shouldReturn` Retry
      readIORef calls `shouldReturn` True
    it "retries unsuccessful HTTP delivery and empty target sets" $ do
      forwardWakeup [pure False] (encode wakeupPayload) `shouldReturn` Retry
      forwardWakeup [] (encode wakeupPayload) `shouldReturn` Retry
    it "does not forward malformed or oversized signals" $ do
      for_ ["{}", "invalid", BL.replicate 1025 32] $ \body ->
        forwardWakeup [expectationFailure "unexpected forwarding" $> True] body `shouldReturn` Invalid
    it "does not swallow cancellation" $
      forwardWakeup [throwIO ThreadKilled] (encode wakeupPayload) `shouldThrow` (== ThreadKilled)
  describe "Cannon wakeup hub" $ do
    it "broadcasts one signal independently to multiple readers" $ do
      hub <- newWakeupHub
      generation <- atomically $ currentGeneration hub
      atomically (signal hub)
      atomically (awaitWakeup hub generation) `shouldReturn` 1
      atomically (awaitWakeup hub generation) `shouldReturn` 1
    it "coalesces signals and waits once caught up" $ do
      hub <- newWakeupHub
      atomically $ replicateM_ 100 (signal hub)
      atomically (awaitWakeup hub 0) `shouldReturn` 100
      atomically ((Just <$> awaitWakeup hub 100) `orElse` pure Nothing) `shouldReturn` Nothing
