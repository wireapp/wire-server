module FanInPerf.ProduceSpec (spec) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import FanInPerf.Produce
import FanInPerf.Stats
import FanInPerf.Targets hiding (spec)
import Imports
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec = do
  let (entries, _) = mkEntries 1 (TargetEntry KindTeam 3 2 :| []) (mkStdGen 1)
      payload = mkPayload 0

  describe "writerStep" $ do
    it "records a successful push with its target count" $ do
      ws <- newWriterStats
      _ <- writerStep (\_ -> pure PushOk) payload entries ws (\_ -> pure ()) (mkStdGen 2)
      s <- readSnapshot ws
      (pushesOf KindTeam s, targetsOf KindTeam s, totalErrors s) `shouldBe` (1, 2, 0)

    it "records a store error and reports it" $ do
      ws <- newWriterStats
      reported <- newIORef []
      _ <- writerStep (\_ -> pure (PushFailed "conflict")) payload entries ws (\m -> modifyIORef reported (m :)) (mkStdGen 2)
      s <- readSnapshot ws
      errorsOf KindTeam s `shouldBe` 1
      readIORef reported `shouldReturn` ["team: conflict"]

    it "counts an exception as error and keeps going" $ do
      ws <- newWriterStats
      reported <- newIORef []
      g <- writerStep (\_ -> throwIO (ErrorCall "boom")) payload entries ws (\m -> modifyIORef reported (m :)) (mkStdGen 2)
      _ <- writerStep (\_ -> pure PushOk) payload entries ws (\_ -> pure ()) g
      s <- readSnapshot ws
      (errorsOf KindTeam s, pushesOf KindTeam s) `shouldBe` (1, 1)
      readIORef reported >>= (`shouldSatisfy` any (T.isInfixOf "boom"))
