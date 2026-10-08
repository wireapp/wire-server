module FanInPerf.ProduceSpec (spec) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.Domain (Domain (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import FanInPerf.Produce
import FanInPerf.Stats
import FanInPerf.Store (describeUsageError, truncateText)
import FanInPerf.Targets hiding (spec)
import Hasql.Errors (ConnectionError (NetworkingConnectionError))
import Hasql.Pool (UsageError (..))
import Imports
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec = do
  let dom = Domain "example.com"
      (entries, _) = mkEntries 1 (TargetEntry KindTeam 3 2 :| []) (mkStdGen 1)

  describe "writerStep" $ do
    it "records a successful push with its target count" $ do
      ws <- newWriterStats
      _ <- writerStep (\_ -> pure PushOk) dom entries ws (\_ -> pure ()) (mkStdGen 2)
      s <- readSnapshot ws
      (pushesOf KindTeam s, targetsOf KindTeam s, totalErrors s) `shouldBe` (1, 2, 0)

    it "records a store error and reports it" $ do
      ws <- newWriterStats
      reported <- newIORef []
      _ <- writerStep (\_ -> pure (PushFailed "conflict")) dom entries ws (\m -> modifyIORef reported (m :)) (mkStdGen 2)
      s <- readSnapshot ws
      errorsOf KindTeam s `shouldBe` 1
      readIORef reported `shouldReturn` ["team: conflict"]

    it "counts an exception as error and keeps going" $ do
      ws <- newWriterStats
      reported <- newIORef []
      g <- writerStep (\_ -> throwIO (ErrorCall "boom")) dom entries ws (\m -> modifyIORef reported (m :)) (mkStdGen 2)
      _ <- writerStep (\_ -> pure PushOk) dom entries ws (\_ -> pure ()) g
      s <- readSnapshot ws
      (errorsOf KindTeam s, pushesOf KindTeam s) `shouldBe` (1, 1)
      readIORef reported >>= (`shouldSatisfy` any (T.isInfixOf "boom"))

  describe "describeUsageError" $
    it "does not leak connection details" $ do
      let msg = describeUsageError (ConnectionError (NetworkingConnectionError "host=db.internal password=hunter2"))
      msg `shouldSatisfy` (not . T.isInfixOf "hunter2")
      msg `shouldSatisfy` (not . T.isInfixOf "db.internal")

  describe "truncateText" $ do
    it "truncates long text with an ellipsis" $
      truncateText 5 "abcdefgh" `shouldBe` "abcde…"
    it "keeps short text" $
      truncateText 5 "abc" `shouldBe` "abc"
