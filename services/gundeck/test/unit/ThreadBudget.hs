{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans #-}

module ThreadBudget where

import Imports

import Control.Concurrent.Async
import Control.Lens
import Data.Metrics.Middleware (metrics)
import Data.String.Conversions (cs)
import Gundeck.ThreadBudget
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Logger.Class as LC


----------------------------------------------------------------------
-- helpers

data LogEntry = NoBudget | Debug String | Unknown String
  deriving (Eq, Show)

makePrisms ''LogEntry

logHistory :: MVar [LogEntry]
logHistory = unsafePerformIO $ newMVar []

expectLogHistory :: HasCallStack => ([LogEntry] -> Bool) -> IO ()
expectLogHistory expected = do
  found <- modifyMVar logHistory (\found -> pure ([], found))
  expected (filter (isn't _Debug) found) @? ("unexpected log data: " <> show found)

enterLogHistory :: LogEntry -> IO ()
enterLogHistory entry = modifyMVar_ logHistory (\found -> pure (entry : found))

instance LC.MonadLogger IO where
  log level msg = do
    let raw :: String = cs $ LC.render LC.renderNetstr msg
        parsed
          | level == LC.Debug                               = Debug raw
          | "runWithBudget: out of budget." `isInfixOf` raw = NoBudget
          | otherwise                                       = Unknown raw
    enterLogHistory parsed


----------------------------------------------------------------------
-- TOC

tests :: TestTree
tests = testGroup "thread budgets" $
  [ testCase "works with mock actions" testThreadBudgets
  ]


----------------------------------------------------------------------
-- deterministic unit test

testThreadBudgets :: Assertion
testThreadBudgets = do
  mtr <- metrics
  tbs <- mkThreadBudgetState 5

  let delayms :: Int -> IO ()
      delayms = threadDelay . (* 1000)

      burstActions :: Int -> Int -> IO ()
      burstActions howlong howmany
        = void . forkIO . void $
          mapConcurrently (\jitter -> do
                              delayms jitter  -- (2 microseconds *should* enough on normal
                                              -- hardware, but we're doing 1 milisecond just
                                              -- to be safe.)
                              runWithBudget tbs $ delayms howlong)
                          [1..howmany]

  watcher <- async $ watchThreadBudgetState mtr tbs 1

  burstActions 1000 5
  delayms 100
  expectLogHistory null

  burstActions 1000 3
  delayms 100
  expectLogHistory (== [NoBudget, NoBudget, NoBudget])

  burstActions 1000 3
  delayms 100
  expectLogHistory (== [NoBudget, NoBudget, NoBudget])

  delayms 800

  burstActions 1000 3
  delayms 100
  expectLogHistory null

  burstActions 1000 3
  delayms 100
  expectLogHistory (== [NoBudget])

  cancel watcher


-- TODO: test watcher: we can kill runWithBudget with async exceptions, that should keep the
-- finalizer from running, no?  interesting to find out if that's true in its own right.  if
-- it's true, we can run a watcher every 1s (that's the highest frequency there is), run
-- long-running actions with runWithBudget, and kill them in a way that keeps the finalizer
-- from running.  then we can check we get the watcher to clean things up.  can we also check
-- that that's the *only* time the watcher gets something to do?
