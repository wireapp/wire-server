{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans #-}

module ThreadBudget where

import Imports

import Control.Concurrent.Async
import Control.Lens
import Data.Metrics.Middleware (metrics)
import Data.String.Conversions (cs)
import Data.TreeDiff.Class (ToExpr)
import GHC.Generics
import Gundeck.ThreadBudget
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.StateMachine
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified System.Logger.Class as LC
import qualified Test.StateMachine.Types as STM
import qualified Test.StateMachine.Types.Rank2 as Rank2


----------------------------------------------------------------------
-- helpers

data LogEntry = NoBudget | Debug String | Unknown String
  deriving (Eq, Show)

makePrisms ''LogEntry

expectLogHistory :: (HasCallStack, MonadReader (MVar [LogEntry]) m, MonadIO m) => ([LogEntry] -> Bool) -> m ()
expectLogHistory expected = do
  logHistory <- ask
  liftIO $ do
    found <- modifyMVar logHistory (\found -> pure ([], found))
    expected (filter (isn't _Debug) found) @? ("unexpected log data: " <> show found)

enterLogHistory :: (HasCallStack, MonadReader (MVar [LogEntry]) m, MonadIO m) => LogEntry -> m ()
enterLogHistory entry = do
  logHistory <- ask
  liftIO $ do
    modifyMVar_ logHistory (\found -> pure (entry : found))

instance LC.MonadLogger (ReaderT (MVar [LogEntry]) IO) where
  log level msg = do
    let raw :: String = cs $ LC.render LC.renderNetstr msg
        parsed
          | level == LC.Debug                               = Debug raw
          | "runWithBudget: out of budget." `isInfixOf` raw = NoBudget
          | otherwise                                       = Unknown raw
    enterLogHistory parsed

delayms :: Int -> MonadIO m => m ()
delayms = threadDelay . (* 1000)

burstActions
  :: ThreadBudgetState
  -> MVar [LogEntry]
  -> Int{- duration of eac thread (milliseconds) -}
  -> Int{- number of threads -}
  -> (MonadIO m) => m ()
burstActions tbs logHistory howlong howmany
    = liftIO . void . forkIO . void $
      mapConcurrently (\jitter -> do
                          delayms jitter  -- (2 microseconds *should* enough on normal
                                          -- hardware, but we're doing 1 milisecond just
                                          -- to be safe.)
                          runReaderT (runWithBudget tbs (delayms howlong)) logHistory)
                      [1..howmany]


----------------------------------------------------------------------
-- TOC

tests :: TestTree
tests = testGroup "thread budgets" $
  [ testCase "unit test" testThreadBudgets
  , testProperty "qc stm (sequential)" propSequential
  , testProperty "qc stm (parallel)" propParallel
  ]


----------------------------------------------------------------------
-- deterministic unit test

testThreadBudgets :: Assertion
testThreadBudgets = do
  mtr <- metrics
  tbs <- mkThreadBudgetState 5
  logHistory :: MVar [LogEntry] <- newMVar []

  watcher <- async $ runReaderT (watchThreadBudgetState mtr tbs 1) logHistory

  flip runReaderT logHistory $ do
    burstActions tbs logHistory 1000 5
    delayms 100
    expectLogHistory null

    burstActions tbs logHistory 1000 3
    delayms 100
    expectLogHistory (== [NoBudget, NoBudget, NoBudget])

    burstActions tbs logHistory 1000 3
    delayms 100
    expectLogHistory (== [NoBudget, NoBudget, NoBudget])

    delayms 800

    burstActions tbs logHistory 1000 3
    delayms 100
    expectLogHistory null

    burstActions tbs logHistory 1000 3
    delayms 100
    expectLogHistory (== [NoBudget])

  cancel watcher


----------------------------------------------------------------------
-- property-based state machine tests

type State = Reference (Opaque (ThreadBudgetState, MVar [LogEntry]))

data Command r
  = Init NumberOfThreads
  | Run (State r) NumberOfThreads MilliSeconds
  | Wait (State r) MilliSeconds
-- TODO:
--  | InitWatcher
--  | ShutdownWatcher  -- perhaps this can't be done here, but we need to use the return value
--                     -- of 'runCommands', 'runParallelCommands'.
  deriving (Show, Generic, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

newtype NumberOfThreads = NumberOfThreads Int
  deriving (Eq, Ord, Show, Generic, ToExpr)

-- | 'microseconds' determines how long one unit lasts.  there is a trade-off of fast
-- vs. robust in this whole setup.  this type is supposed to help us find a good sweet spot.
newtype MilliSeconds = MilliSeconds Int
  deriving (Eq, Ord, Show, Generic, ToExpr)

data Response r
  = InitResponse (State r)
  | VoidResponse
  deriving (Show, Generic, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

-- TODO: once this works: do we really need to keep the 'State' around in here even if it's
-- symbolic?  why?  (not sure this question makes sense, i'll just keep going.)
newtype Model r = Model (Maybe (State r, [(NumberOfThreads, MilliSeconds)]))
  deriving (Show, Generic)

instance ToExpr (Model Symbolic)
instance ToExpr (Model Concrete)


generator :: Model Symbolic -> Gen (Command Symbolic)
generator (Model Nothing) = Init <$> arbitrary
generator (Model (Just (st, _))) = oneof [Run st <$> arbitrary <*> arbitrary, Wait st <$> arbitrary]

shrinker :: Command Symbolic -> [Command Symbolic]
shrinker (Run s n m) = Wait s (MilliSeconds 0) : (Run s <$> shrink n <*> shrink m)
shrinker (Wait s n)  = Wait s <$> shrink n

instance Arbitrary NumberOfThreads where
  arbitrary = NumberOfThreads <$> choose (1, 30)
  shrink (NumberOfThreads n) = NumberOfThreads <$> shrink n

instance Arbitrary MilliSeconds where
  arbitrary = MilliSeconds <$> choose (1, 30)
  shrink (MilliSeconds n) = MilliSeconds <$> shrink n


initModel :: Model r
initModel = Model Nothing


semantics :: Command Concrete -> IO (Response Concrete)
semantics (Init (NumberOfThreads limit)) = do
  mp <- newMVar []
  st <- mkThreadBudgetState limit
  pure . InitResponse . reference . Opaque $ (st, mp)

semantics (Run
            (opaque -> (tbs :: ThreadBudgetState, mp :: MVar [LogEntry]))
            (NumberOfThreads howmany)
            (MilliSeconds howlong))
  = burstActions tbs mp howmany howlong $> VoidResponse

semantics (Wait _ (MilliSeconds howlong))
  = delayms howlong $> VoidResponse


transition :: HasCallStack => Model r -> Command r -> Response r -> Model r
transition (Model Nothing) (Init _) (InitResponse st)
  = Model (Just (st, mempty))

-- 'Run' works asynchronously: start new threads, but return without any time passing.
transition (Model (Just (st, spent))) (Run _ howmany howlong) VoidResponse
  = Model (Just (st, (howmany, howlong) : spent))

-- 'Wait' makes time pass, ie. reduces the run time of running threads, and removes the ones
-- that drop below 0.
transition (Model (Just (st, spent))) (Wait _ (MilliSeconds howlong)) VoidResponse
  = Model (Just (st, filter filterSpent $ mapSpent <$> spent))
  where
    mapSpent :: (NumberOfThreads, MilliSeconds) -> (NumberOfThreads, MilliSeconds)
    mapSpent (nthreads, MilliSeconds ms) = (nthreads, MilliSeconds $ ms - howlong)

    filterSpent :: (NumberOfThreads, MilliSeconds) -> Bool
    filterSpent (_, MilliSeconds ms) = ms <= 0

transition _ _ _ = error "bad transition."


precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition = undefined


postcondition :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition = undefined


mock :: Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock _ (Init _) = InitResponse <$> genSym
mock _ _ = pure VoidResponse


sm :: StateMachine Model Command IO Response
sm = StateMachine
  { STM.initModel     = initModel
  , STM.transition    = transition
  , STM.precondition  = precondition
  , STM.postcondition = postcondition
  , STM.invariant     = Nothing
  , STM.generator     = generator
  , STM.distribution  = Nothing
  , STM.shrinker      = shrinker
  , STM.semantics     = semantics
  , STM.mock          = mock
  }


propSequential :: Property
propSequential = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
  (hist, _model, res) <- runCommands sm cmds
  prettyCommands sm hist (checkCommandNames cmds (res === Ok))

propParallel :: Property
propParallel = forAllParallelCommands sm $ \cmds -> monadicIO $ do
  prettyParallelCommands cmds =<< runParallelCommands sm cmds





-- TODO: test watcher: we can kill runWithBudget with async exceptions, that should keep the
-- finalizer from running, no?  interesting to find out if that's true in its own right.  if
-- it's true, we can run a watcher every 1s (that's the highest frequency there is), run
-- long-running actions with runWithBudget, and kill them in a way that keeps the finalizer
-- from running.  then we can check we get the watcher to clean things up.  can we also check
-- that that's the *only* time the watcher gets something to do?

-- TODO: measure watchThreadBudgetState frequence in milliseconds, so we can run it more
-- frequently during tests.  better yet, use the time measure types we already have around
-- here somewhere.
