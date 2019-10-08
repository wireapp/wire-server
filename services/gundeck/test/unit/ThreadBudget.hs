{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ThreadBudget where

import Imports

import Control.Concurrent.Async
import Control.Lens
import Control.Monad.Catch (MonadCatch, catch)
import Data.Metrics.Middleware (metrics)
import Data.String.Conversions (cs)
import Data.Time
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

newtype NumberOfThreads = NumberOfThreads { fromNumberOfThreads :: Int }
  deriving (Eq, Ord, Show, Generic, ToExpr)

-- | 'microseconds' determines how long one unit lasts.  there is a trade-off of fast
-- vs. robust in this whole setup.  this type is supposed to help us find a good sweet spot.
newtype MilliSeconds = MilliSeconds { fromMilliSeconds :: Int }
  deriving (Eq, Ord, Show, Generic, ToExpr)

-- toMillisecondsCeiling 0.03      == MilliSeconds 30
-- toMillisecondsCeiling 0.003     == MilliSeconds 3
-- toMillisecondsCeiling 0.0003    == MilliSeconds 1
-- toMillisecondsCeiling 0.0000003 == MilliSeconds 1
toMillisecondsCeiling :: NominalDiffTime -> MilliSeconds
toMillisecondsCeiling = MilliSeconds . ceiling . (* 1000) . toRational

milliSecondsToNominalDiffTime :: MilliSeconds -> NominalDiffTime
milliSecondsToNominalDiffTime = fromRational . (/ 1000) . toRational . fromMilliSeconds

instance Arbitrary NumberOfThreads where
  arbitrary = NumberOfThreads <$> choose (1, 30)
  shrink (NumberOfThreads n) = NumberOfThreads <$> filter (> 0) (shrink n)

instance Arbitrary MilliSeconds where
  arbitrary = MilliSeconds <$> choose (1, 30)
  shrink (MilliSeconds n) = MilliSeconds <$> filter (> 0) (shrink n)


data LogEntry = NoBudget | Debug String | Unknown String
  deriving (Eq, Show, Generic)

makePrisms ''LogEntry

type LogHistory = MVar [LogEntry]


extractLogHistory :: (HasCallStack, MonadReader LogHistory m, MonadIO m) => m [LogEntry]
extractLogHistory = do
  logHistory <- ask
  liftIO $ modifyMVar logHistory (pure . ([],))

expectLogHistory :: (HasCallStack, MonadReader LogHistory m, MonadIO m) => ([LogEntry] -> Bool) -> m ()
expectLogHistory expected = do
  logHistory <- ask
  liftIO $ do
    found <- modifyMVar logHistory (\found -> pure ([], found))
    expected (filter (isn't _Debug) found) @? ("unexpected log data: " <> show found)

enterLogHistory :: (HasCallStack, MonadReader LogHistory m, MonadIO m) => LogEntry -> m ()
enterLogHistory entry = do
  logHistory <- ask
  liftIO $ do
    modifyMVar_ logHistory (\found -> pure (entry : found))

instance LC.MonadLogger (ReaderT LogHistory IO) where
  log level msg = do
    let raw :: String = cs $ LC.render LC.renderNetstr msg
        parsed
          | level == LC.Debug                               = Debug raw
          | "runWithBudget: out of budget." `isInfixOf` raw = NoBudget
          | otherwise                                       = Unknown raw
    enterLogHistory parsed

delayms :: MilliSeconds -> (MonadCatch m, MonadIO m) => m ()
delayms = delay' . (* 1000) . fromMilliSeconds

delayndt :: NominalDiffTime -> (MonadCatch m, MonadIO m) => m ()
delayndt = delay' . round . (* 1000) . (* 1000) . toRational

delay' :: Int -> (MonadCatch m, MonadIO m) => m ()
delay' microsecs = threadDelay microsecs `catch` \AsyncCancelled -> pure ()

burstActions
  :: HasCallStack
  => ThreadBudgetState
  -> LogHistory
  -> MilliSeconds
  -> NumberOfThreads
  -> (MonadIO m) => m ()
burstActions tbs logHistory howlong (NumberOfThreads howmany)
    = let budgeted = runWithBudget tbs (delayms howlong)
      in liftIO . replicateM_ howmany . forkIO $ runReaderT budgeted logHistory

-- | Start a watcher with given params and a frequency of 10 milliseconds, so we are more
-- likely to find weird race conditions.
mkWatcher :: ThreadBudgetState -> LogHistory -> IO (Async ())
mkWatcher tbs logHistory = do
  mtr <- metrics
  async $ runReaderT (watchThreadBudgetState mtr tbs 10) logHistory
    `catch` \AsyncCancelled -> pure ()


----------------------------------------------------------------------
-- TOC

tests :: TestTree
tests = testGroup "thread budgets" $
  [ testCase "unit test" testThreadBudgets
  , testProperty "qc stm (sequential)" propSequential
  ]


----------------------------------------------------------------------
-- deterministic unit test

testThreadBudgets :: Assertion
testThreadBudgets = do
  tbs <- mkThreadBudgetState 5
  logHistory :: LogHistory <- newMVar []
  watcher <- mkWatcher tbs logHistory

  flip runReaderT logHistory $ do
    burstActions tbs logHistory (MilliSeconds 100) (NumberOfThreads 5)
    delayms (MilliSeconds 10)
    expectLogHistory null
    liftIO $ runningThreads tbs >>= (@=? 5)

    burstActions tbs logHistory (MilliSeconds 100) (NumberOfThreads 3)
    delayms (MilliSeconds 10)
    expectLogHistory (== [NoBudget, NoBudget, NoBudget])
    liftIO $ runningThreads tbs >>= (@=? 5)

    burstActions tbs logHistory (MilliSeconds 100) (NumberOfThreads 3)
    delayms (MilliSeconds 10)
    expectLogHistory (== [NoBudget, NoBudget, NoBudget])
    liftIO $ runningThreads tbs >>= (@=? 5)

    delayms (MilliSeconds 80)

    burstActions tbs logHistory (MilliSeconds 100) (NumberOfThreads 3)
    delayms (MilliSeconds 10)
    expectLogHistory null
    liftIO $ runningThreads tbs >>= (@=? 3)

    burstActions tbs logHistory (MilliSeconds 100) (NumberOfThreads 3)
    delayms (MilliSeconds 10)
    expectLogHistory (== [NoBudget])
    liftIO $ runningThreads tbs >>= (@=? 5)

  cancel watcher


----------------------------------------------------------------------
-- property-based state machine tests

type State = Reference (Opaque (ThreadBudgetState, Async (), LogHistory))

newtype Model r = Model (Maybe (State r))
  deriving (Show, Generic)

instance ToExpr (Model Symbolic)
instance ToExpr (Model Concrete)


data Command r
  = Init NumberOfThreads
  | Run (State r) NumberOfThreads MilliSeconds
  | Wait (State r) MilliSeconds
  | Measure (State r)
  deriving (Show, Generic, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

data Response r
  = InitResponse (State r)
  | RunResponse
  | WaitResponse
  | MeasureResponse { rspConcreteRunning :: Int }
  deriving (Show, Generic, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)


generator :: HasCallStack => Model Symbolic -> Gen (Command Symbolic)
generator (Model Nothing) = Init <$> arbitrary
generator (Model (Just st)) =
  oneof [ Run st <$> arbitrary <*> arbitrary
        , Wait st <$> arbitrary
        , pure $ Measure st
        ]

shrinker :: HasCallStack => Command Symbolic -> [Command Symbolic]
shrinker (Init _)     = []
shrinker (Run st n m) = Wait st (MilliSeconds 1) : (Run st <$> shrink n <*> shrink m)
shrinker (Wait st n)  = Wait st <$> shrink n
shrinker (Measure _)  = []


initModel :: HasCallStack => Model r
initModel = Model Nothing


semantics :: HasCallStack => Command Concrete -> IO (Response Concrete)
semantics (Init (NumberOfThreads limit))
  = do
    tbs <- mkThreadBudgetState limit
    logHistory <- newMVar []
    watcher <- mkWatcher tbs logHistory
    pure . InitResponse . reference . Opaque $ (tbs, watcher, logHistory)

-- 'Run' works asynchronously: start new threads, but return without any time passing.
semantics (Run (opaque -> (tbs, _, logHistory)) howmany howlong)
  = do
    burstActions tbs logHistory howlong howmany
    pure RunResponse

-- 'Wait' makes time pass, ie. reduces the run time of running threads, and removes the ones
-- that drop below 0.
semantics (Wait _ howlong)
  = do
    delayms howlong
    pure WaitResponse

-- 'Measure' looks at the concrete state and records it into the model.
semantics (Measure (opaque -> (tbs, _, _)))
  = do
    rspConcreteRunning <- runningThreads tbs
    pure MeasureResponse{..}


transition :: HasCallStack => Model r -> Command r -> Response r -> Model r
transition (Model Nothing) (Init _) (InitResponse st) = Model (Just st)
transition (Model (Just st)) Run{} RunResponse = Model (Just st)
transition (Model (Just st)) Wait{} WaitResponse = Model (Just st)
transition (Model (Just st)) Measure{} MeasureResponse{..} = Model (Just st)
transition _ _ _ = error "impossible."


precondition :: HasCallStack => Model Symbolic -> Command Symbolic -> Logic
precondition _ _ = Top

postcondition :: HasCallStack => Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition (Model Nothing) Init{} InitResponse{} = Top
postcondition (Model (Just _)) Run{} RunResponse{} = Top
postcondition (Model (Just _)) Wait{} WaitResponse{} = Top
postcondition model@(Model (Just _)) cmd@Measure{} resp@MeasureResponse{..}
  = threadLimitExceeded
  where
    Model (Just state) = transition model cmd resp

    rspThreadLimit :: Int
    rspThreadLimit = case opaque state of (tbs, _, _) -> threadLimit tbs

    -- number of running threads is never above the limit.
    threadLimitExceeded = Annotate "thread limit exceeded" $ rspConcreteRunning .<= rspThreadLimit

    -- FUTUREWORK: check that the number of running threads matches the model exactly.  looks
    -- plausible, but when i tried to make the model rich enough to express this test i didn't
    -- manage to sort out the timing.
    -- syncNumRunning = Annotate "out of sync" $ rspConcreteRunning .== rspModelRunning

postcondition m c r = error $ "impossible: " <> show (m, c, r)


mock :: HasCallStack => Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock (Model Nothing) (Init _)
  = InitResponse <$> genSym

mock (Model (Just _)) Run{} = pure RunResponse
mock (Model (Just _)) Wait{} = pure WaitResponse
mock (Model (Just _)) Measure{} = pure MeasureResponse{..}
  where
    rspConcreteRunning = undefined
    -- FUTUREWORK: mock is cool because if we do this right, it gives us a quickcheck-
    -- validated mock component that we can use in other tests.  it appears it's not needed in
    -- the tests in this module, though, and we will need to keep track of more of the
    -- concrete state in the model if we want to fill in this 'undefined'.
    --
    -- See also: https://www.well-typed.com/blog/2019/01/qsm-in-depth/

mock badmodel badcmd = error $ "impossible: " <> show (badmodel, badcmd)


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


-- | Remove resources created by the concrete 'STM.Commands', namely watcher and budgeted
-- async threads.
shutdown :: Model Concrete -> MonadIO m => m ()
shutdown (Model Nothing) = pure ()
shutdown (Model (Just (opaque -> (tbs, watcher, _)))) = liftIO $ do
  cancelAllThreads tbs
  cancel watcher

-- | FUTUREWORK: in this use case of quickcheck-state-machine it may be more interesting to
-- look at fewer, but longer command sequences.
propSequential :: Property
propSequential = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
  (hist, model, res) <- runCommands sm cmds
  shutdown model
  prettyCommands sm hist (checkCommandNames cmds (res === Ok))
