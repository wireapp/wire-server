{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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

type LogHistory = MVar [LogEntry]

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

delayms :: Int -> MonadIO m => m ()
delayms = threadDelay . (* 1000)

burstActions
  :: ThreadBudgetState
  -> LogHistory
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

-- | Start a watcher with given params and a frequency of 10 milliseconds, so we are more
-- likely to find weird race conditions.
mkWatcher :: ThreadBudgetState -> LogHistory -> IO (Async ())
mkWatcher tbs logHistory = do
  mtr <- metrics
  async $ runReaderT (watchThreadBudgetState mtr tbs 10) logHistory


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

type State = Reference (Opaque (ThreadBudgetState, Async (), LogHistory))
type ModelState = ([(NumberOfThreads, MilliSeconds)], NumberOfThreads)

-- TODO: once this works: do we really need to keep the 'State' around in here even if it's
-- symbolic?  why?  (not sure this question makes sense, i'll just keep going.)
newtype Model r = Model (Maybe (State r, ModelState))
  deriving (Show, Generic)

instance ToExpr (Model Symbolic)
instance ToExpr (Model Concrete)


data Command r
  = Init NumberOfThreads
  | Run (State r) NumberOfThreads MilliSeconds
  | Wait (State r) MilliSeconds
  deriving (Show, Generic, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

newtype NumberOfThreads = NumberOfThreads Int
  deriving (Eq, Ord, Show, Generic, ToExpr)

-- | 'microseconds' determines how long one unit lasts.  there is a trade-off of fast
-- vs. robust in this whole setup.  this type is supposed to help us find a good sweet spot.
newtype MilliSeconds = MilliSeconds Int
  deriving (Eq, Ord, Show, Generic, ToExpr)

data Response r
  = InitResponse (State r)
  | RunResponse
      { rspConcreteRunning   :: Int
      , rspNumNoBudgetErrors :: Int
      , rspNewlyStarted      :: Int
      }
  | WaitResponse
      { rspConcreteRunning   :: Int
      }
  deriving (Show, Generic, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)


generator :: Model Symbolic -> Gen (Command Symbolic)
generator (Model Nothing) = Init <$> arbitrary
generator (Model (Just (st, _))) = oneof [Run st <$> arbitrary <*> arbitrary, Wait st <$> arbitrary]

shrinker :: Command Symbolic -> [Command Symbolic]
shrinker (Init _)     = []
shrinker (Run st n m) = Wait st (MilliSeconds 0) : (Run st <$> shrink n <*> shrink m)
shrinker (Wait st n)  = Wait st <$> shrink n

instance Arbitrary NumberOfThreads where
  arbitrary = NumberOfThreads <$> choose (1, 30)
  shrink (NumberOfThreads n) = NumberOfThreads <$> filter (> 0) (shrink n)

instance Arbitrary MilliSeconds where
  arbitrary = MilliSeconds <$> choose (1, 30)
  shrink (MilliSeconds n) = MilliSeconds <$> filter (> 0) (shrink n)


initModel :: Model r
initModel = Model Nothing


semantics :: Command Concrete -> IO (Response Concrete)
semantics (Init (NumberOfThreads limit)) = do
  tbs <- mkThreadBudgetState limit
  logHistory <- newMVar []
  watcher <- mkWatcher tbs logHistory
  pure . InitResponse . reference . Opaque $ (tbs, watcher, logHistory)

semantics (Run
            (opaque -> (tbs :: ThreadBudgetState, _, logs :: LogHistory))
            (NumberOfThreads howmany)
            (MilliSeconds howlong))
  = do
    burstActions tbs logs howmany howlong
    rspConcreteRunning   <- length <$> runningThreads tbs
    rspNumNoBudgetErrors <- modifyMVar logs (\found -> pure ([], length $ filter (isn't _Debug) found))
    let rspNewlyStarted  = howmany
    pure RunResponse{..}

semantics (Wait
            (opaque -> (tbs :: ThreadBudgetState, _, _))
            (MilliSeconds howlong))
  = do
    delayms howlong
    rspConcreteRunning <- length <$> runningThreads tbs
    pure WaitResponse{..}


transition :: HasCallStack => Model r -> Command r -> Response r -> Model r
transition (Model Nothing) (Init limit) (InitResponse st)
  = Model (Just (st, (mempty, limit)))

-- 'Run' works asynchronously: start new threads, but return without any time passing.
transition (Model (Just (st, (modelstate, limit)))) (Run _ howmany howlong) _
  = Model (Just (st, ((howmany, howlong) : modelstate, limit)))

-- 'Wait' makes time pass, ie. reduces the run time of running threads, and removes the ones
-- that drop below 0.
transition (Model (Just (st, (modelstate, limit)))) (Wait _ (MilliSeconds howlong)) _
  = Model (Just (st, (filter filterModelstate $ mapModelstate <$> modelstate, limit)))
  where
    mapModelstate :: (NumberOfThreads, MilliSeconds) -> (NumberOfThreads, MilliSeconds)
    mapModelstate (nthreads, MilliSeconds ms) = (nthreads, MilliSeconds $ ms - howlong)

    filterModelstate :: (NumberOfThreads, MilliSeconds) -> Bool
    filterModelstate (_, MilliSeconds ms) = ms <= 0

transition _ _ _ = error "bad transition."


precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition _ _ = Top

postcondition :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition (Model Nothing) (Init _) _
  = Top

postcondition (Model (Just model))  (Run _ _ _) RunResponse{..}
  = postcondition' model rspConcreteRunning (Just (rspNumNoBudgetErrors, rspNewlyStarted))

postcondition (Model (Just model)) (Wait _ _) WaitResponse{..}
  = postcondition' model rspConcreteRunning Nothing

postcondition m c r = error $ "postcondition: " <> show (m, c, r)

postcondition' :: (State Concrete, ModelState) -> Int -> Maybe (Int, Int) -> Logic
postcondition' (state, (modelstate, NumberOfThreads modellimit)) rspConcreteRunning mrun = result
  where
    result :: Logic
    result = foldl' (.&&) Top (runAndWait <> runOnly)

    runAndWait :: [Logic]
    runAndWait
      = [ Annotate "out of sync"           $ rspModelRunning    .== rspConcreteRunning
        , Annotate "thread limit exceeded" $ rspConcreteRunning .<= rspThreadLimit
        , Annotate "wrong thread limit"    $ rspThreadLimit     .== modellimit
        ]

    runOnly :: [Logic]
    runOnly = case mrun of
      Nothing
        -> []
      Just (rspNumNoBudgetErrors, rspNewlyStarted)
        -> [ Annotate ("wrong number of over-budget calls: " <>
                       show (rspConcreteRunning, rspNewlyStarted, rspThreadLimit)) $
             max 0 rspNumNoBudgetErrors .== max 0 (rspConcreteRunning + rspNewlyStarted - rspThreadLimit)
           ]

    rspModelRunning :: Int
    rspModelRunning = sum $ (\(NumberOfThreads n, _) -> n) <$> modelstate

    rspThreadLimit :: Int
    rspThreadLimit = case opaque state of (tbs, _, _) -> threadLimit tbs


mock :: HasCallStack => Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock (Model Nothing) (Init _)
  = InitResponse <$> genSym
mock (Model (Just (_, (modelstate, NumberOfThreads limit)))) (Run _ (NumberOfThreads rspNewlyStarted) _)
  = do
    let rspConcreteRunning   = sum $ (\(NumberOfThreads n, _) -> n) <$> modelstate
        rspNumNoBudgetErrors = rspConcreteRunning + rspNewlyStarted - limit
    pure RunResponse{..}
mock (Model (Just (_, (modelstate, _)))) (Wait _ (MilliSeconds _))
  = do
    let rspConcreteRunning = sum $ (\(NumberOfThreads n, _) -> n) <$> modelstate
    pure WaitResponse{..}
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
shutdown (Model Nothing) = pure ()  -- unlikely though this seems...
shutdown (Model (Just (opaque -> (tbs, watcher, _), _))) = liftIO $ do
  gcThreadBudgetState tbs
  cancel watcher

-- | FUTUREWORK: in this use case of quickcheck-state-machine it may be more interesting to
-- look at fewer, but longer command sequences.
propSequential :: Property
propSequential = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
  (hist, model, res) <- runCommands sm cmds
  shutdown model
  prettyCommands sm hist (checkCommandNames cmds (res === Ok))
