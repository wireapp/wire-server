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

delayms :: Int -> (MonadCatch m, MonadIO m) => m ()
delayms ms = threadDelay (ms * 1000) `catch` \AsyncCancelled -> pure ()

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
type ModelState = (NumberOfThreads{- limit -}, [(NumberOfThreads, UTCTime)]{- expiry -})

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

millliSecondsToNominalDiffTime :: MilliSeconds -> NominalDiffTime
millliSecondsToNominalDiffTime = fromRational . toRational . fromMilliSeconds


data Response r
  = InitResponse (State r)
  | RunResponse
      { rspNow               :: UTCTime
      , rspConcreteRunning   :: Int
      , rspNumNoBudgetErrors :: Int
      , rspNewlyStarted      :: (NumberOfThreads, UTCTime)
      }
  | WaitResponse
      { rspNow               :: UTCTime
      , rspConcreteRunning   :: Int
      }
  deriving (Show, Generic, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)


generator :: Model Symbolic -> Gen (Command Symbolic)
generator (Model Nothing) = Init <$> arbitrary
generator (Model (Just (st, _))) = oneof [Run st <$> arbitrary <*> arbitrary, Wait st <$> arbitrary]

shrinker :: Command Symbolic -> [Command Symbolic]
shrinker (Init _)     = []
shrinker (Run st n m) = Wait st (MilliSeconds 1) : (Run st <$> shrink n <*> shrink m)
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
semantics (Init (NumberOfThreads limit))
  = do
    tbs <- mkThreadBudgetState limit
    logHistory <- newMVar []
    watcher <- mkWatcher tbs logHistory
    pure . InitResponse . reference . Opaque $ (tbs, watcher, logHistory)

semantics (Run
            (opaque -> (tbs :: ThreadBudgetState, _, logs :: LogHistory))
            howmany howlong)
  = do
    burstActions tbs logs (fromNumberOfThreads howmany) (fromMilliSeconds howlong)
    rspConcreteRunning   <- runningThreads tbs
    rspNumNoBudgetErrors <- modifyMVar logs (\found -> pure ([], length $ filter (isn't _Debug) found))
    rspNow               <- getCurrentTime
    let rspNewlyStarted   = (howmany, millliSecondsToNominalDiffTime howlong `addUTCTime` rspNow)
    pure RunResponse{..}

semantics (Wait
            (opaque -> (tbs :: ThreadBudgetState, _, _))
            (MilliSeconds howlong))
  = do
    delayms howlong
    rspConcreteRunning <- runningThreads tbs
    rspNow             <- getCurrentTime
    pure WaitResponse{..}


transition :: HasCallStack => Model r -> Command r -> Response r -> Model r
transition (Model Nothing) (Init limit) (InitResponse st)
  = Model (Just (st, (limit, [])))

-- 'Run' works asynchronously: start new threads, but return without any time passing.
transition (Model (Just (st, (limit, spent)))) (Run _ _ _) (RunResponse now _ _ rspNewlyStarted)
  = Model (Just (st, (limit, updateModelState now $ rspNewlyStarted : spent)))

-- 'Wait' makes time pass, ie. reduces the run time of running threads, and removes the ones
-- that drop below 0.
transition (Model (Just (st, (limit, spent)))) (Wait _ _) (WaitResponse now _)
  = Model (Just (st, (limit, updateModelState now spent)))

transition _ _ _ = error "bad transition."


updateModelState :: UTCTime -> [(NumberOfThreads, UTCTime)] -> [(NumberOfThreads, UTCTime)]
updateModelState now = filter filterSpent
  where
    filterSpent :: (NumberOfThreads, UTCTime) -> Bool
    filterSpent (_, timeOfDeath) = timeOfDeath < addUTCTime errorMargin now

    errorMargin :: NominalDiffTime
    errorMargin = 0.020


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

postcondition' :: (State Concrete, ModelState) -> Int -> Maybe (Int, (NumberOfThreads, UTCTime)) -> Logic
postcondition' (state, (NumberOfThreads modellimit, spent)) rspConcreteRunning mrun = result
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
      Just (rspNumNoBudgetErrors, (NumberOfThreads rspNewlyStarted, _))
        -> [ Annotate ("wrong number of over-budget calls: " <>
                       show (rspConcreteRunning, rspNewlyStarted, rspThreadLimit)) $
             max 0 rspNumNoBudgetErrors .== max 0 (rspConcreteRunning + rspNewlyStarted - rspThreadLimit)
           ]

    rspModelRunning :: Int
    rspModelRunning = sum $ (\(NumberOfThreads n, _) -> n) <$> spent

    rspThreadLimit :: Int
    rspThreadLimit = case opaque state of (tbs, _, _) -> threadLimit tbs


mock :: HasCallStack => Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock (Model Nothing) (Init _)
  = InitResponse <$> genSym
mock (Model (Just (_, (NumberOfThreads limit, spent)))) (Run _ howmany howlong)
  = do
    let rspNow               = undefined  -- doesn't appear to be needed...
        rspConcreteRunning   = sum $ (\(NumberOfThreads n, _) -> n) <$> spent
        rspNumNoBudgetErrors = rspConcreteRunning + (fromNumberOfThreads howmany) - limit
        rspNewlyStarted      = (howmany, millliSecondsToNominalDiffTime howlong `addUTCTime` rspNow)
    pure RunResponse{..}
mock (Model (Just (_, (_, spent)))) (Wait _ (MilliSeconds _))
  = do
    let rspNow             = undefined  -- doesn't appear to be needed...
        rspConcreteRunning = sum $ (\(NumberOfThreads n, _) -> n) <$> spent
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
shutdown (Model Nothing) = pure ()
shutdown (Model (Just (opaque -> (tbs, watcher, _), _))) = liftIO $ do
  cancelAllThreads tbs
  cancel watcher

-- | FUTUREWORK: in this use case of quickcheck-state-machine it may be more interesting to
-- look at fewer, but longer command sequences.
propSequential :: Property
propSequential = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
  (hist, model, res) <- runCommands sm cmds
  shutdown model
  prettyCommands sm hist (checkCommandNames cmds (res === Ok))
