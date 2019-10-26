module Main (main) where

import Imports
import Criterion.Main
import Data.Id (randomId, ConnId (..), ClientId (..))
import Gundeck.Types.Push
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types
import Gundeck.ThreadBudget.Internal
import Gundeck.Options
import Network.AWS (Region (Ireland))
import OpenSSL (withOpenSSL)
import System.Random (randomRIO)
import Data.UUID.V4 (nextRandom)

import qualified Data.Text.Lazy      as LT

main :: IO ()
main = withOpenSSL $ do
    prepared <- prepareBudgetState (100000)
    defaultMain [
        bgroup "notice"
            [ bench "32"   $ nfIO notice
            ]
        , bgroup "ThreadBudget"
            [ bench "budgetSpent'" $ nfIO (bench_BudgetSpent' prepared)
            , bench "prepare + budgetSpent'" $ nfIO (bench_BudgetSpent' =<< prepareBudgetState 100000)
            ]
        ]

-----------------------------------------------------------------------------
-- Benchmarks

notice :: IO Text
notice = do
    i <- randomId
    a <- mkAddress GCM
    let msg   = NativePush i HighPriority Nothing
    Right txt <- serialise msg a
    return $! LT.toStrict txt

bench_BudgetSpent' :: IORef BudgetMap -> IO ()
bench_BudgetSpent' ref = do
    budgetmap <- readIORef ref
    void $ return $ budgetSpent' budgetmap

-----------------------------------------------------------------------------
-- Utilities

mkAddress :: Transport -> IO Address
mkAddress t = do
    u <- randomId
    let app = AppName "test"
    let ept = mkEndpoint t app
    let tok = Token "test"
    let con = ConnId "conn"
    let clt = ClientId "client"
    return $! Address u ept con (pushToken t app tok clt)

mkEndpoint :: Transport -> AppName -> EndpointArn
mkEndpoint t a = mkSnsArn Ireland (Account "test") topic
  where
    topic = mkEndpointTopic (ArnEnv "test") t a (EndpointId "test")

prepareBudgetState :: Int -> IO (IORef BudgetMap)
prepareBudgetState size = do
    ref <- _threadBudgetRunning <$> mkThreadBudgetState (MaxConcurrentNativePushes Nothing Nothing)
    forM_ [1..size] $ \_ -> do
        key <- nextRandom
        weight <- randomRIO (1, 1000)
        allocate ref key weight
    return ref
