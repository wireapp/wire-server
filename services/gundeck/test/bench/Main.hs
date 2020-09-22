-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Main
  ( main,
  )
where

import Criterion.Main
import Data.Id (ClientId (..), ConnId (..), randomId)
import qualified Data.Text.Lazy as LT
import Data.UUID.V4 (nextRandom)
import Gundeck.Options
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types
import Gundeck.ThreadBudget.Internal
import Gundeck.Types.Push
import Imports
import Network.AWS (Region (Ireland))
import OpenSSL (withOpenSSL)
import System.Random (randomRIO)

main :: IO ()
main = withOpenSSL $ do
  prepared <- prepareBudgetState (100000)
  defaultMain
    [ bgroup
        "notice"
        [ bench "32" $ nfIO notice
        ],
      bgroup
        "ThreadBudget"
        [ bench "budgetSpent'" $ nfIO (bench_BudgetSpent' prepared),
          bench "prepare + budgetSpent'" $ nfIO (bench_BudgetSpent' =<< prepareBudgetState 100000)
        ]
    ]

-----------------------------------------------------------------------------
-- Benchmarks

notice :: IO Text
notice = do
  i <- randomId
  a <- mkAddress GCM
  let msg = NativePush i Nothing
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
  forM_ [1 .. size] $ \_ -> do
    key <- nextRandom
    weight <- randomRIO (1, 1000)
    allocate ref key weight
  return ref
