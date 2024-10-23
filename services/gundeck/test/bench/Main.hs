-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main
  ( main,
  )
where

import Amazonka (Region (Ireland))
import Control.Lens ((^.))
import Criterion.Main
import Data.Id (ClientId (..), ConnId (..), randomId)
import Data.Text.Lazy qualified as LT
import Data.UUID.V4 (nextRandom)
import Gundeck.Options
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types
import Gundeck.ThreadBudget.Internal
import Imports
import OpenSSL (withOpenSSL)
import System.Random (randomRIO)
import Wire.API.Push.V2

main :: IO ()
main = withOpenSSL $ do
  prepared <- prepareBudgetState 100000
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
  let msg = NativePush i HighPriority Nothing
      uid = a ^. addrUser
      transp = a ^. addrTransport
  Right txt <- pure $ serialise msg uid transp
  pure $! LT.toStrict txt

bench_BudgetSpent' :: IORef BudgetMap -> IO ()
bench_BudgetSpent' ref = do
  budgetmap <- readIORef ref
  void $ pure $ budgetSpent' budgetmap

-----------------------------------------------------------------------------
-- Utilities

mkAddress :: Transport -> IO Address
mkAddress t = do
  u <- randomId
  let app = AppName "test"
  let ept = mkEndpoint t app
  let tok = Token "test"
  let con = ConnId "conn"
  let clt = ClientId 3265102391
  pure $! Address u ept con (pushToken t app tok clt)

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
  pure ref
