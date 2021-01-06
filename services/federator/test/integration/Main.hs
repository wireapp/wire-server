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

-- import Imports

-- import Test.Tasty

-- main :: IO ()
-- main =
--   defaultMain $
--     testGroup
--       "Tests"
--       [ Test.API.tests
--       ]

import Control.Lens ((^.))
import Data.String.Conversions
import Data.Text (pack)
import Imports
import System.Environment (withArgs)
import System.Random (randomRIO)
import qualified Test.Federator.API
import Test.Federator.Util
import Test.Hspec

main :: IO ()
main = do
  (wireArgs, hspecArgs) <- partitionArgs <$> getArgs
  env <- withArgs wireArgs mkEnvFromOptions
  withArgs hspecArgs . hspec $ do
    beforeAll (pure env) . afterAll destroyEnv $ mkspec

partitionArgs :: [String] -> ([String], [String])
partitionArgs = go [] []
  where
    go wireArgs hspecArgs ("-s" : x : xs) = go (wireArgs <> ["-s", x]) hspecArgs xs
    go wireArgs hspecArgs ("-i" : x : xs) = go (wireArgs <> ["-i", x]) hspecArgs xs
    go wireArgs hspecArgs (x : xs) = go wireArgs (hspecArgs <> [x]) xs
    go wireArgs hspecArgs [] = (wireArgs, hspecArgs)

type TestEnv = String

mkspec :: SpecWith TestEnv
mkspec = do
  -- describe "Logging" Test.LoggingSpec.spec
  -- describe "Metrics" Test.MetricsSpec.spec
  -- describe "Federator.API" Test.Federator.APISpec.spec
  undefined
