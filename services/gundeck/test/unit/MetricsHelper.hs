-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Helpers to test the global Prometheus metrics.
module MetricsHelper (deltas) where

import Data.String.Conversions (cs)
import Imports
import Prometheus qualified as Prom
import Test.Tasty.HUnit

-- | Value of a sample (e.g. @push_stored_targets_sum@) in the exported
-- metrics. A metric is only registered when first used, so a missing sample
-- counts as 0.
sample :: String -> IO Double
sample name = do
  exported <- cs <$> Prom.exportMetricsAsText
  case [read v | l <- lines exported, [k, v] <- [words l], k == name] of
    [] -> pure 0
    [v] -> pure v
    vs -> assertFailure ("expected exactly one sample for " <> name <> ", got " <> show (length vs))

-- | How much the samples change while running the action.
deltas :: [String] -> IO () -> IO [(String, Double)]
deltas names action = do
  beforeVals <- traverse sample names
  action
  afterVals <- traverse sample names
  pure (zip names (zipWith (-) afterVals beforeVals))
