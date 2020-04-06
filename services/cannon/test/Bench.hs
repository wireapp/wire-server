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

module Bench (Bench.benchmark) where

import qualified Cannon.Dict as D
import Control.Concurrent.Async
import Criterion
import Criterion.Main
import Data.UUID
import Data.UUID.V4
import Imports

benchmark :: IO ()
benchmark =
  defaultMain
    [ bgroup
        "Cannon.Dict"
        [ bench "slices 1" $ nfIO (slices 1 1280),
          bench "slices 10" $ nfIO (slices 10 1280),
          bench "slices 100" $ nfIO (slices 100 1280),
          bench "slices 1000" $ nfIO (slices 1000 1280)
        ]
    ]

slices :: Int -> Int -> IO ()
slices s n = do
  let nthreads = 8
  d <- D.empty s
  mapM_ wait =<< replicateM nthreads (async $ action d)
  x <- D.size d
  unless (x == nthreads) $
    error (show x)
  where
    action d = do
      uid <- toByteString <$> nextRandom
      dat <- toByteString <$> nextRandom
      replicateM_ n $
        D.insert uid dat d
      tad <- D.lookup uid d
      unless (Just dat == tad) $
        error "Ooops."
