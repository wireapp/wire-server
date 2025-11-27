-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.Random where

import Crypto.Random
import Data.ByteString.Short (fromShort)
import Data.Id
import Imports
import Polysemy
import Polysemy.State
import System.Random hiding (Random)
import Wire.Sem.Random

randomToStatefulStdGen :: (Member (State StdGen) r) => InterpreterFor Random r
randomToStatefulStdGen = interpret $ \case
  Bytes n -> do
    fromShort <$> withStatefulGen (genShortByteString n)
  Uuid -> withStatefulGen random
  NewId -> Id <$> withStatefulGen random
  ScimTokenId -> Id <$> withStatefulGen random
  LiftRandom m -> do
    seedInt <- withStatefulGen (random @Int)
    let seed = seedFromInteger $ toInteger seedInt
        drg = drgNewSeed seed
        (x, _) = withDRG drg m
    pure x
  NDigitNumber n -> withStatefulGen $ randomR (0, 10 ^ n - 1)

runRandomPure :: InterpreterFor Random r
runRandomPure = evalState defaultGen . randomToStatefulStdGen . raiseUnder

defaultGen :: StdGen
defaultGen = mkStdGen 0xBAD

withStatefulGen :: (Member (State StdGen) r) => (StdGen -> (a, StdGen)) -> Sem r a
withStatefulGen f = do
  g <- get
  let (x, g') = f g
  put g'
  pure x
