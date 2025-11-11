{-# LANGUAGE TemplateHaskell #-}

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

module Wire.Sem.Delay where

import Imports
import Polysemy

data Delay m a where
  Delay :: Int -> Delay m ()

makeSem ''Delay

runDelay :: (Member (Embed IO) r) => Sem (Delay ': r) a -> Sem r a
runDelay = interpret $ \case
  Delay i -> threadDelay i

runControlledDelay :: forall r a. (Member (Embed IO) r) => MVar Int -> Sem (Delay : r) a -> Sem r a
runControlledDelay tickSource = interpret $ \case
  Delay n -> waitForTicks n
  where
    waitForTicks :: Int -> Sem r ()
    waitForTicks 0 = pure ()
    waitForTicks remaining0 = do
      passedTicks <- takeMVar tickSource
      let remaining = remaining0 - passedTicks
      if remaining <= 0
        then pure ()
        else waitForTicks remaining

runDelayInstantly :: Sem (Delay : r) a -> Sem r a
runDelayInstantly = interpret $ \case
  Delay _ -> pure ()
