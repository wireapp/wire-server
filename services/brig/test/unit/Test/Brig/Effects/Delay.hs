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

module Test.Brig.Effects.Delay where

import Imports
import Polysemy
import Wire.Sem.Delay

-- | Ignores the delay time and only progresses when the 'MVar' is empty using
-- 'putMVar'. This way a test using this interpreter can know when the delay
-- event gets called by just waiting using 'takeMVar'. The test can also start
-- this interpreter with a full 'MVar' and use 'takeMVar' to control when the
-- 'Delay' action returns.
--
-- In addition, this interpreter also tracks calls to the 'Delay' action in a
-- 'TVar'.
--
-- Example:
-- > tick <- newEmptyMVar
-- > delayCallsTVar <- newTVarIO []
-- > async . runDelayWithTick tick delayCallsTVar $ do
-- >   doStuff
-- >   delay 100
-- > takeMVar tick -- This blocks until doStuff is done
-- > assertStuffDone
runDelayWithTick :: (Member (Embed IO) r) => MVar () -> TVar [Int] -> Sem (Delay ': r) a -> Sem r a
runDelayWithTick tick calls = interpret $ \case
  Delay i -> do
    atomically $ modifyTVar calls (<> [i])
    putMVar tick ()
