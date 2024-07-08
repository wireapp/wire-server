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
