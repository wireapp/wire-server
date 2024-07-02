{-# LANGUAGE TemplateHaskell #-}

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
