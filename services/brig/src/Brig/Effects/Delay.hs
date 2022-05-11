{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.Delay where

import Imports
import Polysemy

data Delay m a where
  Delay :: Int -> Delay m ()

makeSem ''Delay

runDelay :: Member (Embed IO) r => Sem (Delay ': r) a -> Sem r a
runDelay = interpret $ \case
  Delay i -> threadDelay i
