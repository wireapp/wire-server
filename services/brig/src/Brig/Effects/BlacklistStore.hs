{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.BlacklistStore where

import Brig.Email
import Imports
import Polysemy

data BlacklistStore m a where
  Insert :: EmailKey -> BlacklistStore m ()
  Exists :: EmailKey -> BlacklistStore m Bool
  Delete :: EmailKey -> BlacklistStore m ()

makeSem ''BlacklistStore
