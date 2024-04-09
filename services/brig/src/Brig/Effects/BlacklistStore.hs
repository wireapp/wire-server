{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.BlacklistStore where

import Brig.Data.UserKey
import Polysemy

data BlacklistStore m a where
  Insert :: UserKey -> BlacklistStore m ()
  Exists :: UserKey -> BlacklistStore m Bool
  Delete :: UserKey -> BlacklistStore m ()

makeSem ''BlacklistStore
