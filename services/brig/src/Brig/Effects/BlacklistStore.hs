{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.BlacklistStore where

import Imports
import Polysemy
import Wire.UserKeyStore

data BlacklistStore m a where
  Insert :: UserKey -> BlacklistStore m ()
  Exists :: UserKey -> BlacklistStore m Bool
  Delete :: UserKey -> BlacklistStore m ()

makeSem ''BlacklistStore
