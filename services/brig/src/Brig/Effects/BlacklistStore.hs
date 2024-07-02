{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.BlacklistStore where

import Brig.Data.UserKey
import Brig.Email
import Imports
import Polysemy
import Wire.UserKeyStore

data BlacklistStore m a where
  Insert :: EmailKey -> BlacklistStore m ()
  Exists :: EmailKey -> BlacklistStore m Bool
  Delete :: EmailKey -> BlacklistStore m ()

makeSem ''BlacklistStore
