{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.BlacklistStore where

import Brig.Types.Common
import Imports
import Polysemy

data BlacklistStore m a where
  Insert :: UserKey -> BlacklistStore m ()
  Exists :: UserKey -> BlacklistStore m Bool
  Delete :: UserKey -> BlacklistStore m ()

makeSem ''BlacklistStore
