{-# LANGUAGE TemplateHaskell #-}

module Wire.BlockListStore where

import Imports
import Polysemy
import Wire.UserKeyStore

data BlockListStore m a where
  Insert :: EmailKey -> BlockListStore m ()
  Exists :: EmailKey -> BlockListStore m Bool
  Delete :: EmailKey -> BlockListStore m ()

makeSem ''BlockListStore
