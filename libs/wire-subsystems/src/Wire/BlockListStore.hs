{-# LANGUAGE TemplateHaskell #-}

module Wire.BlockListStore where

import Imports
import Polysemy
import Wire.API.AWS.Types
import Wire.UserKeyStore

data BlockListStore m a where
  Insert :: EmailKey -> Maybe SESOriginalEvent -> BlockListStore m ()
  Exists :: EmailKey -> BlockListStore m Bool
  Delete :: EmailKey -> BlockListStore m ()

makeSem ''BlockListStore
