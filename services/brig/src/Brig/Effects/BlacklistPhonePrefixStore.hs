{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.BlacklistPhonePrefixStore where

import Brig.Phone (Phone)
import Brig.Types.Common (ExcludedPrefix, PhonePrefix)
import Imports
import Polysemy

data BlacklistPhonePrefixStore m a where
  Delete :: PhonePrefix -> BlacklistPhonePrefixStore m ()
  ExistsAny :: Phone -> BlacklistPhonePrefixStore m Bool
  GetAll :: PhonePrefix -> BlacklistPhonePrefixStore m [ExcludedPrefix]

makeSem ''BlacklistPhonePrefixStore
