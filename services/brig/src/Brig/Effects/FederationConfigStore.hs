{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.FederationConfigStore where

import Data.Domain
import Polysemy

data FederationConfigStore m a where
  GetFederationConfig :: Domain -> FederationConfigStore m ()

makeSem ''FederationConfigStore
