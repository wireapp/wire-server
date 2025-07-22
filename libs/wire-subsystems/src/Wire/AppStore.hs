{-# LANGUAGE TemplateHaskell #-}

module Wire.AppStore where

import Data.Aeson
import Data.Id
import Polysemy

data AppStore m a where
  CreateApp :: UserId -> TeamId -> Object -> AppStore m ()

makeSem ''AppStore
