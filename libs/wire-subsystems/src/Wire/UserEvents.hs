{-# LANGUAGE TemplateHaskell #-}

module Wire.UserEvents where

import Data.Id
import Imports
import Polysemy
import Wire.API.UserEvent

data UserEvents m a where
  OnUserEvent :: UserId -> Maybe ConnId -> UserEvent -> UserEvents m ()

makeSem ''UserEvents
