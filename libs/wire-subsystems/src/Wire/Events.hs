{-# LANGUAGE TemplateHaskell #-}

module Wire.Events where

import Data.Id
import Imports
import Polysemy
import Wire.API.UserEvent

data Events m a where
  GenerateUserEvent :: UserId -> Maybe ConnId -> UserEvent -> Events m ()
  GeneratePropertyEvent :: UserId -> ConnId -> PropertyEvent -> Events m ()

makeSem ''Events
