{-# LANGUAGE TemplateHaskell #-}

module Wire.ClientSubsystem where

import Data.Id
import Data.Time.Clock
import Imports
import Polysemy

data ClientSubsystem m a where
  InternalGetActivityTimestamps :: UserId -> ClientSubsystem m [Maybe UTCTime]

makeSem ''ClientSubsystem
