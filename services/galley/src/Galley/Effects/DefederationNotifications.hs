{-# LANGUAGE TemplateHaskell #-}

module Galley.Effects.DefederationNotifications
  ( DefederationNotifications (..),
    sendDefederationNotifications,
    sendOnConnectionRemovedNotifications,
  )
where

import Data.Domain (Domain)
import Data.Range (Range)
import Imports (Int32)
import Polysemy
import Wire.API.Team.Member (HardTruncationLimit)

data DefederationNotifications m a where
  SendDefederationNotifications :: (Range 1 HardTruncationLimit Int32) -> Domain -> DefederationNotifications m ()
  SendOnConnectionRemovedNotifications :: (Range 1 HardTruncationLimit Int32) -> Domain -> Domain -> DefederationNotifications m ()

makeSem ''DefederationNotifications
