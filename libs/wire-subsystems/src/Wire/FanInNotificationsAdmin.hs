{-# LANGUAGE TemplateHaskell #-}

module Wire.FanInNotificationsAdmin where

import Polysemy

-- | Administrative operations on the fan-in notification tables for tests and
-- performance tooling. Never wire this into services.
data FanInNotificationsAdmin m a where
  TruncateAll :: FanInNotificationsAdmin m ()
  Ping :: FanInNotificationsAdmin m ()

makeSem ''FanInNotificationsAdmin
