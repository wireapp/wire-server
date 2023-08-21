{-# LANGUAGE TemplateHaskell #-}

module Galley.Effects.DefederationNotifications
  ( DefederationNotifications (..),
    sendDefederationNotifications,
    sendOnConnectionRemovedNotifications,
  )
where

import Data.Domain (Domain)
import Polysemy

data DefederationNotifications m a where
  SendDefederationNotifications :: Domain -> DefederationNotifications m ()
  SendOnConnectionRemovedNotifications :: Domain -> Domain -> DefederationNotifications m ()

makeSem ''DefederationNotifications
