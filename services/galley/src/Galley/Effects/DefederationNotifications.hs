{-# LANGUAGE TemplateHaskell #-}

module Galley.Effects.DefederationNotifications
  ( DefederationNotifications (..),
    sendDefederationNotifications,
  )
where

import Data.Domain (Domain)
import Polysemy

data DefederationNotifications m a where
  SendDefederationNotifications :: Domain -> DefederationNotifications m ()

makeSem ''DefederationNotifications
