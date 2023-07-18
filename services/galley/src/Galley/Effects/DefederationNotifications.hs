{-# LANGUAGE TemplateHaskell #-}

module Galley.Effects.DefederationNotifications 
  ( DefederationNotifications (..),
    sendDefederationNotifications
  ) where

import Polysemy
import Data.Domain (Domain)

data DefederationNotifications m a where
  SendDefederationNotifications :: Domain -> DefederationNotifications m ()

makeSem ''DefederationNotifications