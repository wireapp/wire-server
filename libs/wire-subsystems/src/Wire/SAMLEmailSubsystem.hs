{-# LANGUAGE TemplateHaskell #-}

module Wire.SAMLEmailSubsystem where

import Polysemy
import Wire.API.Routes.Internal.Brig (IdpChangedNotification)

data SAMLEmailSubsystem m a where
  SendSAMLIdPChanged :: IdpChangedNotification -> SAMLEmailSubsystem m ()

makeSem ''SAMLEmailSubsystem
