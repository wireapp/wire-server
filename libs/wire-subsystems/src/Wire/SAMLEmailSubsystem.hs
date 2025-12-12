{-# LANGUAGE TemplateHaskell #-}

module Wire.SAMLEmailSubsystem where

import Polysemy
import Wire.API.User.IdentityProvider (IdP)

data SAMLEmailSubsystem m a where
  SendSAMLIdPCreated :: IdP -> SAMLEmailSubsystem m ()

makeSem ''SAMLEmailSubsystem
