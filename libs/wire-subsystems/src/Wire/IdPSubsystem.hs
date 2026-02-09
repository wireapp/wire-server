{-# LANGUAGE TemplateHaskell #-}

module Wire.IdPSubsystem where

import Imports
import Polysemy
import SAML2.WebSSO qualified as SAML
import Wire.API.Routes.Public (ZHostValue)
import Wire.API.User (EmailAddress)

data IdPSubsystem m a where
  GetSsoCodeByEmail :: Maybe ZHostValue -> EmailAddress -> IdPSubsystem m (Maybe SAML.IdPId)

makeSem ''IdPSubsystem

data IdPSubsystemError = InconsistentUsers
