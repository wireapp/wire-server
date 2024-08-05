{-# LANGUAGE TemplateHaskell #-}

module Wire.EmailSubsystem where

import Data.Code qualified as Code
import Imports
import Polysemy
import Wire.API.Locale
import Wire.API.User
import Wire.API.User.Activation (ActivationCode, ActivationKey)
import Wire.API.User.Client (Client (..))

data EmailSubsystem m a where
  SendPasswordResetMail :: Email -> PasswordResetPair -> Maybe Locale -> EmailSubsystem m ()
  SendVerificationMail :: Email -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendCreateScimTokenVerificationMail :: Email -> Code.Value -> Maybe Locale -> EmailSubsystem m ()
  SendLoginVerificationMail :: Email -> Code.Value -> Maybe Locale -> EmailSubsystem m ()
  SendActivationMail :: Email -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendEmailAddressUpdateMail :: Email -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendNewClientEmail :: Email -> Name -> Client -> Locale -> EmailSubsystem m ()
  SendAccountDeletionEmail :: Email -> Name -> Code.Key -> Code.Value -> Locale -> EmailSubsystem m ()
  SendTeamActivationMail :: Email -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> Text -> EmailSubsystem m ()
  SendTeamDeletionVerificationMail :: Email -> Code.Value -> Maybe Locale -> EmailSubsystem m ()

makeSem ''EmailSubsystem
