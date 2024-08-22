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
  SendPasswordResetMail :: EmailAddress -> PasswordResetPair -> Maybe Locale -> EmailSubsystem m ()
  SendVerificationMail :: EmailAddress -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendCreateScimTokenVerificationMail :: EmailAddress -> Code.Value -> Maybe Locale -> EmailSubsystem m ()
  SendLoginVerificationMail :: EmailAddress -> Code.Value -> Maybe Locale -> EmailSubsystem m ()
  SendActivationMail :: EmailAddress -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendEmailAddressUpdateMail :: EmailAddress -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendNewClientEmail :: EmailAddress -> Name -> Client -> Locale -> EmailSubsystem m ()
  SendAccountDeletionEmail :: EmailAddress -> Name -> Code.Key -> Code.Value -> Locale -> EmailSubsystem m ()
  SendTeamActivationMail :: EmailAddress -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> Text -> EmailSubsystem m ()
  SendTeamDeletionVerificationMail :: EmailAddress -> Code.Value -> Maybe Locale -> EmailSubsystem m ()

makeSem ''EmailSubsystem
