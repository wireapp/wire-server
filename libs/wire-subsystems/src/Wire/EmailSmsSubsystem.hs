{-# LANGUAGE TemplateHaskell #-}

module Wire.EmailSmsSubsystem where

import Data.Code qualified as Code
import Imports
import Polysemy
import Wire.API.Locale
import Wire.API.User
import Wire.API.User.Activation (ActivationCode, ActivationKey)
import Wire.API.User.Client (Client (..))

data EmailSmsSubsystem m a where
  SendPasswordResetMail :: Email -> PasswordResetPair -> Maybe Locale -> EmailSmsSubsystem m ()
  SendPasswordResetSms :: Phone -> PasswordResetPair -> Maybe Locale -> EmailSmsSubsystem m ()
  SendVerificationMail :: Email -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSmsSubsystem m ()
  SendTeamDeletionVerificationMail :: Email -> Code.Value -> Maybe Locale -> EmailSmsSubsystem m ()
  SendCreateScimTokenVerificationMail :: Email -> Code.Value -> Maybe Locale -> EmailSmsSubsystem m ()
  SendLoginVerificationMail :: Email -> Code.Value -> Maybe Locale -> EmailSmsSubsystem m ()
  SendActivationMail :: Email -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSmsSubsystem m ()
  SendEmailAddressUpdateMail :: Email -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSmsSubsystem m ()
  SendTeamActivationMail :: Email -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> Text -> EmailSmsSubsystem m ()
  SendNewClientEmail :: Email -> Name -> Client -> Locale -> EmailSmsSubsystem m ()
  SendDeletionEmail :: Email -> Name -> Code.Key -> Code.Value -> Locale -> EmailSmsSubsystem m ()

makeSem ''EmailSmsSubsystem
