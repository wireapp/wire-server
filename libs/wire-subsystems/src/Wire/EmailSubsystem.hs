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
  -- | Context: request to create new account with this email address
  SendVerificationMail :: Email -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendCreateScimTokenVerificationMail :: Email -> Code.Value -> Maybe Locale -> EmailSubsystem m ()
  SendLoginVerificationMail :: Email -> Code.Value -> Maybe Locale -> EmailSubsystem m ()
  -- | Context: request to create new account with this email address (also)
  -- TODO(fisx): i think this is rendundant with SendVerificationMail, see docs/src/developer/reference/user/registration.md.  remove one of them?
  SendActivationMail :: Email -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  -- | Context: existing account owner changes their email
  SendEmailAddressUpdateMail :: Email -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendNewClientEmail :: Email -> Name -> Client -> Locale -> EmailSubsystem m ()
  SendAccountDeletionEmail :: Email -> Name -> Code.Key -> Code.Value -> Locale -> EmailSubsystem m ()
  -- | Context: create a team with owner
  SendTeamActivationMail :: Email -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> Text -> EmailSubsystem m ()
  SendTeamDeletionVerificationMail :: Email -> Code.Value -> Maybe Locale -> EmailSubsystem m ()

makeSem ''EmailSubsystem
