module Wire.MockInterpreters.EmailSubsystem where

import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User
import Wire.EmailSubsystem

data SentMail = SentMail
  { locale :: Maybe Locale,
    content :: SentMailContent
  }
  deriving (Show, Eq)

data SentMailContent = PasswordResetMail PasswordResetPair
  deriving (Show, Eq)

-- TODO: rename to 'emailSubsystemInterpreter'
emailSubsystemInterpreter :: (Member (State (Map EmailAddress [SentMail])) r) => InterpreterFor EmailSubsystem r
emailSubsystemInterpreter = interpret \case
  SendPasswordResetMail email keyCodePair mLocale -> modify $ Map.insertWith (<>) email [SentMail mLocale $ PasswordResetMail keyCodePair]
  _ -> error "emailSubsystemInterpreter: implement on demand"

getEmailsSentTo :: (Member (State (Map EmailAddress [SentMail])) r) => EmailAddress -> Sem r [SentMail]
getEmailsSentTo email = gets $ Map.findWithDefault [] email

noopEmailSubsystemInterpreter :: InterpreterFor EmailSubsystem r
noopEmailSubsystemInterpreter = interpret \case
  SendPasswordResetMail {} -> pure ()
  SendVerificationMail {} -> pure ()
  SendCreateScimTokenVerificationMail {} -> pure ()
  SendLoginVerificationMail {} -> pure ()
  SendActivationMail {} -> pure ()
  SendEmailAddressUpdateMail {} -> pure ()
  SendNewClientEmail {} -> pure ()
  SendAccountDeletionEmail {} -> pure ()
  SendTeamActivationMail {} -> pure ()
  SendTeamDeletionVerificationMail {} -> pure ()
  SendTeamInvitationMail {} -> pure ""
  SendTeamInvitationMailPersonalUser {} -> pure ""
