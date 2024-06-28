module Wire.MockInterpreters.EmailSmsSubsystem where

import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User
import Wire.EmailSmsSubsystem

data SentMail = SentMail
  { locale :: Maybe Locale,
    content :: SentMailContent
  }
  deriving (Show, Eq)

data SentMailContent = PasswordResetMail PasswordResetPair
  deriving (Show, Eq)

emailSmsSubsystemInterpreter :: (Member (State (Map Email [SentMail])) r) => InterpreterFor EmailSmsSubsystem r
emailSmsSubsystemInterpreter = interpret \case
  SendPasswordResetMail email keyCodePair mLocale -> modify $ Map.insertWith (<>) email [SentMail mLocale $ PasswordResetMail keyCodePair]
  _ -> (error "emailSmsSubsystemInterpreter: implement on demand")

getEmailsSentTo :: (Member (State (Map Email [SentMail])) r) => Email -> Sem r [SentMail]
getEmailsSentTo email = gets $ Map.findWithDefault [] email
