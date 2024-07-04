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

emailSubsystemInterpreter :: (Member (State (Map Email [SentMail])) r) => InterpreterFor EmailSubsystem r
emailSubsystemInterpreter = interpret \case
  SendPasswordResetMail email keyCodePair mLocale -> modify $ Map.insertWith (<>) email [SentMail mLocale $ PasswordResetMail keyCodePair]
  _ -> error "emailSubsystemInterpreter: implement on demand"

getEmailsSentTo :: (Member (State (Map Email [SentMail])) r) => Email -> Sem r [SentMail]
getEmailsSentTo email = gets $ Map.findWithDefault [] email
