module Wire.MockInterpreters.EmailSmsSubsystem where

import Data.Code qualified as Code
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

data SentMailContent
  = PasswordResetMail PasswordResetPair
  | AccountDeletionCodeMail Name Code.Key Code.Value
  deriving (Show, Eq)

emailSmsSubsystemInterpreter :: (Member (State (Map Email [SentMail])) r) => InterpreterFor EmailSmsSubsystem r
emailSmsSubsystemInterpreter = interpret \case
  SendPasswordResetMail email keyCodePair mLocale -> storeOutgoingMail email mLocale $ PasswordResetMail keyCodePair
  SendAccountDeletionEmail email name key value locale -> storeOutgoingMail email (Just locale) $ AccountDeletionCodeMail name key value
  _ -> error "emailSmsSubsystemInterpreter: implement on demand"

storeOutgoingMail :: (Member (State (Map Email [SentMail])) r) => Email -> Maybe Locale -> SentMailContent -> Sem r ()
storeOutgoingMail email mLocale content = modify $ Map.insertWith (<>) email [SentMail mLocale content]

getEmailsSentTo :: (Member (State (Map Email [SentMail])) r) => Email -> Sem r [SentMail]
getEmailsSentTo email = gets $ Map.findWithDefault [] email
