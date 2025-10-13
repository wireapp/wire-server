module Wire.EmailSending.Core where

import Amazonka qualified as AWS
import Imports
import Polysemy
import System.Logger (Logger)
import Wire.EmailSending
import Wire.EmailSending.SES
import Wire.EmailSending.SMTP

data EmailSendingInterpreterConfig = EmailSendingInterpreterConfig
  { smtpEnv :: Maybe SMTP,
    awsEnv :: AWS.Env,
    appLogger :: Logger
  }

emailSendingInterpreter ::
  (Member (Embed IO) r) =>
  EmailSendingInterpreterConfig ->
  InterpreterFor EmailSending r
emailSendingInterpreter c = do
  case c.smtpEnv of
    Just smtp -> emailViaSMTPInterpreter c.appLogger smtp
    Nothing -> emailViaSESInterpreter c.awsEnv
