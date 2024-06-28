module Wire.EmailSending.Interpreter where

import Imports
import Polysemy
import System.Logger (Logger)
import Wire.API.AWS qualified as AWS
import Wire.EmailSending
import Wire.EmailSending.SMTP qualified as SMTP

emailToAWSInterpreter :: (Member (Embed IO) r) => AWS.Env -> InterpreterFor EmailSending r
emailToAWSInterpreter e = interpret \case
  SendMail mail -> do
    AWS.execute e $ AWS.sendMail mail

emailToSMTPInterpreter :: (Member (Embed IO) r) => Logger -> SMTP.SMTP -> InterpreterFor EmailSending r
emailToSMTPInterpreter logger smtp = interpret \case
  SendMail mail -> SMTP.sendMail logger smtp mail
