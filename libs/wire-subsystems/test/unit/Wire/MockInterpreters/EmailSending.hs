module Wire.MockInterpreters.EmailSending where

import Imports
import Polysemy
import Wire.EmailSending

noopEmailSendingInterpreter :: InterpreterFor EmailSending r
noopEmailSendingInterpreter =
  interpret \case
    SendMail _ -> pure ()
