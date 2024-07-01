module Wire.EmailSending.Error where

import Amazonka.Types
import Control.Monad.Catch
import Imports

data EmailSendingAWSError where
  SESInvalidDomain :: EmailSendingAWSError
  EmailSendingAWSGeneralError :: (Show e, AsError e) => e -> EmailSendingAWSError

deriving instance Show EmailSendingAWSError

deriving instance Typeable EmailSendingAWSError

instance Exception EmailSendingAWSError
