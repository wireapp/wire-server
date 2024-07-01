module Wire.EmailSending.Interpreter where

import Amazonka (Env, runResourceT)
import Amazonka.Core.Lens.Internal qualified as AWS
import Amazonka.Data.Text as AWS
import Amazonka.SES qualified as SES
import Amazonka.SES.Lens qualified as SES
import Amazonka.Send as AWS
import Amazonka.Types qualified as AWS
import Control.Lens
import Control.Retry
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as Text
import Imports
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Mail.Mime (Mail, addressEmail, mailFrom, mailTo, renderMail')
import Polysemy
import Polysemy.Error
import Polysemy.Input
import System.Logger (Logger)
import Wire.EmailSending
import Wire.EmailSending.SMTP qualified as SMTP

emailToAWSInterpreter ::
  ( Member (Error EmailSendingAWSError) r,
    Member (Input Amazonka.Env) r,
    Member (Embed IO) r
  ) =>
  InterpreterFor EmailSending r
emailToAWSInterpreter = interpret \case
  SendMail mail -> sendMailAWSImpl mail

emailToSMTPInterpreter :: (Member (Embed IO) r) => Logger -> SMTP.SMTP -> InterpreterFor EmailSending r
emailToSMTPInterpreter logger smtp = interpret \case
  SendMail mail -> SMTP.sendMail logger smtp mail

sendMailAWSImpl ::
  ( Member (Error EmailSendingAWSError) r,
    Member (Input Amazonka.Env) r,
    Member (Embed IO) r
  ) =>
  Mail ->
  Sem r ()
sendMailAWSImpl m = do
  body <- liftIO $ BL.toStrict <$> renderMail' m
  let raw =
        SES.newSendRawEmail (SES.newRawMessage body)
          & SES.sendRawEmail_destinations ?~ fmap addressEmail (mailTo m)
          & SES.sendRawEmail_source ?~ addressEmail (mailFrom m)
  resp <- retrying retry5x (\_ -> pure . canRetry) $ const (sendCatch raw)
  void $ either check pure resp
  where
    check x = case x of
      -- To map rejected domain names by SES to 400 responses, in order
      -- not to trigger false 5xx alerts. Upfront domain name validation
      -- is only according to the syntax rules of RFC5322 but additional
      -- constraints may be applied by email servers (in this case SES).
      -- Since such additional constraints are neither standardised nor
      -- documented in the cases of SES, we can only handle the errors
      -- after the fact.
      AWS.ServiceError se
        | (se ^. AWS.serviceError_status == status400)
            && ("Invalid domain name" `Text.isPrefixOf` AWS.toText (se ^. AWS.serviceError_code)) ->
            throw SESInvalidDomain
      _ -> throw (EmailSendingAWSGeneralError x)

data EmailSendingAWSError where
  SESInvalidDomain :: EmailSendingAWSError
  EmailSendingAWSGeneralError :: (Show e, AWS.AsError e) => e -> EmailSendingAWSError

deriving instance Show EmailSendingAWSError

deriving instance Typeable EmailSendingAWSError

instance Exception EmailSendingAWSError

-- TODO: deduplicate in Brig.AWS.
sendCatch ::
  ( Member (Input Amazonka.Env) r,
    Member (Embed IO) r,
    AWS.AWSRequest req,
    Typeable req,
    Typeable (AWS.AWSResponse req)
  ) =>
  req ->
  Sem r (Either AWS.Error (AWS.AWSResponse req))
sendCatch req = do
  env <- input
  embed . AWS.trying AWS._Error . runResourceT . AWS.send env $ req

-- TODO: deduplicate in Brig.AWS.
canRetry :: Either AWS.Error a -> Bool
canRetry (Right _) = False
canRetry (Left e) = case e of
  AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> True
  AWS.ServiceError se | se ^. AWS.serviceError_code == AWS.ErrorCode "RequestThrottled" -> True
  _ -> False

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000
