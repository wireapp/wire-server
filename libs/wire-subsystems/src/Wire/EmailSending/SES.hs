module Wire.EmailSending.SES where

import Amazonka (Env)
import Amazonka.Data.Text as AWS
import Amazonka.SES qualified as SES
import Amazonka.SES.Lens qualified as SES
import Amazonka.Types qualified as AWS
import Control.Lens
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as Text
import Imports
import Network.HTTP.Types
import Network.Mail.Mime (Mail, addressEmail, mailFrom, mailTo, renderMail')
import Polysemy
import Polysemy.Input
import Wire.AWS
import Wire.EmailSending

emailViaSESInterpreter ::
  (Member (Embed IO) r) =>
  Amazonka.Env ->
  InterpreterFor EmailSending r
emailViaSESInterpreter env =
  interpret $
    runInputConst env . \case
      SendMail mail -> sendMailAWSImpl mail

sendMailAWSImpl ::
  ( Member (Input Amazonka.Env) r,
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
  void . embed $ either check pure resp
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
            throwM SESInvalidDomain
      _ -> throwM (EmailSendingAWSGeneralError x)

data EmailSendingAWSError where
  SESInvalidDomain :: EmailSendingAWSError
  EmailSendingAWSGeneralError :: (Show e, AWS.AsError e) => e -> EmailSendingAWSError

deriving instance Show EmailSendingAWSError

deriving instance Typeable EmailSendingAWSError

instance Exception EmailSendingAWSError

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000
