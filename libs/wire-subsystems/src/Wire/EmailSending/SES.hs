-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.Error (errorToWai)
import Wire.API.Error.Brig qualified as E
import Wire.AWS hiding (Error)
import Wire.EmailSending
import Wire.Error (HttpError (StdError))

emailViaSESInterpreter ::
  ( Member (Embed IO) r,
    Member (Error EmailSendingAWSError) r,
    Member TinyLog r
  ) =>
  Amazonka.Env ->
  InterpreterFor EmailSending r
emailViaSESInterpreter env =
  interpret $
    runInputConst env . \case
      SendMail mail -> sendMailAWSImpl mail

sendMailAWSImpl ::
  ( Member (Input Amazonka.Env) r,
    Member (Embed IO) r,
    Member (Error EmailSendingAWSError) r,
    Member TinyLog r
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
  either check (void . pure) resp
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
            && maybe False (Text.isPrefixOf "Invalid domain name" . AWS.toText) (se ^. AWS.serviceError_message) ->
            throw SESInvalidDomain
      _ -> do
        Log.err $
          Log.msg ("Error sending email through SES" :: ByteString)
            . Log.field "error" (show x)
        throw EmailSendingAWSGeneralError

data EmailSendingAWSError where
  SESInvalidDomain :: EmailSendingAWSError
  EmailSendingAWSGeneralError :: EmailSendingAWSError

deriving instance Show EmailSendingAWSError

deriving instance Typeable EmailSendingAWSError

instance Exception EmailSendingAWSError

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000

emailSendingErrorToHttpError :: EmailSendingAWSError -> HttpError
emailSendingErrorToHttpError =
  \case
    SESInvalidDomain -> StdError $ errorToWai @'E.InvalidEmail
    EmailSendingAWSGeneralError -> StdError $ Wai.mkError status500 "server-error" "Internal Server Error"
