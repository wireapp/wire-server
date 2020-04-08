-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Provider.Template
  ( ProviderTemplates (..),
    ActivationEmailTemplate (..),
    ApprovalRequestEmailTemplate (..),
    ApprovalConfirmEmailTemplate (..),
    PasswordResetEmailTemplate (..),
    -- , TODO: NewServiceEmailTemplate   (..)
    loadProviderTemplates,

    -- * Re-exports
    Template,
    renderText,
    renderHtml,
  )
where

import Brig.Options
import Brig.Template
import Brig.Types
import Data.ByteString.Conversion (fromByteString)
import Data.Misc (HttpsUrl)
import Data.Text.Encoding (encodeUtf8)
import Imports

data ProviderTemplates
  = ProviderTemplates
      { activationEmail :: !ActivationEmailTemplate,
        activationEmailUpdate :: !ActivationEmailTemplate,
        approvalRequestEmail :: !ApprovalRequestEmailTemplate,
        approvalConfirmEmail :: !ApprovalConfirmEmailTemplate,
        passwordResetEmail :: !PasswordResetEmailTemplate
      }

data ActivationEmailTemplate
  = ActivationEmailTemplate
      { activationEmailUrl :: !Template,
        activationEmailSubject :: !Template,
        activationEmailBodyText :: !Template,
        activationEmailBodyHtml :: !Template,
        activationEmailSender :: !Email,
        activationEmailSenderName :: !Text
      }

data ApprovalRequestEmailTemplate
  = ApprovalRequestEmailTemplate
      { approvalRequestEmailUrl :: !Template,
        approvalRequestEmailSubject :: !Template,
        approvalRequestEmailBodyText :: !Template,
        approvalRequestEmailBodyHtml :: !Template,
        approvalRequestEmailSender :: !Email,
        approvalRequestEmailSenderName :: !Text,
        approvalRequestEmailTo :: !Email
      }

data ApprovalConfirmEmailTemplate
  = ApprovalConfirmEmailTemplate
      { approvalConfirmEmailSubject :: !Template,
        approvalConfirmEmailBodyText :: !Template,
        approvalConfirmEmailBodyHtml :: !Template,
        approvalConfirmEmailSender :: !Email,
        approvalConfirmEmailSenderName :: !Text,
        approvalConfirmEmailHomeUrl :: !HttpsUrl
      }

data PasswordResetEmailTemplate
  = PasswordResetEmailTemplate
      { passwordResetEmailUrl :: !Template,
        passwordResetEmailSubject :: !Template,
        passwordResetEmailBodyText :: !Template,
        passwordResetEmailBodyHtml :: !Template,
        passwordResetEmailSender :: !Email,
        passwordResetEmailSenderName :: !Text
      }

-- TODO
-- data NewServiceEmailTemplate = NewServiceEmailTemplate
--     { newServiceEmailSubject    :: !Template
--     , newServiceEmailBodyText   :: !Template
--     , newServiceEmailBodyHtml   :: !Template
--     , newServiceEmailSender     :: !Email
--     , newServiceEmailSenderName :: !Text
--     }

loadProviderTemplates :: Opts -> IO (Localised ProviderTemplates)
loadProviderTemplates o = readLocalesDir defLocale (templateDir gOptions) "provider" $ \fp ->
  ProviderTemplates
    <$> ( ActivationEmailTemplate activationUrl'
            <$> readTemplate fp "email/activation-subject.txt"
            <*> readTemplate fp "email/activation.txt"
            <*> readTemplate fp "email/activation.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
        )
    <*> ( ActivationEmailTemplate activationUrl'
            <$> readTemplate fp "email/update-subject.txt"
            <*> readTemplate fp "email/update.txt"
            <*> readTemplate fp "email/update.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
        )
    <*> ( ApprovalRequestEmailTemplate approvalUrl'
            <$> readTemplate fp "email/approval-request-subject.txt"
            <*> readTemplate fp "email/approval-request.txt"
            <*> readTemplate fp "email/approval-request.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
            <*> pure (approvalTo pOptions)
        )
    <*> ( ApprovalConfirmEmailTemplate
            <$> readTemplate fp "email/approval-confirm-subject.txt"
            <*> readTemplate fp "email/approval-confirm.txt"
            <*> readTemplate fp "email/approval-confirm.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
            <*> pure (fromMaybe (error "Invalid HTTPS URL") maybeUrl)
        )
    <*> ( PasswordResetEmailTemplate pwResetUrl'
            <$> readTemplate fp "email/password-reset-subject.txt"
            <*> readTemplate fp "email/password-reset.txt"
            <*> readTemplate fp "email/password-reset.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
        )
  where
    maybeUrl = fromByteString $ encodeUtf8 $ homeUrl pOptions
    gOptions = general $ emailSMS o
    pOptions = provider $ emailSMS o
    defLocale = setDefaultLocale (optSettings o)
    readTemplate = readTemplateWithDefault (templateDir gOptions) defLocale "provider"
    readText = readTextWithDefault (templateDir gOptions) defLocale "provider"
    -- URL templates
    activationUrl' = template $ providerActivationUrl pOptions
    approvalUrl' = template $ approvalUrl pOptions
    pwResetUrl' = template $ providerPwResetUrl pOptions
