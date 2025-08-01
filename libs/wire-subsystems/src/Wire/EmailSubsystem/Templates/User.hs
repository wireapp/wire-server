{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.EmailSubsystem.Templates.User where

import Data.Text.Template
import Imports
import Wire.API.User


data UserTemplates = UserTemplates
  { activationSms :: ActivationSmsTemplate,
    activationCall :: ActivationCallTemplate,
    verificationEmail :: VerificationEmailTemplate,
    activationEmail :: ActivationEmailTemplate,
    activationEmailUpdate :: ActivationEmailTemplate,
    teamActivationEmail :: TeamActivationEmailTemplate,
    passwordResetSms :: PasswordResetSmsTemplate,
    passwordResetEmail :: PasswordResetEmailTemplate,
    loginSms :: LoginSmsTemplate,
    loginCall :: LoginCallTemplate,
    deletionSms :: DeletionSmsTemplate,
    deletionEmail :: DeletionEmailTemplate,
    newClientEmail :: NewClientEmailTemplate,
    verificationLoginEmail :: SecondFactorVerificationEmailTemplate,
    verificationScimTokenEmail :: SecondFactorVerificationEmailTemplate,
    verificationTeamDeletionEmail :: SecondFactorVerificationEmailTemplate
  }

data ActivationSmsTemplate = ActivationSmsTemplate
  { activationSmslUrl :: Template,
    activationSmsText :: Template,
    activationSmsSender :: Text
  }

data ActivationCallTemplate = ActivationCallTemplate
  { activationCallText :: Template
  }

data VerificationEmailTemplate = VerificationEmailTemplate
  { verificationEmailUrl :: Template,
    verificationEmailSubject :: Template,
    verificationEmailBodyText :: Template,
    verificationEmailBodyHtml :: Template,
    verificationEmailSender :: EmailAddress,
    verificationEmailSenderName :: Text
  }

data ActivationEmailTemplate = ActivationEmailTemplate
  { activationEmailUrl :: Template,
    activationEmailSubject :: Template,
    activationEmailBodyText :: Template,
    activationEmailBodyHtml :: Template,
    activationEmailSender :: EmailAddress,
    activationEmailSenderName :: Text
  }

data TeamActivationEmailTemplate = TeamActivationEmailTemplate
  { teamActivationEmailUrl :: Template,
    teamActivationEmailSubject :: Template,
    teamActivationEmailBodyText :: Template,
    teamActivationEmailBodyHtml :: Template,
    teamActivationEmailSender :: EmailAddress,
    teamActivationEmailSenderName :: Text
  }

data DeletionEmailTemplate = DeletionEmailTemplate
  { deletionEmailUrl :: Template,
    deletionEmailSubject :: Template,
    deletionEmailBodyText :: Template,
    deletionEmailBodyHtml :: Template,
    deletionEmailSender :: EmailAddress,
    deletionEmailSenderName :: Text
  }

data PasswordResetEmailTemplate = PasswordResetEmailTemplate
  { passwordResetEmailUrl :: Template,
    passwordResetEmailSubject :: Template,
    passwordResetEmailBodyText :: Template,
    passwordResetEmailBodyHtml :: Template,
    passwordResetEmailSender :: EmailAddress,
    passwordResetEmailSenderName :: Text
  }

data PasswordResetSmsTemplate = PasswordResetSmsTemplate
  { passwordResetSmsText :: Template,
    passwordResetSmsSender :: Text
  }

data LoginSmsTemplate = LoginSmsTemplate
  { loginSmsUrl :: Template,
    loginSmsText :: Template,
    loginSmsSender :: Text
  }

data LoginCallTemplate = LoginCallTemplate
  { loginCallText :: Template
  }

data DeletionSmsTemplate = DeletionSmsTemplate
  { deletionSmsUrl :: Template,
    deletionSmsText :: Template,
    deletionSmsSender :: Text
  }

data NewClientEmailTemplate = NewClientEmailTemplate
  { newClientEmailSubject :: Template,
    newClientEmailBodyText :: Template,
    newClientEmailBodyHtml :: Template,
    newClientEmailSender :: EmailAddress,
    newClientEmailSenderName :: Text
  }

data SecondFactorVerificationEmailTemplate = SecondFactorVerificationEmailTemplate
  { sndFactorVerificationEmailSubject :: Template,
    sndFactorVerificationEmailBodyText :: Template,
    sndFactorVerificationEmailBodyHtml :: Template,
    sndFactorVerificationEmailSender :: EmailAddress,
    sndFactorVerificationEmailSenderName :: Text
  }
