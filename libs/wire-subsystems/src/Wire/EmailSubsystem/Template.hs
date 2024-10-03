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

module Wire.EmailSubsystem.Template
  ( module Wire.EmailSubsystem.Template,

    -- * Re-exports
    Template,
  )
where

import Data.Map qualified as Map
import Data.Text.Lazy qualified as Lazy
import Data.Text.Template
import HTMLEntities.Text qualified as HTML
import Imports
import Wire.API.Locale
import Wire.API.User

-- | Lookup a localised item from a 'Localised' structure.
forLocale ::
  -- | 'Just' the preferred locale or 'Nothing' for
  -- the default locale.
  Maybe Locale ->
  -- | The 'Localised' structure.
  Localised a ->
  -- | Pair of the effectively chosen locale and the
  -- associated value.
  (Locale, a)
forLocale pref t = case pref of
  Just l -> fromMaybe (locDefault t) (select l)
  Nothing -> locDefault t
  where
    select l =
      let l' = l {lCountry = Nothing}
          loc = Map.lookup l (locOther t)
          lan = Map.lookup l' (locOther t)
       in (l,) <$> loc <|> (l',) <$> lan

-- | See 'genTemplateBranding'.
type TemplateBranding = Text -> Text

-- | Localised templates.
data Localised a = Localised
  { locDefault :: (Locale, a),
    locOther :: (Map Locale a)
  }

-- | Uses a replace and a branding function, to replaces all placeholders from the
-- given template to produce a Text. To be used on plain text templates
renderTextWithBranding :: Template -> (Text -> Text) -> TemplateBranding -> Lazy.Text
renderTextWithBranding tpl replace branding = render tpl (replace . branding)

-- | Uses a replace and a branding function to replace all placeholders from the
-- given template to produce a Text. To be used on HTML templates
renderHtmlWithBranding :: Template -> (Text -> Text) -> TemplateBranding -> Lazy.Text
renderHtmlWithBranding tpl replace branding = render tpl (HTML.text . replace . branding)

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
    upgradePersonalToTeamEmail :: UpgradePersonalToTeamEmailTemplate,
    finishedPersonalToTeamEmail :: FinishedPersonalToTeamEmailTemplate,
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

data UpgradePersonalToTeamEmailTemplate = UpgradePersonalToTeamEmailTemplate
  { upgradePersonalToTeamEmailSubject :: Template,
    upgradePersonalToTeamEmailBodyText :: Template,
    upgradePersonalToTeamEmailBodyHtml :: Template,
    upgradePersonalToTeamEmailSender :: EmailAddress,
    upgradePersonalToTeamEmailSenderName :: Text
  }

data FinishedPersonalToTeamEmailTemplate = FinishedPersonalToTeamEmailTemplate
  { finishedPersonalToTeamEmailSubject :: Template,
    finishedPersonalToTeamEmailBodyText :: Template,
    finishedPersonalToTeamEmailBodyHtml :: Template,
    finishedPersonalToTeamEmailSender :: EmailAddress,
    finishedPersonalToTeamEmailSenderName :: Text
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

data InvitationEmailTemplate = InvitationEmailTemplate
  { invitationEmailUrl :: !Template,
    invitationEmailSubject :: !Template,
    invitationEmailBodyText :: !Template,
    invitationEmailBodyHtml :: !Template,
    invitationEmailSender :: !EmailAddress,
    invitationEmailSenderName :: !Text
  }

data CreatorWelcomeEmailTemplate = CreatorWelcomeEmailTemplate
  { creatorWelcomeEmailUrl :: !Text,
    creatorWelcomeEmailSubject :: !Template,
    creatorWelcomeEmailBodyText :: !Template,
    creatorWelcomeEmailBodyHtml :: !Template,
    creatorWelcomeEmailSender :: !EmailAddress,
    creatorWelcomeEmailSenderName :: !Text
  }

data MemberWelcomeEmailTemplate = MemberWelcomeEmailTemplate
  { memberWelcomeEmailUrl :: !Text,
    memberWelcomeEmailSubject :: !Template,
    memberWelcomeEmailBodyText :: !Template,
    memberWelcomeEmailBodyHtml :: !Template,
    memberWelcomeEmailSender :: !EmailAddress,
    memberWelcomeEmailSenderName :: !Text
  }

data TeamTemplates = TeamTemplates
  { invitationEmail :: !InvitationEmailTemplate,
    existingUserInvitationEmail :: !InvitationEmailTemplate,
    creatorWelcomeEmail :: !CreatorWelcomeEmailTemplate,
    memberWelcomeEmail :: !MemberWelcomeEmailTemplate
  }
