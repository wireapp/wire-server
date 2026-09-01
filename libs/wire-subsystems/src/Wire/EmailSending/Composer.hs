-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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
{-# LANGUAGE RecordWildCards #-}

-- | Worker-side email composition.
--
-- Producers enqueue the composing payload ('SendEmailRequest'); this module
-- turns a payload into a MIME 'Mail' right before sending: locale template
-- selection, placeholder rendering and MIME building all happen here.
module Wire.EmailSending.Composer
  ( EmailTemplates (..),
    loadEmailTemplates,
    composeEmail,
  )
where

import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Code qualified as Code
import Data.Default (def)
import Data.Range (fromRange)
import Data.Text (pack)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Encoding qualified as LT
import Data.Text.Template (Template)
import Imports
import Network.Mail.Mime
import Polysemy
import Polysemy.Output (Output)
import Wire.API.BackgroundJobs.Email
import Wire.API.EnterpriseLogin (DomainRegistrationResponse)
import Wire.API.Routes.Version (Version (V10))
import Wire.API.User
import Wire.EmailSubsystem.Interpreter
  ( InvitationEmail (..),
    mkMimeAddress,
    renderActivationMail,
    renderDeletionEmail,
    renderIdPConfigChangeEmail,
    renderInvitationEmail,
    renderMemberWelcomeMail,
    renderNewClientEmail,
    renderNewTeamOwnerWelcomeEmail,
    renderPwResetMail,
    renderSecondFactorVerificationEmail,
    renderTeamActivationMail,
    renderVerificationMail,
  )
import Wire.EmailSubsystem.Template
import Wire.EmailSubsystem.Templates.Provider qualified as P
import Wire.EmailSubsystem.Templates.Team
import Wire.EmailSubsystem.Templates.User qualified as U

-- | The full set of localised templates the composer needs, plus the branding
-- in both forms used by the render functions (user\/team renders take a map,
-- provider renders take a function).
data EmailTemplates = EmailTemplates
  { userTemplates :: Localised U.UserTemplates,
    teamTemplates :: Localised TeamTemplates,
    providerTemplates :: Localised P.ProviderTemplates,
    brandingFn :: TemplateBranding,
    brandingMap :: Map Text Text
  }

-- | Load all templates from the bundled templates directory. Fails fast at
-- startup if files are missing (same behaviour brig used to have).
loadEmailTemplates :: EmailTemplatesOpts -> IO EmailTemplates
loadEmailTemplates opts = do
  user <- loadUserTemplates opts.user dir locale sender
  team <- loadTeamTemplates opts.team dir locale sender
  provider <- P.loadProviderTemplates opts.provider dir locale sender
  pure
    EmailTemplates
      { userTemplates = user,
        teamTemplates = team,
        providerTemplates = provider,
        brandingFn = genTemplateBranding opts.templateBranding,
        brandingMap = genTemplateBrandingMap opts.templateBranding
      }
  where
    dir = opts.templateDir
    locale = fromMaybe def opts.defaultLocale
    sender = opts.emailSender

-- | Compose the email for a queued composing payload.
composeEmail :: (Member (Output Text) r) => EmailTemplates -> SendEmailRequest -> Sem r Mail
composeEmail tpls = \case
  VerificationEmail (MkVerificationEmail to key code locale) ->
    renderVerificationMail to key code (U.verificationEmail . snd $ forLocale locale tpls.userTemplates) tpls.brandingMap
  ActivationEmail (MkActivationEmail to name key code locale) ->
    renderActivationMail to name key code (U.activationEmail . snd $ forLocale locale tpls.userTemplates) tpls.brandingMap
  EmailAddressUpdateEmail (MkActivationEmail to name key code locale) ->
    renderActivationMail to name key code (U.activationEmailUpdate . snd $ forLocale locale tpls.userTemplates) tpls.brandingMap
  TeamActivationEmail (MkTeamActivationEmail to name key code teamName locale) ->
    renderTeamActivationMail to name teamName key code (U.teamActivationEmail . snd $ forLocale locale tpls.userTemplates) tpls.brandingMap
  PasswordResetEmail (MkPasswordResetEmail to key code locale) ->
    renderPwResetMail to key code (U.passwordResetEmail . snd $ forLocale locale tpls.userTemplates) tpls.brandingMap
  NewClientEmail (MkNewClientEmail to name client locale) ->
    renderNewClientEmail to name locale client (U.newClientEmail . snd $ forLocale (Just locale) tpls.userTemplates) tpls.brandingMap
  AccountDeletionEmail (MkAccountDeletionEmail to name key code locale) ->
    renderDeletionEmail to name key code (U.deletionEmail . snd $ forLocale (Just locale) tpls.userTemplates) tpls.brandingMap
  LoginVerificationEmail (MkSecondFactorVerificationEmail to code locale) ->
    renderSecondFactorVerificationEmail to code (U.verificationLoginEmail . snd $ forLocale locale tpls.userTemplates) tpls.brandingMap
  ScimTokenVerificationEmail (MkSecondFactorVerificationEmail to code locale) ->
    renderSecondFactorVerificationEmail to code (U.verificationScimTokenEmail . snd $ forLocale locale tpls.userTemplates) tpls.brandingMap
  TeamDeletionVerificationEmail (MkSecondFactorVerificationEmail to code locale) ->
    renderSecondFactorVerificationEmail to code (U.verificationTeamDeletionEmail . snd $ forLocale locale tpls.userTemplates) tpls.brandingMap
  TeamInvitationEmail (MkTeamInvitationEmail {to, teamId, inviter, code, locale}) -> do
    (mail, _) <-
      renderInvitationEmail
        (InvitationEmail to teamId code inviter)
        (invitationEmail . snd $ forLocale locale tpls.teamTemplates)
        tpls.brandingMap
    pure mail
  TeamInvitationPersonalUserEmail (MkTeamInvitationEmail {to, teamId, inviter, code, locale}) -> do
    (mail, _) <-
      renderInvitationEmail
        (InvitationEmail to teamId code inviter)
        (existingUserInvitationEmail . snd $ forLocale locale tpls.teamTemplates)
        tpls.brandingMap
    pure mail
  MemberWelcomeEmail (MkMemberWelcomeEmail to teamId teamName locale) ->
    renderMemberWelcomeMail to teamId teamName (memberWelcomeEmail . snd $ forLocale locale tpls.teamTemplates) tpls.brandingMap
  NewTeamOwnerWelcomeEmail (MkNewTeamOwnerWelcomeEmail to teamId teamName profileName locale) ->
    renderNewTeamOwnerWelcomeEmail to teamId teamName profileName (newTeamOwnerWelcomeEmail . snd $ forLocale locale tpls.teamTemplates) tpls.brandingMap
  IdpChangedEmail payload@MkIdpChangedEmail {userId = _userId, ..} ->
    renderIdPConfigChangeEmail
      (idpConfigChangeEmail . snd $ forLocale locale tpls.teamTemplates)
      tpls.brandingMap
      payload
  -- Provider emails always used the default locale, so no locale selection here.
  ProviderActivationEmail (MkProviderActivationEmail to name key code update) -> do
    let P.ProviderTemplates {..} = snd $ forLocale Nothing tpls.providerTemplates
        tpl = if update then activationEmailUpdate else activationEmail
    pure $ renderProviderActivationMail to name key code tpl tpls.brandingFn
  ProviderApprovalConfirmEmail (MkProviderApprovalConfirmEmail to name) ->
    pure $
      renderProviderApprovalConfirmMail
        to
        name
        (P.approvalConfirmEmail . snd $ forLocale Nothing tpls.providerTemplates)
        tpls.brandingFn
  ProviderPasswordResetEmail (MkProviderPasswordResetEmail to key code) ->
    pure $
      renderProviderPwResetMail
        to
        key
        code
        (P.passwordResetEmail . snd $ forLocale Nothing tpls.providerTemplates)
        tpls.brandingFn
  EnterpriseAuditEmail (MkEnterpriseAuditEmail {..}) ->
    pure $ mkAuditMail from to subject (mkAuditBody url before after)

--------------------------------------------------------------------------------
-- Provider renders
--------------------------------------------------------------------------------

renderProviderActivationMail :: EmailAddress -> Name -> Code.Key -> Code.Value -> P.ActivationEmailTemplate -> TemplateBranding -> Mail
renderProviderActivationMail acmTo acmName acmKey acmCode P.ActivationEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", Lazy.toStrict subj),
          ("X-Zeta-Purpose", "ProviderActivation"),
          ("X-Zeta-Key", Ascii.toText (fromRange key)),
          ("X-Zeta-Code", Ascii.toText (fromRange code))
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    (Code.Key key, Code.Value code) = (acmKey, acmCode)
    from = Address (Just activationEmailSenderName) (fromEmail activationEmailSender)
    to = mkMimeAddress acmName acmTo
    txt = renderTextWithBranding activationEmailBodyText replace branding
    html = renderHtmlWithBranding activationEmailBodyHtml replace branding
    subj = renderTextWithBranding activationEmailSubject replace branding
    replace "url" = renderProviderActivationUrl activationEmailUrl acmKey acmCode branding
    replace "email" = fromEmail acmTo
    replace "name" = fromName acmName
    replace x = x

renderProviderActivationUrl :: Template -> Code.Key -> Code.Value -> TemplateBranding -> Text
renderProviderActivationUrl t (Code.Key k) (Code.Value v) branding =
  Lazy.toStrict $ renderTextWithBranding t replace branding
  where
    replace "key" = Ascii.toText (fromRange k)
    replace "code" = Ascii.toText (fromRange v)
    replace x = x

renderProviderApprovalConfirmMail :: EmailAddress -> Name -> P.ApprovalConfirmEmailTemplate -> TemplateBranding -> Mail
renderProviderApprovalConfirmMail apcTo apcName P.ApprovalConfirmEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", Lazy.toStrict subj),
          ("X-Zeta-Purpose", "ProviderApprovalConfirm")
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    from = Address (Just approvalConfirmEmailSenderName) (fromEmail approvalConfirmEmailSender)
    to = mkMimeAddress apcName apcTo
    txt = renderTextWithBranding approvalConfirmEmailBodyText replace branding
    html = renderHtmlWithBranding approvalConfirmEmailBodyHtml replace branding
    subj = renderTextWithBranding approvalConfirmEmailSubject replace branding
    replace "homeUrl" = pack $ show approvalConfirmEmailHomeUrl
    replace "email" = fromEmail apcTo
    replace "name" = fromName apcName
    replace x = x

renderProviderPwResetMail :: EmailAddress -> Code.Key -> Code.Value -> P.PasswordResetEmailTemplate -> TemplateBranding -> Mail
renderProviderPwResetMail pwrTo pwrKey pwrCode P.PasswordResetEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", Lazy.toStrict subj),
          ("X-Zeta-Purpose", "ProviderPasswordReset"),
          ("X-Zeta-Key", Ascii.toText (fromRange key)),
          ("X-Zeta-Code", Ascii.toText (fromRange code))
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    (Code.Key key, Code.Value code) = (pwrKey, pwrCode)
    from = Address (Just passwordResetEmailSenderName) (fromEmail passwordResetEmailSender)
    to = Address Nothing (fromEmail pwrTo)
    txt = renderTextWithBranding passwordResetEmailBodyText replace branding
    html = renderHtmlWithBranding passwordResetEmailBodyHtml replace branding
    subj = renderTextWithBranding passwordResetEmailSubject replace branding
    replace "url" = renderProviderPwResetUrl passwordResetEmailUrl pwrKey pwrCode branding
    replace x = x

renderProviderPwResetUrl :: Template -> Code.Key -> Code.Value -> TemplateBranding -> Text
renderProviderPwResetUrl t (Code.Key k) (Code.Value v) branding =
  Lazy.toStrict $ renderTextWithBranding t replace branding
  where
    replace "key" = Ascii.toText (fromRange k)
    replace "code" = Ascii.toText (fromRange v)
    replace x = x

--------------------------------------------------------------------------------
-- Enterprise audit email
--------------------------------------------------------------------------------

-- | Audit email body: the called URL plus pretty-printed old\/new values.
mkAuditBody ::
  Text ->
  Maybe (DomainRegistrationResponse V10) ->
  Maybe (DomainRegistrationResponse V10) ->
  Lazy.Text
mkAuditBody url before after =
  Lazy.fromStrict url
    <> " called;\nOld value:\n"
    <> pretty before
    <> "\nNew value:\n"
    <> pretty after
  where
    pretty = maybe "null" (LT.decodeUtf8 . Aeson.encodePretty)

mkAuditMail :: EmailAddress -> EmailAddress -> Text -> Lazy.Text -> Mail
mkAuditMail from to subject bdy =
  (emptyMail (Address Nothing (fromEmail from)))
    { mailTo = [Address Nothing (fromEmail to)],
      mailHeaders =
        [ ("Subject", subject),
          ("X-Zeta-Purpose", "audit")
        ],
      mailParts = [[plainPart bdy]]
    }
