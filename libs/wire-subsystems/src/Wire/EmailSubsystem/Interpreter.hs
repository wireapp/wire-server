{-# LANGUAGE RecordWildCards #-}

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

module Wire.EmailSubsystem.Interpreter where

import Data.Code qualified as Code
import Data.Id
import Data.Json.Util
import Data.Map as Map
import Data.Range (fromRange)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy qualified as TL
import Data.Text.Template
import Data.UUID qualified as UUID
import Data.X509.Extended
import Imports
import Network.Mail.Mime
import Polysemy
import Polysemy.Output (Output)
import SAML2.WebSSO
import URI.ByteString (serializeURIRef')
import Wire.API.BackgroundJobs.Email
import Wire.API.Locale
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Client (Client (..))
import Wire.API.User.Password
import Wire.EmailSending.Queueing (EmailQueueing, queueEmail)
import Wire.EmailSubsystem
import Wire.EmailSubsystem.Template
import Wire.EmailSubsystem.Templates.Team
import Wire.EmailSubsystem.Templates.User

-- | Interpret 'EmailSubsystem' by enqueueing the composing payload for each
-- email on the Arbiter @emails@ queue (via 'EmailQueueing'). No templates or
-- branding are touched here; the background-worker composes the actual email
-- right before sending (see "Wire.EmailSending.Composer").
emailSubsystemInterpreter :: (Member EmailQueueing r) => InterpreterFor EmailSubsystem r
emailSubsystemInterpreter = interpret \case
  -- USER EMAILS
  SendPasswordResetMail email (key, code) mLocale ->
    queueEmail $ PasswordResetEmail (MkPasswordResetEmail email key code mLocale)
  SendVerificationMail email key code mLocale ->
    queueEmail $ VerificationEmail (MkVerificationEmail email key code mLocale)
  SendTeamDeletionVerificationMail email code mLocale ->
    queueEmail $ TeamDeletionVerificationEmail (MkSecondFactorVerificationEmail email code mLocale)
  SendCreateScimTokenVerificationMail email code mLocale ->
    queueEmail $ ScimTokenVerificationEmail (MkSecondFactorVerificationEmail email code mLocale)
  SendLoginVerificationMail email code mLocale ->
    queueEmail $ LoginVerificationEmail (MkSecondFactorVerificationEmail email code mLocale)
  SendActivationMail email name key code mLocale ->
    queueEmail $ ActivationEmail (MkActivationEmail email name key code mLocale)
  SendEmailAddressUpdateMail email name key code mLocale ->
    queueEmail $ EmailAddressUpdateEmail (MkActivationEmail email name key code mLocale)
  SendTeamActivationMail email name key code mLocale teamName ->
    queueEmail $ TeamActivationEmail (MkTeamActivationEmail email name key code teamName mLocale)
  SendNewClientEmail email name client locale ->
    queueEmail $ NewClientEmail (MkNewClientEmail email name client locale)
  SendAccountDeletionEmail email name key code locale ->
    queueEmail $ AccountDeletionEmail (MkAccountDeletionEmail email name key code locale)
  -- TEAM EMAILS
  SendTeamInvitationMail email tid from code loc ->
    queueEmail $ TeamInvitationEmail (MkTeamInvitationEmail {to = email, teamId = tid, inviter = from, code = code, locale = loc})
  SendTeamInvitationMailPersonalUser email tid from code loc ->
    queueEmail $ TeamInvitationPersonalUserEmail (MkTeamInvitationEmail {to = email, teamId = tid, inviter = from, code = code, locale = loc})
  SendMemberWelcomeEmail email tid teamName loc ->
    queueEmail $ MemberWelcomeEmail (MkMemberWelcomeEmail email tid teamName loc)
  SendNewTeamOwnerWelcomeEmail email tid teamName loc name ->
    queueEmail $ NewTeamOwnerWelcomeEmail (MkNewTeamOwnerWelcomeEmail email tid teamName name loc)
  SendSAMLIdPChanged email tid mbUid addedCerts removedCerts idPId oldIssuer oldEndpoint newIssuer newEndpoint mLocale ->
    queueEmail . IdpChangedEmail $
      MkIdpChangedEmail
        { to = email,
          teamId = tid,
          userId = mbUid,
          addedCerts = toCertSummary <$> addedCerts,
          removedCerts = toCertSummary <$> removedCerts,
          idpId = UUID.toText (fromIdPId idPId),
          oldIssuer = renderIssuer <$> oldIssuer,
          oldEndpoint = renderUri <$> oldEndpoint,
          newIssuer = renderIssuer <$> newIssuer,
          newEndpoint = renderUri <$> newEndpoint,
          locale = mLocale
        }
  where
    toCertSummary d =
      CertSummary
        { algorithm = T.pack d.fingerprintAlgorithm,
          fingerprint = T.pack d.fingerprint,
          subject = T.pack d.subject,
          issuer = T.pack d.issuer
        }
    renderIssuer = T.decodeUtf8 . serializeURIRef' . _fromIssuer
    renderUri = T.decodeUtf8 . serializeURIRef'

-------------------------------------------------------------------------------
-- Verification Email for
-- - Login
-- - Creation of ScimToken
-- - Team Deletion

renderSecondFactorVerificationEmail ::
  (Member (Output Text) r) =>
  EmailAddress ->
  Code.Value ->
  SecondFactorVerificationEmailTemplate ->
  Map Text Text ->
  Sem r Mail
renderSecondFactorVerificationEmail email codeValue SecondFactorVerificationEmailTemplate {..} branding = do
  let replace =
        branding
          & Map.insert "email" (fromEmail email)
          & Map.insert "code" code
  txt <- renderTextWithBrandingSem sndFactorVerificationEmailBodyText replace
  html <- renderHtmlWithBrandingSem sndFactorVerificationEmailBodyHtml replace
  subj <- renderTextWithBrandingSem sndFactorVerificationEmailSubject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "SecondFactorVerification"),
            ("X-Zeta-Code", code)
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    from = Address (Just sndFactorVerificationEmailSenderName) (fromEmail sndFactorVerificationEmailSender)
    to = Address Nothing (fromEmail email)
    code = Ascii.toText (fromRange codeValue.asciiValue)

-------------------------------------------------------------------------------
-- Activation Email

renderActivationMail :: (Member (Output Text) r) => EmailAddress -> Name -> ActivationKey -> ActivationCode -> ActivationEmailTemplate -> Map Text Text -> Sem r Mail
renderActivationMail email name akey@(ActivationKey key) acode@(ActivationCode code) ActivationEmailTemplate {..} branding = do
  url <- renderActivationUrl activationEmailUrl akey acode branding
  let replace =
        branding
          & Map.insert "url" url
          & Map.insert "email" (fromEmail email)
          & Map.insert "name" (fromName name)
  txt <- renderTextWithBrandingSem activationEmailBodyText replace
  html <- renderHtmlWithBrandingSem activationEmailBodyHtml replace
  subj <- renderTextWithBrandingSem activationEmailSubject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        -- To make automated processing possible, the activation code is also added to
        -- headers. {#RefActivationEmailHeaders}
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "Activation"),
            ("X-Zeta-Key", Ascii.toText key),
            ("X-Zeta-Code", Ascii.toText code)
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    from = Address (Just activationEmailSenderName) (fromEmail activationEmailSender)
    to = mkMimeAddress name email

renderActivationUrl :: (Member (Output Text) r) => Template -> ActivationKey -> ActivationCode -> Map Text Text -> Sem r Text
renderActivationUrl t (ActivationKey k) (ActivationCode c) branding = do
  let replace =
        branding
          & Map.insert "key" (Ascii.toText k)
          & Map.insert "code" (Ascii.toText c)
  toStrict <$> renderTextWithBrandingSem t replace

-------------------------------------------------------------------------------
-- Team Activation Email

renderTeamActivationMail :: (Member (Output Text) r) => EmailAddress -> Name -> Text -> ActivationKey -> ActivationCode -> TeamActivationEmailTemplate -> Map Text Text -> Sem r Mail
renderTeamActivationMail email name teamName akey@(ActivationKey key) acode@(ActivationCode code) TeamActivationEmailTemplate {..} branding = do
  url <- renderActivationUrl teamActivationEmailUrl akey acode branding
  let replace =
        branding
          & Map.insert "url" url
          & Map.insert "email" (fromEmail email)
          & Map.insert "name" (fromName name)
          & Map.insert "team" teamName
  txt <- renderTextWithBrandingSem teamActivationEmailBodyText replace
  html <- renderHtmlWithBrandingSem teamActivationEmailBodyHtml replace
  subj <- renderTextWithBrandingSem teamActivationEmailSubject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "Activation"),
            ("X-Zeta-Key", Ascii.toText key),
            ("X-Zeta-Code", Ascii.toText code)
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    from = Address (Just teamActivationEmailSenderName) (fromEmail teamActivationEmailSender)
    to = mkMimeAddress name email

-------------------------------------------------------------------------------
-- Verification Email

renderVerificationMail :: (Member (Output Text) r) => EmailAddress -> ActivationKey -> ActivationCode -> VerificationEmailTemplate -> Map Text Text -> Sem r Mail
renderVerificationMail email akey acode VerificationEmailTemplate {..} branding = do
  let replace =
        branding
          & Map.insert "code" (Ascii.toText code)
          & Map.insert "email" (fromEmail email)
  txt <- renderTextWithBrandingSem verificationEmailBodyText replace
  html <- renderHtmlWithBrandingSem verificationEmailBodyHtml replace
  subj <- renderTextWithBrandingSem verificationEmailSubject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        -- To make automated processing possible, the activation code is also added to
        -- headers. {#RefActivationEmailHeaders}
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "Verification"),
            ("X-Zeta-Code", Ascii.toText code)
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    (ActivationKey _, ActivationCode code) = (akey, acode)
    from = Address (Just verificationEmailSenderName) (fromEmail verificationEmailSender)
    to = Address Nothing (fromEmail email)

-------------------------------------------------------------------------------
-- Password Reset Email

renderPwResetMail :: (Member (Output Text) r) => EmailAddress -> PasswordResetKey -> PasswordResetCode -> PasswordResetEmailTemplate -> Map Text Text -> Sem r Mail
renderPwResetMail email pkey pcode PasswordResetEmailTemplate {..} branding = do
  url <- renderPwResetUrl passwordResetEmailUrl pkey pcode
  let replace = branding & Map.insert "url" url
  txt <- renderTextWithBrandingSem passwordResetEmailBodyText replace
  html <- renderHtmlWithBrandingSem passwordResetEmailBodyHtml replace
  subj <- renderTextWithBrandingSem passwordResetEmailSubject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "PasswordReset"),
            ("X-Zeta-Key", Ascii.toText key),
            ("X-Zeta-Code", Ascii.toText code)
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    (PasswordResetKey key, PasswordResetCode code) = (pkey, pcode)
    from = Address (Just passwordResetEmailSenderName) (fromEmail passwordResetEmailSender)
    to = Address Nothing (fromEmail email)

    renderPwResetUrl t (PasswordResetKey k) (PasswordResetCode c) = do
      let replace =
            branding
              & Map.insert "key" (Ascii.toText k)
              & Map.insert "code" (Ascii.toText c)
      toStrict <$> renderTextWithBrandingSem t replace

-------------------------------------------------------------------------------
-- New Client Email

renderNewClientEmail :: (Member (Output Text) r) => EmailAddress -> Name -> Locale -> Client -> NewClientEmailTemplate -> Map Text Text -> Sem r Mail
renderNewClientEmail email name locale Client {..} NewClientEmailTemplate {..} branding = do
  let replace =
        branding
          & Map.insert "name" (fromName name)
          & Map.insert "label" (fromMaybe defRequestId clientLabel)
          & Map.insert "model" (fromMaybe defRequestId clientModel)
          & Map.insert "date" formattedDate
  txt <- renderTextWithBrandingSem newClientEmailBodyText replace
  html <- renderHtmlWithBrandingSem newClientEmailBodyHtml replace
  subj <- renderTextWithBrandingSem newClientEmailSubject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "NewDevice")
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    from = Address (Just newClientEmailSenderName) (fromEmail newClientEmailSender)
    to = mkMimeAddress name email
    formattedDate =
      formatDateTime
        "%A %e %B %Y, %H:%M - %Z"
        (timeLocale locale)
        (fromUTCTimeMillis clientTime)

-------------------------------------------------------------------------------
-- Deletion Email

renderDeletionEmail :: (Member (Output Text) r) => EmailAddress -> Name -> Code.Key -> Code.Value -> DeletionEmailTemplate -> Map Text Text -> Sem r Mail
renderDeletionEmail email name cKey cValue DeletionEmailTemplate {..} branding = do
  url <- renderDeletionUrl deletionEmailUrl cKey cValue branding
  let replace =
        branding
          & Map.insert "url" url
          & Map.insert "email" (fromEmail email)
          & Map.insert "name" (fromName name)
  txt <- renderTextWithBrandingSem deletionEmailBodyText replace
  html <- renderHtmlWithBrandingSem deletionEmailBodyHtml replace
  subj <- renderTextWithBrandingSem deletionEmailSubject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "Delete"),
            ("X-Zeta-Key", key),
            ("X-Zeta-Code", code)
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    from = Address (Just deletionEmailSenderName) (fromEmail deletionEmailSender)
    to = mkMimeAddress name email
    key = Ascii.toText (fromRange (Code.asciiKey cKey))
    code = Ascii.toText (fromRange (Code.asciiValue cValue))

renderDeletionUrl :: (Member (Output Text) r) => Template -> Code.Key -> Code.Value -> Map Text Text -> Sem r Text
renderDeletionUrl t cKey cValue branding = do
  let replace =
        branding
          & Map.insert "key" (Ascii.toText (fromRange (Code.asciiKey cKey)))
          & Map.insert "code" (Ascii.toText (fromRange (Code.asciiValue cValue)))
  toStrict <$> renderTextWithBrandingSem t replace

-------------------------------------------------------------------------------
-- Invitation Email

data InvitationEmail = InvitationEmail
  { invTo :: !EmailAddress,
    invTeamId :: !TeamId,
    invInvCode :: !InvitationCode,
    invInviter :: !EmailAddress
  }

renderInvitationEmail :: (Member (Output Text) r) => InvitationEmail -> InvitationEmailTemplate -> Map Text Text -> Sem r (Mail, Text)
renderInvitationEmail InvitationEmail {..} InvitationEmailTemplate {..} branding = do
  invitationUrl <- renderInvitationUrl invitationEmailUrl invTeamId invInvCode
  let replace = branding & Map.insert "inviter" (fromEmail invInviter) & Map.insert "url" invitationUrl
  txt <- renderTextWithBrandingSem invitationEmailBodyText replace
  html <- renderHtmlWithBrandingSem invitationEmailBodyHtml replace
  subj <- renderTextWithBrandingSem invitationEmailSubject replace
  pure
    ( (emptyMail from)
        { mailTo = [to],
          mailHeaders =
            [ ("Subject", toStrict subj),
              ("X-Zeta-Purpose", "TeamInvitation"),
              ("X-Zeta-Code", Ascii.toText code)
            ],
          mailParts = [[plainPart txt, htmlPart html]]
        },
      invitationUrl
    )
  where
    (InvitationCode code) = invInvCode
    from = Address (Just invitationEmailSenderName) (fromEmail invitationEmailSender)
    to = Address Nothing (fromEmail invTo)

renderInvitationUrl :: (Member (Output Text) r) => Template -> TeamId -> InvitationCode -> Sem r Text
renderInvitationUrl t tid (InvitationCode c) =
  toStrict <$> renderTextWithBrandingSem t (Map.fromList [("team", idToText tid), ("code", Ascii.toText c)])

-------------------------------------------------------------------------------
-- Member Welcome Email

renderMemberWelcomeMail :: (Member (Output Text) r) => EmailAddress -> TeamId -> Text -> MemberWelcomeEmailTemplate -> Map Text Text -> Sem r Mail
renderMemberWelcomeMail emailTo tid teamName MemberWelcomeEmailTemplate {..} branding = do
  let replace =
        branding
          & Map.insert "url" memberWelcomeEmailUrl
          & Map.insert "email" (fromEmail emailTo)
          & Map.insert "team_id" (idToText tid)
          & Map.insert "team_name" teamName
  txt <- renderTextWithBrandingSem memberWelcomeEmailBodyText replace
  html <- renderHtmlWithBrandingSem memberWelcomeEmailBodyHtml replace
  subj <- renderTextWithBrandingSem memberWelcomeEmailSubject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "Welcome")
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    from = Address (Just memberWelcomeEmailSenderName) (fromEmail memberWelcomeEmailSender)
    to = Address Nothing (fromEmail emailTo)

-------------------------------------------------------------------------------
-- New Team Owner Welcome Email

renderNewTeamOwnerWelcomeEmail :: (Member (Output Text) r) => EmailAddress -> TeamId -> Text -> Name -> NewTeamOwnerWelcomeEmailTemplate -> Map Text Text -> Sem r Mail
renderNewTeamOwnerWelcomeEmail emailTo tid teamName profileName NewTeamOwnerWelcomeEmailTemplate {..} branding = do
  let replace =
        branding
          & Map.insert "url" newTeamOwnerWelcomeEmailUrl
          & Map.insert "email" (fromEmail emailTo)
          & Map.insert "team_id" (idToText tid)
          & Map.insert "team_name" teamName
          & Map.insert "name" profileName.fromName
  txt <- renderTextWithBrandingSem newTeamOwnerWelcomeEmailBodyText replace
  html <- renderHtmlWithBrandingSem newTeamOwnerWelcomeEmailBodyHtml replace
  subj <- renderTextWithBrandingSem newTeamOwnerWelcomeEmailSubject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "Welcome")
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    from = Address (Just newTeamOwnerWelcomeEmailSenderName) (fromEmail newTeamOwnerWelcomeEmailSender)
    to = Address Nothing (fromEmail emailTo)

-------------------------------------------------------------------------------
-- IdP change email for team admins and owners

renderIdPConfigChangeEmail ::
  (Member (Output Text) r) =>
  IdPConfigChangeEmailTemplate ->
  Map Text Text ->
  IdpChangedEmail ->
  Sem r Mail
renderIdPConfigChangeEmail IdPConfigChangeEmailTemplate {..} branding MkIdpChangedEmail {to = email, teamId = tid, userId = uid, idpId = idpIdText, oldIssuer, oldEndpoint, newIssuer, newEndpoint, ..} = do
  idpDetailsAddedTextRendered :: Text <-
    (TL.toStrict . TL.unlines)
      <$> mapM (renderTextWithBrandingSem idpDetailsAddedText . idpDetailsToMap) addedCerts
  idpDetailsAddedHtmlRendered :: Text <-
    (TL.toStrict . TL.unlines)
      <$> mapM (renderHtmlWithBrandingSem idpDetailsAddedHtml . idpDetailsToMap) addedCerts
  idpDetailsRemovedTextRendered :: Text <-
    (TL.toStrict . TL.unlines)
      <$> mapM (renderTextWithBrandingSem idpDetailsRemovedText . idpDetailsToMap) removedCerts
  idpDetailsRemovedHtmlRendered :: Text <-
    (TL.toStrict . TL.unlines)
      <$> mapM (renderHtmlWithBrandingSem idpDetailsRemovedHtml . idpDetailsToMap) removedCerts

  let replace =
        branding
          & Map.insert "team_id" (UUID.toText (toUUID tid))
          & Map.insert "user_id" (maybe "None" (UUID.toText . toUUID) uid)
          & Map.insert "old_idp_issuer" (fromMaybe "None" oldIssuer)
          & Map.insert "old_idp_endpoint" (fromMaybe "None" oldEndpoint)
          & Map.insert "new_idp_issuer" (fromMaybe "None" newIssuer)
          & Map.insert "new_idp_endpoint" (fromMaybe "None" newEndpoint)
          & Map.insert "idp_id" idpIdText
      certificateDetailsHtml =
        (T.unlines . Imports.filter (not . T.null)) [idpDetailsAddedHtmlRendered, idpDetailsRemovedHtmlRendered]
      replaceHtml =
        replace
          & Map.insert "certificates_details" "CERTIFICATE_DETAILS"
      replaceText =
        replace
          & Map.insert "certificates_details" ((T.unlines . Imports.filter (not . T.null)) [idpDetailsAddedTextRendered, idpDetailsRemovedTextRendered])

  txt <- renderTextWithBrandingSem bodyText replaceText
  -- For HTML mails ${certificates_details} needs to be replaced in two steps:
  -- First we want to get rid of the variable. Second, we want to insert the
  -- certificates' HTML snippets directly to avoid quoting.
  html <-
    renderHtmlWithBrandingSem bodyHtml replaceHtml
      <&> TL.replace "CERTIFICATE_DETAILS" (TL.fromStrict certificateDetailsHtml)
  subj <- renderTextWithBrandingSem subject replace
  pure
    (emptyMail from)
      { mailTo = [to],
        mailHeaders =
          [ ("Subject", toStrict subj),
            ("X-Zeta-Purpose", "IdPConfigChange")
          ],
        mailParts = [[plainPart txt, htmlPart html]]
      }
  where
    from = Address (Just senderName) (fromEmail sender)
    to = Address Nothing (fromEmail email)

    idpDetailsToMap :: CertSummary -> Map Text Text
    idpDetailsToMap d =
      empty @Text @Text
        & Map.insert "algorithm" d.algorithm
        & Map.insert "fingerprint" d.fingerprint
        & Map.insert "subject" d.subject
        & Map.insert "issuer" d.issuer

-------------------------------------------------------------------------------
-- MIME Conversions

-- | Construct a MIME 'Address' from the given display 'Name' and 'Email'
-- address that does not exceed 320 bytes in length when rendered for use
-- in SMTP, which is a safe limit for most mail servers (including those of
-- Amazon SES). The display name is only included if it fits within that
-- limit, otherwise it is dropped.
mkMimeAddress :: Name -> EmailAddress -> Address
mkMimeAddress name email =
  let addr = Address (Just (fromName name)) (fromEmail email)
   in if Text.compareLength (renderAddress addr) 320 == GT
        then Address Nothing (fromEmail email)
        else addr
