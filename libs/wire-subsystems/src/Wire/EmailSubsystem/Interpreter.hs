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
import Data.UUID (toText)
import Imports
import Network.Mail.Mime
import Polysemy
import Polysemy.Output (Output)
import Polysemy.TinyLog (TinyLog)
import SAML2.WebSSO
import URI.ByteString (URI, serializeURIRef')
import Wire.API.Locale
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Client (Client (..))
import Wire.API.User.Password
import Wire.EmailSending (EmailSending, sendMail)
import Wire.EmailSubsystem
import Wire.EmailSubsystem.Template
import Wire.EmailSubsystem.Templates.Team
import Wire.EmailSubsystem.Templates.User

emailSubsystemInterpreter ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Localised TeamTemplates ->
  Map Text Text ->
  InterpreterFor EmailSubsystem r
emailSubsystemInterpreter userTpls teamTpls branding = interpret \case
  -- USER EMAILS
  SendPasswordResetMail email (key, code) mLocale -> sendPasswordResetMailImpl userTpls branding email key code mLocale
  SendVerificationMail email key code mLocale -> sendVerificationMailImpl userTpls branding email key code mLocale
  SendTeamDeletionVerificationMail email code mLocale -> sendTeamDeletionVerificationMailImpl userTpls branding email code mLocale
  SendCreateScimTokenVerificationMail email code mLocale -> sendCreateScimTokenVerificationMailImpl userTpls branding email code mLocale
  SendLoginVerificationMail email code mLocale -> sendLoginVerificationMailImpl userTpls branding email code mLocale
  SendActivationMail email name key code mLocale -> sendActivationMailImpl userTpls branding email name key code mLocale
  SendEmailAddressUpdateMail email name key code mLocale -> sendEmailAddressUpdateMailImpl userTpls branding email name key code mLocale
  SendTeamActivationMail email name key code mLocale teamName -> sendTeamActivationMailImpl userTpls branding email name key code mLocale teamName
  SendNewClientEmail email name client locale -> sendNewClientEmailImpl userTpls branding email name client locale
  SendAccountDeletionEmail email name key code locale -> sendAccountDeletionEmailImpl userTpls branding email name key code locale
  -- TEAM EMAILS
  SendTeamInvitationMail email tid from code loc -> sendTeamInvitationMailImpl teamTpls branding email tid from code loc
  SendTeamInvitationMailPersonalUser email tid from code loc -> sendTeamInvitationMailPersonalUserImpl teamTpls branding email tid from code loc
  SendMemberWelcomeEmail email tid teamName loc -> sendMemberWelcomeEmailImpl teamTpls branding email tid teamName loc
  SendNewTeamOwnerWelcomeEmail email tid teamName loc name -> sendNewTeamOwnerWelcomeEmailImpl teamTpls branding email tid teamName loc name
  SendSAMLIdPChanged email tid mbUid addedCerts removedCerts idPId iss requestUri mLocale ->
    sendSAMLIdPChangedImpl teamTpls branding email tid mbUid addedCerts removedCerts idPId iss requestUri mLocale

-- TODO: Move these functions down in this file.
sendSAMLIdPChangedImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised TeamTemplates ->
  Map Text Text ->
  EmailAddress ->
  TeamId ->
  Maybe UserId ->
  [IdPDescription] ->
  [IdPDescription] ->
  IdPId ->
  Issuer ->
  URI ->
  Maybe Locale ->
  Sem r ()
sendSAMLIdPChangedImpl teamTemplates branding to tid mbUid addedCerts removedCerts idPId issuer endpoint mLocale = do
  let tpl = idpConfigChangeEmail . snd $ forLocale mLocale teamTemplates
  mail <-
    logEmailRenderErrors "team deletion verification email" $
      renderIdPConfigChangeEmail to tpl branding addedCerts removedCerts tid mbUid idPId issuer endpoint
  sendMail mail

renderIdPConfigChangeEmail ::
  (Member (Output Text) r) =>
  EmailAddress ->
  IdPConfigChangeEmailTemplate ->
  Map Text Text ->
  [IdPDescription] ->
  [IdPDescription] ->
  TeamId ->
  Maybe UserId ->
  IdPId ->
  Issuer ->
  URI ->
  Sem r Mail
renderIdPConfigChangeEmail email IdPConfigChangeEmailTemplate {..} branding addedCerts removedCerts tid uid idPId issuer endpoint = do
  idpDetailsAddedText :: Text <-
    (TL.toStrict . TL.unlines)
      <$> mapM (renderTextWithBrandingSem idpConfigChangeEmailIdPDetailsAddedText . idpDetailsToMap) addedCerts
  idpDetailsAddedHtml :: Text <-
    (TL.toStrict . TL.unlines)
      <$> mapM (renderTextWithBrandingSem idpConfigChangeEmailIdPDetailsAddedHtml . idpDetailsToMap) addedCerts
  idpDetailsRemovedText :: Text <-
    (TL.toStrict . TL.unlines)
      <$> mapM (renderTextWithBrandingSem idpConfigChangeEmailIdPDetailsRemovedText . idpDetailsToMap) removedCerts
  idpDetailsRemovedHtml :: Text <-
    (TL.toStrict . TL.unlines)
      <$> mapM (renderTextWithBrandingSem idpConfigChangeEmailIdPDetailsRemovedHtml . idpDetailsToMap) removedCerts

  let replace =
        branding
          & Map.insert "teamId" ((toText . toUUID) tid)
          & Map.insert "userId" (maybe "None" (toText . toUUID) uid)
          & Map.insert "idpIssuer" ((T.decodeUtf8 . serializeURIRef' . _fromIssuer) issuer)
          & Map.insert "idpEndpoint" ((T.decodeUtf8 . serializeURIRef') endpoint)
          & Map.insert "idpId" ((toText . fromIdPId) idPId)
      replaceHtml =
        replace
          & Map.insert "idpDetails" (T.unlines [idpDetailsAddedHtml, idpDetailsRemovedHtml])
      replaceText =
        replace
          & Map.insert "idpDetails" (T.unlines [idpDetailsAddedText, idpDetailsRemovedText])

  txt <- renderTextWithBrandingSem idpConfigChangeEmailBodyText replaceText
  html <- renderHtmlWithBrandingSem idpConfigChangeEmailBodyHtml replaceHtml
  subj <- renderTextWithBrandingSem idpConfigChangeEmailSubject replace
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
    from = Address (Just idpConfigChangeEmailSenderName) (fromEmail idpConfigChangeEmailSender)
    to = Address Nothing (fromEmail email)

    idpDetailsToMap :: IdPDescription -> Map Text Text
    idpDetailsToMap d =
      empty @Text @Text
        & Map.insert "algorithm" d.idpDescriptionFingerprintAlgorithm
        & Map.insert "fingerprint" d.idpDescriptionFingerprint
        & Map.insert "subject" d.idpDescriptionSubject
        & Map.insert "issuer" d.idpDescriptionSubject

-------------------------------------------------------------------------------
-- Verification Email for
-- - Login
-- - Creation of ScimToken
-- - Team Deletion

sendTeamDeletionVerificationMailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  Code.Value ->
  Maybe Locale ->
  Sem r ()
sendTeamDeletionVerificationMailImpl userTemplates branding email code mLocale = do
  let tpl = verificationTeamDeletionEmail . snd $ forLocale mLocale userTemplates
  mail <- logEmailRenderErrors "team deletion verification email" $ renderSecondFactorVerificationEmail email code tpl branding
  sendMail mail

sendCreateScimTokenVerificationMailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  Code.Value ->
  Maybe Locale ->
  Sem r ()
sendCreateScimTokenVerificationMailImpl userTemplates branding email code mLocale = do
  let tpl = verificationScimTokenEmail . snd $ forLocale mLocale userTemplates
  mail <- logEmailRenderErrors "scim token verification email" $ renderSecondFactorVerificationEmail email code tpl branding
  sendMail mail

sendLoginVerificationMailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  Code.Value ->
  Maybe Locale ->
  Sem r ()
sendLoginVerificationMailImpl userTemplates branding email code mLocale = do
  let tpl = verificationLoginEmail . snd $ forLocale mLocale userTemplates
  mail <- logEmailRenderErrors "login verification email" $ renderSecondFactorVerificationEmail email code tpl branding
  sendMail mail

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

sendActivationMailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  Name ->
  ActivationKey ->
  ActivationCode ->
  Maybe Locale ->
  Sem r ()
sendActivationMailImpl userTemplates branding email name akey acode mLocale = do
  let tpl = activationEmail . snd $ forLocale mLocale userTemplates
  mail <- logEmailRenderErrors "activation email" $ renderActivationMail email name akey acode tpl branding
  sendMail mail

sendEmailAddressUpdateMailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  Name ->
  ActivationKey ->
  ActivationCode ->
  Maybe Locale ->
  Sem r ()
sendEmailAddressUpdateMailImpl userTemplates branding email name akey acode mLocale = do
  let tpl = activationEmailUpdate . snd $ forLocale mLocale userTemplates
  mail <- logEmailRenderErrors "email address update email" $ renderActivationMail email name akey acode tpl branding
  sendMail mail

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

sendTeamActivationMailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  Name ->
  ActivationKey ->
  ActivationCode ->
  Maybe Locale ->
  Text ->
  Sem r ()
sendTeamActivationMailImpl userTemplates branding email name akey acode mLocale teamName = do
  let tpl = teamActivationEmail . snd $ forLocale mLocale userTemplates
  mail <- logEmailRenderErrors "team activation email" $ renderTeamActivationMail email name teamName akey acode tpl branding
  sendMail mail

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

sendVerificationMailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  ActivationKey ->
  ActivationCode ->
  Maybe Locale ->
  Sem r ()
sendVerificationMailImpl userTemplates branding email akey acode mLocale = do
  let tpl = verificationEmail . snd $ forLocale mLocale userTemplates
  mail <- logEmailRenderErrors "verification email" $ renderVerificationMail email akey acode tpl branding
  sendMail mail

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

sendPasswordResetMailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  PasswordResetKey ->
  PasswordResetCode ->
  Maybe Locale ->
  Sem r ()
sendPasswordResetMailImpl userTemplates branding email pkey pcode mLocale = do
  let tpl = passwordResetEmail . snd $ forLocale mLocale userTemplates
  mail <- logEmailRenderErrors "password reset email" $ renderPwResetMail email pkey pcode tpl branding
  sendMail mail

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

sendNewClientEmailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  Name ->
  Client ->
  Locale ->
  Sem r ()
sendNewClientEmailImpl userTemplates branding email name client locale = do
  let tpl = newClientEmail . snd $ forLocale (Just locale) userTemplates
  mail <- logEmailRenderErrors "new client email" $ renderNewClientEmail email name locale client tpl branding
  sendMail mail

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

sendAccountDeletionEmailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised UserTemplates ->
  Map Text Text ->
  EmailAddress ->
  Name ->
  Code.Key ->
  Code.Value ->
  Locale ->
  Sem r ()
sendAccountDeletionEmailImpl userTemplates branding email name key code locale = do
  let tpl = deletionEmail . snd $ forLocale (Just locale) userTemplates
  mail <- logEmailRenderErrors "account deletion email" $ renderDeletionEmail email name key code tpl branding
  sendMail mail

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

sendTeamInvitationMailImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised TeamTemplates ->
  Map Text Text ->
  EmailAddress ->
  TeamId ->
  EmailAddress ->
  InvitationCode ->
  Maybe Locale ->
  Sem r Text
sendTeamInvitationMailImpl teamTemplates branding to tid from code loc = do
  let tpl = invitationEmail . snd $ forLocale loc teamTemplates
      mail = InvitationEmail to tid code from
  (renderedMail, renderedInvitationUrl) <- logEmailRenderErrors "invitation" $ renderInvitationEmail mail tpl branding
  sendMail renderedMail
  pure renderedInvitationUrl

sendTeamInvitationMailPersonalUserImpl ::
  (Member EmailSending r, Member TinyLog r) =>
  Localised TeamTemplates ->
  Map Text Text ->
  EmailAddress ->
  TeamId ->
  EmailAddress ->
  InvitationCode ->
  Maybe Locale ->
  Sem r Text
sendTeamInvitationMailPersonalUserImpl teamTemplates branding to tid from code loc = do
  let tpl = existingUserInvitationEmail . snd $ forLocale loc teamTemplates
      mail = InvitationEmail to tid code from
  (renderedMail, renderedInvitationUrl) <- logEmailRenderErrors "personal user invitation" $ renderInvitationEmail mail tpl branding
  sendMail renderedMail
  pure renderedInvitationUrl

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

sendMemberWelcomeEmailImpl :: (Member EmailSending r, Member TinyLog r) => Localised TeamTemplates -> Map Text Text -> EmailAddress -> TeamId -> Text -> Maybe Locale -> Sem r ()
sendMemberWelcomeEmailImpl teamTemplates branding to tid teamName loc = do
  let tpl = memberWelcomeEmail . snd $ forLocale loc teamTemplates
  mail <- logEmailRenderErrors "member welcome email" $ renderMemberWelcomeMail to tid teamName tpl branding
  sendMail mail

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

sendNewTeamOwnerWelcomeEmailImpl :: (Member EmailSending r, Member TinyLog r) => Localised TeamTemplates -> Map Text Text -> EmailAddress -> TeamId -> Text -> Maybe Locale -> Name -> Sem r ()
sendNewTeamOwnerWelcomeEmailImpl teamTemplates branding to tid teamName loc profileName = do
  let tpl = newTeamOwnerWelcomeEmail . snd $ forLocale loc teamTemplates
  mail <- logEmailRenderErrors "new team owner welcome email" $ renderNewTeamOwnerWelcomeEmail to tid teamName profileName tpl branding
  sendMail mail

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
