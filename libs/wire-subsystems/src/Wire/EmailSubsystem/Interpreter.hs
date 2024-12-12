{-# LANGUAGE RecordWildCards #-}

module Wire.EmailSubsystem.Interpreter
  ( emailSubsystemInterpreter,
    mkMimeAddress,
    renderInvitationUrl,
  )
where

import Data.Code qualified as Code
import Data.Id
import Data.Json.Util
import Data.Range (fromRange)
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Data.Text.Lazy (toStrict)
import Imports
import Network.Mail.Mime
import Polysemy
import Wire.API.Locale
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Client (Client (..))
import Wire.API.User.Password
import Wire.EmailSending (EmailSending, sendMail)
import Wire.EmailSubsystem
import Wire.EmailSubsystem.Template

emailSubsystemInterpreter :: (Member EmailSending r) => Localised UserTemplates -> Localised TeamTemplates -> TemplateBranding -> InterpreterFor EmailSubsystem r
emailSubsystemInterpreter userTpls teamTpls branding = interpret \case
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
  SendTeamInvitationMail email tid from code loc -> sendTeamInvitationMailImpl teamTpls branding email tid from code loc
  SendTeamInvitationMailPersonalUser email tid from code loc -> sendTeamInvitationMailPersonalUserImpl teamTpls branding email tid from code loc

-------------------------------------------------------------------------------
-- Verification Email for
-- - Login
-- - Creation of ScimToken
-- - Team Deletion

sendTeamDeletionVerificationMailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  Code.Value ->
  Maybe Locale ->
  Sem r ()
sendTeamDeletionVerificationMailImpl userTemplates branding email code mLocale = do
  let tpl = verificationTeamDeletionEmail . snd $ forLocale mLocale userTemplates
  sendMail $ renderSecondFactorVerificationEmail email code tpl branding

sendCreateScimTokenVerificationMailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  Code.Value ->
  Maybe Locale ->
  Sem r ()
sendCreateScimTokenVerificationMailImpl userTemplates branding email code mLocale = do
  let tpl = verificationScimTokenEmail . snd $ forLocale mLocale userTemplates
  sendMail $ renderSecondFactorVerificationEmail email code tpl branding

sendLoginVerificationMailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  Code.Value ->
  Maybe Locale ->
  Sem r ()
sendLoginVerificationMailImpl userTemplates branding email code mLocale = do
  let tpl = verificationLoginEmail . snd $ forLocale mLocale userTemplates
  sendMail $ renderSecondFactorVerificationEmail email code tpl branding

renderSecondFactorVerificationEmail ::
  EmailAddress ->
  Code.Value ->
  SecondFactorVerificationEmailTemplate ->
  TemplateBranding ->
  Mail
renderSecondFactorVerificationEmail email codeValue SecondFactorVerificationEmailTemplate {..} branding =
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
    txt = renderTextWithBranding sndFactorVerificationEmailBodyText replace branding
    html = renderHtmlWithBranding sndFactorVerificationEmailBodyHtml replace branding
    subj = renderTextWithBranding sndFactorVerificationEmailSubject replace branding
    code = Ascii.toText (fromRange codeValue.asciiValue)
    replace :: Text -> Text
    replace "email" = fromEmail email
    replace "code" = code
    replace x = x

-------------------------------------------------------------------------------
-- Activation Email

sendActivationMailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  Name ->
  ActivationKey ->
  ActivationCode ->
  Maybe Locale ->
  Sem r ()
sendActivationMailImpl userTemplates branding email name akey acode mLocale = do
  let tpl = activationEmail . snd $ forLocale mLocale userTemplates
  sendMail $ renderActivationMail email name akey acode tpl branding

sendEmailAddressUpdateMailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  Name ->
  ActivationKey ->
  ActivationCode ->
  Maybe Locale ->
  Sem r ()
sendEmailAddressUpdateMailImpl userTemplates branding email name akey acode mLocale = do
  let tpl = activationEmailUpdate . snd $ forLocale mLocale userTemplates
  sendMail $ renderActivationMail email name akey acode tpl branding

renderActivationMail :: EmailAddress -> Name -> ActivationKey -> ActivationCode -> ActivationEmailTemplate -> TemplateBranding -> Mail
renderActivationMail email name akey@(ActivationKey key) acode@(ActivationCode code) ActivationEmailTemplate {..} branding =
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
    from, to :: Address
    from = Address (Just activationEmailSenderName) (fromEmail activationEmailSender)
    to = mkMimeAddress name email

    txt, html, subj :: LText
    txt = renderTextWithBranding activationEmailBodyText replace branding
    html = renderHtmlWithBranding activationEmailBodyHtml replace branding
    subj = renderTextWithBranding activationEmailSubject replace branding

    replace :: Text -> Text
    replace "url" = renderActivationUrl activationEmailUrl akey acode branding
    replace "email" = fromEmail email
    replace "name" = fromName name
    replace x = x

renderActivationUrl :: Template -> ActivationKey -> ActivationCode -> TemplateBranding -> Text
renderActivationUrl t (ActivationKey k) (ActivationCode c) branding =
  toStrict $ renderTextWithBranding t replace branding
  where
    replace :: Text -> Text
    replace "key" = Ascii.toText k
    replace "code" = Ascii.toText c
    replace x = x

-------------------------------------------------------------------------------
-- Team Activation Email

sendTeamActivationMailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  Name ->
  ActivationKey ->
  ActivationCode ->
  Maybe Locale ->
  Text ->
  Sem r ()
sendTeamActivationMailImpl userTemplates branding email name akey acode mLocale teamName = do
  let tpl = teamActivationEmail . snd $ forLocale mLocale userTemplates
  sendMail $ renderTeamActivationMail email name teamName akey acode tpl branding

renderTeamActivationMail :: EmailAddress -> Name -> Text -> ActivationKey -> ActivationCode -> TeamActivationEmailTemplate -> TemplateBranding -> Mail
renderTeamActivationMail email name teamName akey@(ActivationKey key) acode@(ActivationCode code) TeamActivationEmailTemplate {..} branding =
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
    from, to :: Address
    from = Address (Just teamActivationEmailSenderName) (fromEmail teamActivationEmailSender)
    to = mkMimeAddress name email
    txt, html, subj :: LText
    txt = renderTextWithBranding teamActivationEmailBodyText replace branding
    html = renderHtmlWithBranding teamActivationEmailBodyHtml replace branding
    subj = renderTextWithBranding teamActivationEmailSubject replace branding
    replace :: Text -> Text
    replace "url" = renderActivationUrl teamActivationEmailUrl akey acode branding
    replace "email" = fromEmail email
    replace "name" = fromName name
    replace "team" = teamName
    replace x = x

-------------------------------------------------------------------------------
-- Verification Email

sendVerificationMailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  ActivationKey ->
  ActivationCode ->
  Maybe Locale ->
  Sem r ()
sendVerificationMailImpl userTemplates branding email akey acode mLocale = do
  let tpl = verificationEmail . snd $ forLocale mLocale userTemplates
  sendMail $ renderVerificationMail email akey acode tpl branding

renderVerificationMail :: EmailAddress -> ActivationKey -> ActivationCode -> VerificationEmailTemplate -> TemplateBranding -> Mail
renderVerificationMail email akey acode VerificationEmailTemplate {..} branding =
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
    txt = renderTextWithBranding verificationEmailBodyText replace branding
    html = renderHtmlWithBranding verificationEmailBodyHtml replace branding
    subj = renderTextWithBranding verificationEmailSubject replace branding
    replace "code" = Ascii.toText code
    replace "email" = fromEmail email
    replace x = x

-------------------------------------------------------------------------------
-- Password Reset Email

sendPasswordResetMailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  PasswordResetKey ->
  PasswordResetCode ->
  Maybe Locale ->
  Sem r ()
sendPasswordResetMailImpl userTemplates branding email pkey pcode mLocale = do
  let tpl = passwordResetEmail . snd $ forLocale mLocale userTemplates
  sendMail $ renderPwResetMail email pkey pcode tpl branding

renderPwResetMail :: EmailAddress -> PasswordResetKey -> PasswordResetCode -> PasswordResetEmailTemplate -> TemplateBranding -> Mail
renderPwResetMail email pkey pcode PasswordResetEmailTemplate {..} branding =
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
    txt = renderTextWithBranding passwordResetEmailBodyText replace branding
    html = renderHtmlWithBranding passwordResetEmailBodyHtml replace branding
    subj = renderTextWithBranding passwordResetEmailSubject replace branding
    replace "url" = renderPwResetUrl passwordResetEmailUrl (pkey, pcode) branding
    replace x = x

renderPwResetUrl :: Template -> PasswordResetPair -> TemplateBranding -> Text
renderPwResetUrl t (PasswordResetKey k, PasswordResetCode c) branding =
  toStrict $ renderTextWithBranding t replace branding
  where
    replace "key" = Ascii.toText k
    replace "code" = Ascii.toText c
    replace x = x

-------------------------------------------------------------------------------
-- New Client Email

sendNewClientEmailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  Name ->
  Client ->
  Locale ->
  Sem r ()
sendNewClientEmailImpl userTemplates branding email name client locale = do
  let tpl = newClientEmail . snd $ forLocale (Just locale) userTemplates
  sendMail $ renderNewClientEmail email name locale client tpl branding

renderNewClientEmail :: EmailAddress -> Name -> Locale -> Client -> NewClientEmailTemplate -> TemplateBranding -> Mail
renderNewClientEmail email name locale Client {..} NewClientEmailTemplate {..} branding =
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
    txt = renderTextWithBranding newClientEmailBodyText replace branding
    html = renderHtmlWithBranding newClientEmailBodyHtml replace branding
    subj = renderTextWithBranding newClientEmailSubject replace branding
    replace "name" = fromName name
    replace "label" = fromMaybe defRequestId clientLabel
    replace "model" = fromMaybe defRequestId clientModel
    replace "date" =
      formatDateTime
        "%A %e %B %Y, %H:%M - %Z"
        (timeLocale locale)
        (fromUTCTimeMillis clientTime)
    replace x = x

-------------------------------------------------------------------------------
-- Deletion Email

sendAccountDeletionEmailImpl ::
  (Member EmailSending r) =>
  Localised UserTemplates ->
  TemplateBranding ->
  EmailAddress ->
  Name ->
  Code.Key ->
  Code.Value ->
  Locale ->
  Sem r ()
sendAccountDeletionEmailImpl userTemplates branding email name key code locale = do
  let tpl = deletionEmail . snd $ forLocale (Just locale) userTemplates
  sendMail $ renderDeletionEmail email name key code tpl branding

renderDeletionEmail :: EmailAddress -> Name -> Code.Key -> Code.Value -> DeletionEmailTemplate -> TemplateBranding -> Mail
renderDeletionEmail email name cKey cValue DeletionEmailTemplate {..} branding =
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
    txt = renderTextWithBranding deletionEmailBodyText replace1 branding
    html = renderHtmlWithBranding deletionEmailBodyHtml replace1 branding
    subj = renderTextWithBranding deletionEmailSubject replace1 branding
    key = Ascii.toText (fromRange (Code.asciiKey cKey))
    code = Ascii.toText (fromRange (Code.asciiValue cValue))
    replace1 "url" = toStrict (renderTextWithBranding deletionEmailUrl replace2 branding)
    replace1 "email" = fromEmail email
    replace1 "name" = fromName name
    replace1 x = x
    replace2 "key" = key
    replace2 "code" = code
    replace2 x = x

-------------------------------------------------------------------------------
-- Invitation Email

sendTeamInvitationMailImpl :: (Member EmailSending r) => Localised TeamTemplates -> TemplateBranding -> EmailAddress -> TeamId -> EmailAddress -> InvitationCode -> Maybe Locale -> Sem r Text
sendTeamInvitationMailImpl teamTemplates branding to tid from code loc = do
  let tpl = invitationEmail . snd $ forLocale loc teamTemplates
      mail = InvitationEmail to tid code from
      (renderedMail, renderedInvitaitonUrl) = renderInvitationEmail mail tpl branding
  sendMail renderedMail
  pure renderedInvitaitonUrl

sendTeamInvitationMailPersonalUserImpl :: (Member EmailSending r) => Localised TeamTemplates -> TemplateBranding -> EmailAddress -> TeamId -> EmailAddress -> InvitationCode -> Maybe Locale -> Sem r Text
sendTeamInvitationMailPersonalUserImpl teamTemplates branding to tid from code loc = do
  let tpl = existingUserInvitationEmail . snd $ forLocale loc teamTemplates
      mail = InvitationEmail to tid code from
      (renderedMail, renderedInvitaitonUrl) = renderInvitationEmail mail tpl branding
  sendMail renderedMail
  pure renderedInvitaitonUrl

data InvitationEmail = InvitationEmail
  { invTo :: !EmailAddress,
    invTeamId :: !TeamId,
    invInvCode :: !InvitationCode,
    invInviter :: !EmailAddress
  }

renderInvitationEmail :: InvitationEmail -> InvitationEmailTemplate -> TemplateBranding -> (Mail, Text)
renderInvitationEmail InvitationEmail {..} InvitationEmailTemplate {..} branding =
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
    txt = renderTextWithBranding invitationEmailBodyText replace branding
    html = renderHtmlWithBranding invitationEmailBodyHtml replace branding
    subj = renderTextWithBranding invitationEmailSubject replace branding
    invitationUrl = renderInvitationUrl invitationEmailUrl invTeamId invInvCode branding
    replace "url" = invitationUrl
    replace "inviter" = fromEmail invInviter
    replace x = x

renderInvitationUrl :: Template -> TeamId -> InvitationCode -> TemplateBranding -> Text
renderInvitationUrl t tid (InvitationCode c) branding =
  toStrict $ renderTextWithBranding t replace branding
  where
    replace "team" = idToText tid
    replace "code" = Ascii.toText c
    replace x = x

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
