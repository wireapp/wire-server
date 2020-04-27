{-# LANGUAGE RecordWildCards #-}

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

module Brig.User.Email
  ( sendActivationMail,
    sendVerificationMail,
    sendTeamActivationMail,
    sendPasswordResetMail,
    sendDeletionEmail,
    sendNewClientEmail,

    -- * Re-exports
    validateEmail,
  )
where

import Brig.App
import Brig.Email
import qualified Brig.Email as Email
import Brig.Locale (formatDateTime, timeLocale)
import Brig.Template
import Brig.Types
import qualified Brig.Types.Code as Code
import Brig.User.Template
import Control.Lens (view)
import Data.Json.Util (fromUTCTimeMillis)
import Data.Range
import qualified Data.Text.Ascii as Ascii
import Data.Text.Lazy (toStrict)
import Imports

sendVerificationMail :: Email -> ActivationPair -> Maybe Locale -> AppIO ()
sendVerificationMail to pair loc = do
  tpl <- verificationEmail . snd <$> userTemplates loc
  branding <- view templateBranding
  let mail = VerificationEmail to pair
  Email.sendMail $ renderVerificationMail mail tpl branding

sendActivationMail :: Email -> Name -> ActivationPair -> Maybe Locale -> Maybe UserIdentity -> AppIO ()
sendActivationMail to name pair loc ident = do
  tpl <- selectTemplate . snd <$> userTemplates loc
  branding <- view templateBranding
  let mail = ActivationEmail to name pair
  Email.sendMail $ renderActivationMail mail tpl branding
  where
    selectTemplate =
      if isNothing ident
        then activationEmail
        else activationEmailUpdate

sendPasswordResetMail :: Email -> PasswordResetPair -> Maybe Locale -> AppIO ()
sendPasswordResetMail to pair loc = do
  tpl <- passwordResetEmail . snd <$> userTemplates loc
  branding <- view templateBranding
  let mail = PasswordResetEmail to pair
  Email.sendMail $ renderPwResetMail mail tpl branding

sendDeletionEmail :: Name -> Email -> Code.Key -> Code.Value -> Locale -> AppIO ()
sendDeletionEmail name email key code locale = do
  tpl <- deletionEmail . snd <$> userTemplates (Just locale)
  branding <- view templateBranding
  Email.sendMail $ renderDeletionEmail tpl (DeletionEmail email name key code) branding

sendNewClientEmail :: Name -> Email -> Client -> Locale -> AppIO ()
sendNewClientEmail name email client locale = do
  tpl <- newClientEmail . snd <$> userTemplates (Just locale)
  branding <- view templateBranding
  Email.sendMail $ renderNewClientEmail tpl (NewClientEmail locale email name client) branding

sendTeamActivationMail :: Email -> Name -> ActivationPair -> Maybe Locale -> Text -> AppIO ()
sendTeamActivationMail to name pair loc team = do
  tpl <- teamActivationEmail . snd <$> userTemplates loc
  let mail = TeamActivationEmail to name team pair
  branding <- view templateBranding
  Email.sendMail $ renderTeamActivationMail mail tpl branding

-------------------------------------------------------------------------------
-- New Client Email

data NewClientEmail = NewClientEmail
  { nclLocale :: !Locale,
    nclTo :: !Email,
    nclName :: !Name,
    nclClient :: !Client
  }

renderNewClientEmail :: NewClientEmailTemplate -> NewClientEmail -> TemplateBranding -> Mail
renderNewClientEmail NewClientEmailTemplate {..} NewClientEmail {..} branding =
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
    to = mkMimeAddress nclName nclTo
    txt = renderTextWithBranding newClientEmailBodyText replace branding
    html = renderHtmlWithBranding newClientEmailBodyHtml replace branding
    subj = renderTextWithBranding newClientEmailSubject replace branding
    replace "name" = fromName nclName
    replace "label" = fromMaybe "N/A" (clientLabel nclClient)
    replace "model" = fromMaybe "N/A" (clientModel nclClient)
    replace "date" =
      formatDateTime
        "%A %e %B %Y, %H:%M - %Z"
        (timeLocale nclLocale)
        (fromUTCTimeMillis $ clientTime nclClient)
    replace x = x

-------------------------------------------------------------------------------
-- Deletion Email

data DeletionEmail = DeletionEmail
  { delTo :: !Email,
    delName :: !Name,
    delKey :: !Code.Key,
    delCode :: !Code.Value
  }

renderDeletionEmail :: DeletionEmailTemplate -> DeletionEmail -> TemplateBranding -> Mail
renderDeletionEmail DeletionEmailTemplate {..} DeletionEmail {..} branding =
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
    to = mkMimeAddress delName delTo
    txt = renderTextWithBranding deletionEmailBodyText replace1 branding
    html = renderHtmlWithBranding deletionEmailBodyHtml replace1 branding
    subj = renderTextWithBranding deletionEmailSubject replace1 branding
    key = Ascii.toText (fromRange (Code.asciiKey delKey))
    code = Ascii.toText (fromRange (Code.asciiValue delCode))
    replace1 "url" = toStrict (renderTextWithBranding deletionEmailUrl replace2 branding)
    replace1 "email" = fromEmail delTo
    replace1 "name" = fromName delName
    replace1 x = x
    replace2 "key" = key
    replace2 "code" = code
    replace2 x = x

-------------------------------------------------------------------------------
-- Verification Email

data VerificationEmail = VerificationEmail
  { vfTo :: !Email,
    vfPair :: !ActivationPair
  }

renderVerificationMail :: VerificationEmail -> VerificationEmailTemplate -> TemplateBranding -> Mail
renderVerificationMail VerificationEmail {..} VerificationEmailTemplate {..} branding =
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
    (ActivationKey _, ActivationCode code) = vfPair
    from = Address (Just verificationEmailSenderName) (fromEmail verificationEmailSender)
    to = Address Nothing (fromEmail vfTo)
    txt = renderTextWithBranding verificationEmailBodyText replace branding
    html = renderHtmlWithBranding verificationEmailBodyHtml replace branding
    subj = renderTextWithBranding verificationEmailSubject replace branding
    replace "code" = Ascii.toText code
    replace "email" = fromEmail vfTo
    replace x = x

-------------------------------------------------------------------------------
-- Activation Email

data ActivationEmail = ActivationEmail
  { acmTo :: !Email,
    acmName :: !Name,
    acmPair :: !ActivationPair
  }

renderActivationMail :: ActivationEmail -> ActivationEmailTemplate -> TemplateBranding -> Mail
renderActivationMail ActivationEmail {..} ActivationEmailTemplate {..} branding =
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
    (ActivationKey key, ActivationCode code) = acmPair
    from = Address (Just activationEmailSenderName) (fromEmail activationEmailSender)
    to = mkMimeAddress acmName acmTo
    txt = renderTextWithBranding activationEmailBodyText replace branding
    html = renderHtmlWithBranding activationEmailBodyHtml replace branding
    subj = renderTextWithBranding activationEmailSubject replace branding
    replace "url" = renderActivationUrl activationEmailUrl acmPair branding
    replace "email" = fromEmail acmTo
    replace "name" = fromName acmName
    replace x = x

renderActivationUrl :: Template -> ActivationPair -> TemplateBranding -> Text
renderActivationUrl t (ActivationKey k, ActivationCode c) branding =
  toStrict $ renderTextWithBranding t replace branding
  where
    replace "key" = Ascii.toText k
    replace "code" = Ascii.toText c
    replace x = x

-------------------------------------------------------------------------------
-- Team Activation Email

data TeamActivationEmail = TeamActivationEmail
  { tacmTo :: !Email,
    tacmName :: !Name,
    tacmTeamName :: !Text,
    tacmPair :: !ActivationPair
  }

renderTeamActivationMail :: TeamActivationEmail -> TeamActivationEmailTemplate -> TemplateBranding -> Mail
renderTeamActivationMail TeamActivationEmail {..} TeamActivationEmailTemplate {..} branding =
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
    (ActivationKey key, ActivationCode code) = tacmPair
    from = Address (Just teamActivationEmailSenderName) (fromEmail teamActivationEmailSender)
    to = mkMimeAddress tacmName tacmTo
    txt = renderTextWithBranding teamActivationEmailBodyText replace branding
    html = renderHtmlWithBranding teamActivationEmailBodyHtml replace branding
    subj = renderTextWithBranding teamActivationEmailSubject replace branding
    replace "url" = renderActivationUrl teamActivationEmailUrl tacmPair branding
    replace "email" = fromEmail tacmTo
    replace "name" = fromName tacmName
    replace "team" = tacmTeamName
    replace x = x

-------------------------------------------------------------------------------
-- Password Reset Email

data PasswordResetEmail = PasswordResetEmail
  { pwrTo :: !Email,
    pwrPair :: !PasswordResetPair
  }

renderPwResetMail :: PasswordResetEmail -> PasswordResetEmailTemplate -> TemplateBranding -> Mail
renderPwResetMail PasswordResetEmail {..} PasswordResetEmailTemplate {..} branding =
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
    (PasswordResetKey key, PasswordResetCode code) = pwrPair
    from = Address (Just passwordResetEmailSenderName) (fromEmail passwordResetEmailSender)
    to = Address Nothing (fromEmail pwrTo)
    txt = renderTextWithBranding passwordResetEmailBodyText replace branding
    html = renderHtmlWithBranding passwordResetEmailBodyHtml replace branding
    subj = renderTextWithBranding passwordResetEmailSubject replace branding
    replace "url" = renderPwResetUrl passwordResetEmailUrl pwrPair branding
    replace x = x

renderPwResetUrl :: Template -> PasswordResetPair -> TemplateBranding -> Text
renderPwResetUrl t (PasswordResetKey k, PasswordResetCode c) branding =
  toStrict $ renderTextWithBranding t replace branding
  where
    replace "key" = Ascii.toText k
    replace "code" = Ascii.toText c
    replace x = x
