{-# LANGUAGE RecordWildCards #-}

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

module Brig.Provider.Email
  ( sendActivationMail,
    sendApprovalConfirmMail,
    sendPasswordResetMail,
  )
where

import Brig.App
import Brig.Provider.Template
import Control.Lens (view)
import Data.Code qualified as Code
import Data.Range
import Data.Text (pack)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Lazy qualified as LT
import Imports
import Network.Mail.Mime
import Polysemy
import Wire.API.User
import Wire.EmailSending
import Wire.EmailSubsystem.Interpreter (mkMimeAddress)
import Wire.EmailSubsystem.Template (TemplateBranding, renderHtmlWithBranding, renderTextWithBranding)

-------------------------------------------------------------------------------
-- Activation Email

sendActivationMail :: (Member EmailSending r) => Name -> Email -> Code.Key -> Code.Value -> Bool -> (AppT r) ()
sendActivationMail name email key code update = do
  tpl <- selectTemplate update . snd <$> providerTemplates Nothing
  branding <- view templateBranding
  let mail = ActivationEmail email name key code
  liftSem $ sendMail $ renderActivationMail mail tpl branding
  where
    selectTemplate True = activationEmailUpdate
    selectTemplate False = activationEmail

data ActivationEmail = ActivationEmail
  { acmTo :: !Email,
    acmName :: !Name,
    acmKey :: !Code.Key,
    acmCode :: !Code.Value
  }

renderActivationMail :: ActivationEmail -> ActivationEmailTemplate -> TemplateBranding -> Mail
renderActivationMail ActivationEmail {..} ActivationEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", LT.toStrict subj),
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
    replace "url" = renderActivationUrl activationEmailUrl acmKey acmCode branding
    replace "email" = fromEmail acmTo
    replace "name" = fromName acmName
    replace x = x

renderActivationUrl :: Template -> Code.Key -> Code.Value -> TemplateBranding -> Text
renderActivationUrl t (Code.Key k) (Code.Value v) branding =
  LT.toStrict $ renderTextWithBranding t replace branding
  where
    replace "key" = Ascii.toText (fromRange k)
    replace "code" = Ascii.toText (fromRange v)
    replace x = x

--------------------------------------------------------------------------------
-- Approval Confirmation Email

sendApprovalConfirmMail :: (Member EmailSending r) => Name -> Email -> (AppT r) ()
sendApprovalConfirmMail name email = do
  tpl <- approvalConfirmEmail . snd <$> providerTemplates Nothing
  branding <- view templateBranding
  let mail = ApprovalConfirmEmail email name
  liftSem $ sendMail $ renderApprovalConfirmMail mail tpl branding

data ApprovalConfirmEmail = ApprovalConfirmEmail
  { apcTo :: !Email,
    apcName :: !Name
  }

renderApprovalConfirmMail :: ApprovalConfirmEmail -> ApprovalConfirmEmailTemplate -> TemplateBranding -> Mail
renderApprovalConfirmMail ApprovalConfirmEmail {..} ApprovalConfirmEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", LT.toStrict subj),
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

--------------------------------------------------------------------------------
-- Password Reset Email

sendPasswordResetMail :: (Member EmailSending r) => Email -> Code.Key -> Code.Value -> (AppT r) ()
sendPasswordResetMail to key code = do
  tpl <- passwordResetEmail . snd <$> providerTemplates Nothing
  branding <- view templateBranding
  let mail = PasswordResetEmail to key code
  liftSem $ sendMail $ renderPwResetMail mail tpl branding

data PasswordResetEmail = PasswordResetEmail
  { pwrTo :: !Email,
    pwrKey :: !Code.Key,
    pwrCode :: !Code.Value
  }

renderPwResetMail :: PasswordResetEmail -> PasswordResetEmailTemplate -> TemplateBranding -> Mail
renderPwResetMail PasswordResetEmail {..} PasswordResetEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", LT.toStrict subj),
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
    replace "url" = renderPwResetUrl passwordResetEmailUrl pwrKey pwrCode branding
    replace x = x

renderPwResetUrl :: Template -> Code.Key -> Code.Value -> TemplateBranding -> Text
renderPwResetUrl t (Code.Key k) (Code.Value v) branding =
  LT.toStrict $ renderTextWithBranding t replace branding
  where
    replace "key" = Ascii.toText (fromRange k)
    replace "code" = Ascii.toText (fromRange v)
    replace x = x
