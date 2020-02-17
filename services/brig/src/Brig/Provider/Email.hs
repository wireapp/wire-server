{-# LANGUAGE RecordWildCards #-}

module Brig.Provider.Email
  ( sendActivationMail,
    sendApprovalRequestMail,
    sendApprovalConfirmMail,
    sendPasswordResetMail,
  )
where

import Brig.App
import Brig.Email
import Brig.Provider.Template
import Brig.Template
import qualified Brig.Types.Code as Code
import Brig.Types.Common
import Brig.Types.Provider
import Control.Lens (view)
import Data.ByteString.Conversion
import Data.Range
import Data.Text (pack)
import qualified Data.Text.Ascii as Ascii
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LT
import Imports

-------------------------------------------------------------------------------
-- Activation Email

sendActivationMail :: Name -> Email -> Code.Key -> Code.Value -> Bool -> AppIO ()
sendActivationMail name email key code update = do
  tpl <- selectTemplate update . snd <$> providerTemplates Nothing
  branding <- view templateBranding
  let mail = ActivationEmail email name key code
  sendMail $ renderActivationMail mail tpl branding
  where
    selectTemplate True = activationEmailUpdate
    selectTemplate False = activationEmail

data ActivationEmail
  = ActivationEmail
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
-- Approval Request Email

sendApprovalRequestMail :: Name -> Email -> HttpsUrl -> Text -> Code.Key -> Code.Value -> AppIO ()
sendApprovalRequestMail name email url descr key val = do
  tpl <- approvalRequestEmail . snd <$> providerTemplates Nothing
  branding <- view templateBranding
  let mail = ApprovalRequestEmail email name url descr key val
  sendMail $ renderApprovalRequestMail mail tpl branding

data ApprovalRequestEmail
  = ApprovalRequestEmail
      { aprTo :: !Email,
        aprName :: !Name,
        aprUrl :: !HttpsUrl,
        aprDescr :: !Text,
        aprKey :: !Code.Key,
        aprCode :: !Code.Value
      }

renderApprovalRequestMail :: ApprovalRequestEmail -> ApprovalRequestEmailTemplate -> TemplateBranding -> Mail
renderApprovalRequestMail ApprovalRequestEmail {..} ApprovalRequestEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", LT.toStrict subj),
          ("X-Zeta-Purpose", "ProviderApprovalRequest")
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    from = Address (Just approvalRequestEmailSenderName) (fromEmail approvalRequestEmailSender)
    to = Address (Just "Provider Approval Staff") (fromEmail approvalRequestEmailTo)
    txt = renderTextWithBranding approvalRequestEmailBodyText replace branding
    html = renderHtmlWithBranding approvalRequestEmailBodyHtml replace branding
    subj = renderTextWithBranding approvalRequestEmailSubject replace branding
    replace "email" = fromEmail aprTo
    replace "name" = fromName aprName
    replace "url" = Text.decodeUtf8 (toByteString' aprUrl)
    replace "description" = aprDescr
    replace "approvalUrl" = renderApprovalUrl approvalRequestEmailUrl aprKey aprCode branding
    replace x = x

-- TODO: Unify with renderActivationUrl
renderApprovalUrl :: Template -> Code.Key -> Code.Value -> TemplateBranding -> Text
renderApprovalUrl t (Code.Key k) (Code.Value v) branding =
  LT.toStrict $ renderTextWithBranding t replace branding
  where
    replace "key" = Ascii.toText (fromRange k)
    replace "code" = Ascii.toText (fromRange v)
    replace x = x

--------------------------------------------------------------------------------
-- Approval Confirmation Email

sendApprovalConfirmMail :: Name -> Email -> AppIO ()
sendApprovalConfirmMail name email = do
  tpl <- approvalConfirmEmail . snd <$> providerTemplates Nothing
  branding <- view templateBranding
  let mail = ApprovalConfirmEmail email name
  sendMail $ renderApprovalConfirmMail mail tpl branding

data ApprovalConfirmEmail
  = ApprovalConfirmEmail
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

sendPasswordResetMail :: Email -> Code.Key -> Code.Value -> AppIO ()
sendPasswordResetMail to key code = do
  tpl <- passwordResetEmail . snd <$> providerTemplates Nothing
  branding <- view templateBranding
  let mail = PasswordResetEmail to key code
  sendMail $ renderPwResetMail mail tpl branding

data PasswordResetEmail
  = PasswordResetEmail
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
