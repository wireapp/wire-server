{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Brig.Provider.Email
    ( sendActivationMail
    , sendApprovalRequestMail
    , sendApprovalConfirmMail
    ) where

import Brig.App
import Brig.Email
import Brig.Provider.Template
import Brig.Types.Common
import Brig.Types.Provider
import Data.ByteString.Conversion
import Data.Range
import Data.Text (Text, pack)

import qualified Brig.Types.Code    as Code
import qualified Data.Text.Ascii    as Ascii
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy     as LT

-------------------------------------------------------------------------------
-- Activation Email

sendActivationMail :: Name -> Email -> Code.Key -> Code.Value -> AppIO ()
sendActivationMail name email key code = do
    tpl <- activationEmail . snd <$> providerTemplates Nothing
    let mail = ActivationEmail email name key code
    sendMail $ renderActivationMail mail tpl

data ActivationEmail = ActivationEmail
    { acmTo   :: !Email
    , acmName :: !Name
    , acmKey  :: !Code.Key
    , acmCode :: !Code.Value
    }

renderActivationMail :: ActivationEmail -> ActivationEmailTemplate -> Mail
renderActivationMail ActivationEmail{..} ActivationEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", LT.toStrict subj)
                        , ("X-Zeta-Purpose", "ProviderActivation")
                        , ("X-Zeta-Key", Ascii.toText (fromRange key))
                        , ("X-Zeta-Code", Ascii.toText (fromRange code))
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    (Code.Key key, Code.Value code) = (acmKey, acmCode)

    from = Address (Just "Wire") (fromEmail activationEmailSender)
    to   = mkMimeAddress acmName acmTo
    txt  = renderText activationEmailBodyText replace
    html = renderHtml activationEmailBodyHtml replace
    subj = renderText activationEmailSubject  replace

    replace "url"   = renderActivationUrl activationEmailUrl acmKey acmCode
    replace "email" = fromEmail acmTo
    replace "name"  = fromName acmName
    replace x       = x

renderActivationUrl :: Template -> Code.Key -> Code.Value -> Text
renderActivationUrl t (Code.Key k) (Code.Value v) =
    LT.toStrict $ renderText t replace
  where
    replace "key"  = Ascii.toText (fromRange k)
    replace "code" = Ascii.toText (fromRange v)
    replace x      = x

--------------------------------------------------------------------------------
-- Approval Request Email

sendApprovalRequestMail :: Name -> Email -> HttpsUrl -> Text -> Code.Key -> Code.Value -> AppIO ()
sendApprovalRequestMail name email url descr key val = do
    tpl <- approvalRequestEmail . snd <$> providerTemplates Nothing
    let mail = ApprovalRequestEmail email name url descr key val
    sendMail $ renderApprovalRequestMail mail tpl

data ApprovalRequestEmail = ApprovalRequestEmail
    { aprTo    :: !Email
    , aprName  :: !Name
    , aprUrl   :: !HttpsUrl
    , aprDescr :: !Text
    , aprKey   :: !Code.Key
    , aprCode  :: !Code.Value
    }

renderApprovalRequestMail :: ApprovalRequestEmail -> ApprovalRequestEmailTemplate -> Mail
renderApprovalRequestMail ApprovalRequestEmail{..} ApprovalRequestEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", LT.toStrict subj)
                        , ("X-Zeta-Purpose", "ProviderApprovalRequest")
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    from = Address (Just "Wire")       (fromEmail approvalRequestEmailSender)
    to   = Address (Just "Wire Staff") (fromEmail approvalRequestEmailTo)
    txt  = renderText approvalRequestEmailBodyText replace
    html = renderHtml approvalRequestEmailBodyHtml replace
    subj = renderText approvalRequestEmailSubject  replace

    replace "email"       = fromEmail aprTo
    replace "name"        = fromName aprName
    replace "url"         = Text.decodeUtf8 (toByteString' aprUrl)
    replace "description" = aprDescr
    replace "approvalUrl" = renderApprovalUrl approvalRequestEmailUrl aprKey aprCode
    replace x             = x

-- TODO: Unify with renderActivationUrl
renderApprovalUrl :: Template -> Code.Key -> Code.Value -> Text
renderApprovalUrl t (Code.Key k) (Code.Value v) =
    LT.toStrict $ renderText t replace
  where
    replace "key"  = Ascii.toText (fromRange k)
    replace "code" = Ascii.toText (fromRange v)
    replace x      = x

--------------------------------------------------------------------------------
-- Approval Confirmation Email

sendApprovalConfirmMail :: Name -> Email -> AppIO ()
sendApprovalConfirmMail name email = do
    tpl <- approvalConfirmEmail . snd <$> providerTemplates Nothing
    let mail = ApprovalConfirmEmail email name
    sendMail $ renderApprovalConfirmMail mail tpl

data ApprovalConfirmEmail = ApprovalConfirmEmail
    { apcTo   :: !Email
    , apcName :: !Name
    }

renderApprovalConfirmMail :: ApprovalConfirmEmail -> ApprovalConfirmEmailTemplate -> Mail
renderApprovalConfirmMail ApprovalConfirmEmail{..} ApprovalConfirmEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", LT.toStrict subj)
                        , ("X-Zeta-Purpose", "ProviderApprovalConfirm")
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    from = Address (Just "Wire") (fromEmail approvalConfirmEmailSender)
    to   = mkMimeAddress apcName apcTo
    txt  = renderText approvalConfirmEmailBodyText replace
    html = renderHtml approvalConfirmEmailBodyHtml replace
    subj = renderText approvalConfirmEmailSubject  replace

    replace "homeUrl" = pack $ show approvalConfirmEmailHomeUrl
    replace "email"   = fromEmail apcTo
    replace "name"    = fromName apcName
    replace x         = x

