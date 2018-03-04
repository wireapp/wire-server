{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Brig.Provider.Template
    ( ProviderTemplates            (..)
    , ActivationEmailTemplate      (..)
    , ApprovalRequestEmailTemplate (..)
    , ApprovalConfirmEmailTemplate (..)
    , PasswordResetEmailTemplate   (..)
    -- , TODO: NewServiceEmailTemplate   (..)
    , loadProviderTemplates

      -- * Re-exports
    , Template
    , renderText
    , renderHtml
    ) where

import Brig.Options
import Brig.Template
import Brig.Types
import Data.ByteString.Conversion (fromByteString)
import Data.Misc (HttpsUrl)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text.Encoding (encodeUtf8)

data ProviderTemplates = ProviderTemplates
    { activationEmail       :: !ActivationEmailTemplate
    , activationEmailUpdate :: !ActivationEmailTemplate
    , approvalRequestEmail  :: !ApprovalRequestEmailTemplate
    , approvalConfirmEmail  :: !ApprovalConfirmEmailTemplate
    , passwordResetEmail    :: !PasswordResetEmailTemplate
    }

data ActivationEmailTemplate = ActivationEmailTemplate
    { activationEmailUrl        :: !Template
    , activationEmailSubject    :: !Template
    , activationEmailBodyText   :: !Template
    , activationEmailBodyHtml   :: !Template
    , activationEmailSender     :: !Email
    }

data ApprovalRequestEmailTemplate = ApprovalRequestEmailTemplate
    { approvalRequestEmailUrl        :: !Template
    , approvalRequestEmailSubject    :: !Template
    , approvalRequestEmailBodyText   :: !Template
    , approvalRequestEmailBodyHtml   :: !Template
    , approvalRequestEmailSender     :: !Email
    , approvalRequestEmailTo         :: !Email
    }

data ApprovalConfirmEmailTemplate = ApprovalConfirmEmailTemplate
    { approvalConfirmEmailSubject    :: !Template
    , approvalConfirmEmailBodyText   :: !Template
    , approvalConfirmEmailBodyHtml   :: !Template
    , approvalConfirmEmailSender     :: !Email
    , approvalConfirmEmailHomeUrl    :: !HttpsUrl
    }

data PasswordResetEmailTemplate = PasswordResetEmailTemplate
    { passwordResetEmailUrl        :: !Template
    , passwordResetEmailSubject    :: !Template
    , passwordResetEmailBodyText   :: !Template
    , passwordResetEmailBodyHtml   :: !Template
    , passwordResetEmailSender     :: !Email
    }

-- TODO
-- data NewServiceEmailTemplate = NewServiceEmailTemplate
--     { newServiceEmailSubject    :: !Template
--     , newServiceEmailBodyText   :: !Template
--     , newServiceEmailBodyHtml   :: !Template
--     , newServiceEmailSender     :: !Email
--     , newServiceEmailSenderName :: !Text
--     }

loadProviderTemplates :: Opts -> IO (Localised ProviderTemplates)
loadProviderTemplates o = readLocalesDir defLocale templates $ \fp ->
    ProviderTemplates
        <$> (ActivationEmailTemplate activationUrl'
                <$> readTemplate (fp <> "/email/activation-subject.txt")
                <*> readTemplate (fp <> "/email/activation.txt")
                <*> readTemplate (fp <> "/email/activation.html")
                <*> pure (emailSender gOptions))
        <*> (ActivationEmailTemplate activationUrl'
                <$> readTemplate (fp <> "/email/update-subject.txt")
                <*> readTemplate (fp <> "/email/update.txt")
                <*> readTemplate (fp <> "/email/update.html")
                <*> pure (emailSender gOptions))
        <*> (ApprovalRequestEmailTemplate approvalUrl'
                <$> readTemplate (fp <> "/email/approval-request-subject.txt")
                <*> readTemplate (fp <> "/email/approval-request.txt")
                <*> readTemplate (fp <> "/email/approval-request.html")
                <*> pure (emailSender gOptions)
                <*> pure (approvalTo pOptions))
        <*> (ApprovalConfirmEmailTemplate
                <$> readTemplate (fp <> "/email/approval-confirm-subject.txt")
                <*> readTemplate (fp <> "/email/approval-confirm.txt")
                <*> readTemplate (fp <> "/email/approval-confirm.html")
                <*> pure (emailSender gOptions)
                <*> pure (fromMaybe (error "Invalid HTTPS URL") maybeUrl))
        <*> (PasswordResetEmailTemplate pwResetUrl'
                <$> readTemplate (fp <> "/email/password-reset-subject.txt")
                <*> readTemplate (fp <> "/email/password-reset.txt")
                <*> readTemplate (fp <> "/email/password-reset.html")
                <*> pure (emailSender gOptions))
  where
    maybeUrl  = fromByteString $ encodeUtf8 $ homeUrl pOptions
    gOptions  = general $ emailSMS o
    pOptions  = provider $ emailSMS o
    templates = templateDir gOptions <> "/provider"
    defLocale = setDefaultLocale (optSettings o)

    -- URL templates
    activationUrl' = template $ providerActivationUrl pOptions
    approvalUrl'   = template $ approvalUrl pOptions
    pwResetUrl'    = template $ providerPwResetUrl pOptions

