{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Brig.Provider.Template
    ( ProviderTemplates            (..)
    , ActivationEmailTemplate      (..)
    , ApprovalRequestEmailTemplate (..)
    , ApprovalConfirmEmailTemplate (..)
    -- , TODO: NewServiceEmailTemplate   (..)
    , loadProviderTemplates
    ) where

import Brig.Options
import Brig.Template
import Brig.Types
import Data.Misc (HttpsUrl)
import Data.Monoid

import qualified Data.Text.Encoding as Text

data ProviderTemplates = ProviderTemplates
    { activationEmail      :: !ActivationEmailTemplate
    , approvalRequestEmail :: !ApprovalRequestEmailTemplate
    , approvalConfirmEmail :: !ApprovalConfirmEmailTemplate
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

-- TODO
-- data NewServiceEmailTemplate = NewServiceEmailTemplate
--     { newServiceEmailSubject    :: !Template
--     , newServiceEmailBodyText   :: !Template
--     , newServiceEmailBodyHtml   :: !Template
--     , newServiceEmailSender     :: !Email
--     , newServiceEmailSenderName :: !Text
--     }

loadProviderTemplates :: Opts -> IO (Localised ProviderTemplates)
loadProviderTemplates o = readLocalesDir defLocale templateDir $ \fp ->
    ProviderTemplates
        <$> (ActivationEmailTemplate activationUrl
                <$> readTemplate (fp <> "/email/activation-subject.txt")
                <*> readTemplate (fp <> "/email/activation.txt")
                <*> readTemplate (fp <> "/email/activation.html")
                <*> pure (optEmailSender o))
        <*> (ApprovalRequestEmailTemplate approvalUrl
                <$> readTemplate (fp <> "/email/approval-request-subject.txt")
                <*> readTemplate (fp <> "/email/approval-request.txt")
                <*> readTemplate (fp <> "/email/approval-request.html")
                <*> pure (optEmailSender o)
                <*> pure (optProviderApprovalTo o))
        <*> (ApprovalConfirmEmailTemplate
                <$> readTemplate (fp <> "/email/approval-confirm-subject.txt")
                <*> readTemplate (fp <> "/email/approval-confirm.txt")
                <*> readTemplate (fp <> "/email/approval-confirm.html")
                <*> pure (optEmailSender o)
                <*> pure (optProviderHomeUrl o))
  where
    templateDir = optTemplateDir o <> "/provider"
    defLocale   = setDefaultLocale (optSettings o)

    -- URL templates
    activationUrl = template . Text.decodeLatin1 $ optProviderActivationUrl o
    approvalUrl   = template . Text.decodeLatin1 $ optProviderApprovalUrl o

