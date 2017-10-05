{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Brig.User.Template
    ( UserTemplates              (..)
    , ActivationSmsTemplate      (..)
    , ActivationEmailTemplate    (..)
    , TeamActivationEmailTemplate(..)
    , ActivationCallTemplate     (..)
    , PasswordResetSmsTemplate   (..)
    , PasswordResetEmailTemplate (..)
    , InvitationEmailTemplate    (..)
    , InvitationSmsTemplate      (..)
    , LoginSmsTemplate           (..)
    , LoginCallTemplate          (..)
    , DeletionSmsTemplate        (..)
    , DeletionEmailTemplate      (..)
    , NewClientEmailTemplate     (..)
    , loadUserTemplates

      -- * Re-exports
    , Template
    , renderText
    , renderHtml
    ) where

import Brig.Options
import Brig.Template
import Brig.Types
import Data.Monoid
import Data.Text (Text)

import qualified Data.Text.Encoding as Text

data UserTemplates = UserTemplates
    { activationSms         :: !ActivationSmsTemplate
    , activationCall        :: !ActivationCallTemplate
    , activationEmail       :: !ActivationEmailTemplate
    , activationEmailUpdate :: !ActivationEmailTemplate
    , teamActivationEmail   :: !TeamActivationEmailTemplate
    , passwordResetSms      :: !PasswordResetSmsTemplate
    , passwordResetEmail    :: !PasswordResetEmailTemplate
    , invitationEmail       :: !InvitationEmailTemplate
    , invitationSms         :: !InvitationSmsTemplate
    , loginSms              :: !LoginSmsTemplate
    , loginCall             :: !LoginCallTemplate
    , deletionSms           :: !DeletionSmsTemplate
    , deletionEmail         :: !DeletionEmailTemplate
    , newClientEmail        :: !NewClientEmailTemplate
    }

data ActivationSmsTemplate = ActivationSmsTemplate
    { activationSmslUrl   :: !Template
    , activationSmsText   :: !Template
    , activationSmsSender :: !Text
    }

data ActivationCallTemplate = ActivationCallTemplate
    { activationCallText  :: !Template
    }

data ActivationEmailTemplate = ActivationEmailTemplate
    { activationEmailUrl        :: !Template
    , activationEmailSubject    :: !Template
    , activationEmailBodyText   :: !Template
    , activationEmailBodyHtml   :: !Template
    , activationEmailSender     :: !Email
    , activationEmailSenderName :: !Text
    }

data TeamActivationEmailTemplate = TeamActivationEmailTemplate
    { teamActivationEmailUrl        :: !Template
    , teamActivationEmailSubject    :: !Template
    , teamActivationEmailBodyText   :: !Template
    , teamActivationEmailBodyHtml   :: !Template
    , teamActivationEmailSender     :: !Email
    , teamActivationEmailSenderName :: !Text
    }

data DeletionEmailTemplate = DeletionEmailTemplate
    { deletionEmailUrl        :: !Template
    , deletionEmailSubject    :: !Template
    , deletionEmailBodyText   :: !Template
    , deletionEmailBodyHtml   :: !Template
    , deletionEmailSender     :: !Email
    , deletionEmailSenderName :: !Text
    }

data InvitationEmailTemplate = InvitationEmailTemplate
    { invitationEmailUrl        :: !Template
    , invitationEmailSubject    :: !Template
    , invitationEmailBodyText   :: !Template
    , invitationEmailBodyHtml   :: !Template
    , invitationEmailSender     :: !Email
    , invitationEmailSenderName :: !Text
    }

data InvitationSmsTemplate = InvitationSmsTemplate
    { invitationSmsUrl       :: !Template
    , invitationSmsText      :: !Template
    , invitationSmsSender    :: !Text
    }

data PasswordResetEmailTemplate = PasswordResetEmailTemplate
    { passwordResetEmailUrl        :: !Template
    , passwordResetEmailSubject    :: !Template
    , passwordResetEmailBodyText   :: !Template
    , passwordResetEmailBodyHtml   :: !Template
    , passwordResetEmailSender     :: !Email
    , passwordResetEmailSenderName :: !Text
    }

data PasswordResetSmsTemplate = PasswordResetSmsTemplate
    { passwordResetSmsText   :: !Template
    , passwordResetSmsSender :: !Text
    }

data LoginSmsTemplate = LoginSmsTemplate
    { loginSmsUrl    :: !Template
    , loginSmsText   :: !Template
    , loginSmsSender :: !Text
    }

data LoginCallTemplate = LoginCallTemplate
    { loginCallText   :: !Template
    }

data DeletionSmsTemplate = DeletionSmsTemplate
    { deletionSmsUrl    :: !Template
    , deletionSmsText   :: !Template
    , deletionSmsSender :: !Text
    }

data NewClientEmailTemplate = NewClientEmailTemplate
    { newClientEmailSubject    :: !Template
    , newClientEmailBodyText   :: !Template
    , newClientEmailBodyHtml   :: !Template
    , newClientEmailSender     :: !Email
    , newClientEmailSenderName :: !Text
    }

loadUserTemplates :: Opts -> IO (Localised UserTemplates)
loadUserTemplates o = readLocalesDir defLocale templateDir $ \fp ->
    UserTemplates
        <$> (ActivationSmsTemplate smsActivationUrl
                <$> readTemplate (fp <> "/sms/activation.txt")
                <*> pure (optTwilioSender o))
        <*> (ActivationCallTemplate
                <$> readTemplate (fp <> "/call/activation.txt"))
        <*> (ActivationEmailTemplate activationUrl
                <$> readTemplate (fp <> "/email/activation-subject.txt")
                <*> readTemplate (fp <> "/email/activation.txt")
                <*> readTemplate (fp <> "/email/activation.html")
                <*> pure (optEmailSender o)
                <*> readText (fp <> "/email/sender.txt"))
        <*> (ActivationEmailTemplate activationUrl
                <$> readTemplate (fp <> "/email/update-subject.txt")
                <*> readTemplate (fp <> "/email/update.txt")
                <*> readTemplate (fp <> "/email/update.html")
                <*> pure (optEmailSender o)
                <*> readText (fp <> "/email/sender.txt"))
        <*> (TeamActivationEmailTemplate teamActivationUrl
                <$> readTemplate (fp <> "/email/team-activation-subject.txt")
                <*> readTemplate (fp <> "/email/team-activation.txt")
                <*> readTemplate (fp <> "/email/team-activation.html")
                <*> pure (optEmailSender o)
                <*> readText (fp <> "/email/sender.txt"))
        <*> (PasswordResetSmsTemplate
                <$> readTemplate (fp <> "/sms/password-reset.txt")
                <*> pure (optTwilioSender o))
        <*> (PasswordResetEmailTemplate passwordResetUrl
                <$> readTemplate (fp <> "/email/password-reset-subject.txt")
                <*> readTemplate (fp <> "/email/password-reset.txt")
                <*> readTemplate (fp <> "/email/password-reset.html")
                <*> pure (optEmailSender o)
                <*> readText (fp <> "/email/sender.txt"))
        <*> (InvitationEmailTemplate invitationUrl
                <$> readTemplate (fp <> "/email/invitation-subject.txt")
                <*> readTemplate (fp <> "/email/invitation.txt")
                <*> readTemplate (fp <> "/email/invitation.html")
                <*> pure (optEmailSender o)
                <*> readText (fp <> "/email/sender.txt"))
        <*> (InvitationSmsTemplate invitationUrl
                <$> readTemplate (fp <> "/sms/invitation.txt")
                <*> pure (optTwilioSender o))
        <*> (LoginSmsTemplate smsActivationUrl
                <$> readTemplate (fp <> "/sms/login.txt")
                <*> pure (optTwilioSender o))
        <*> (LoginCallTemplate
                <$> readTemplate (fp <> "/call/login.txt"))
        <*> (DeletionSmsTemplate deletionUserUrl
                <$> readTemplate (fp <> "/sms/deletion.txt")
                <*> pure (optTwilioSender o))
        <*> (DeletionEmailTemplate deletionUserUrl
                <$> readTemplate (fp <> "/email/deletion-subject.txt")
                <*> readTemplate (fp <> "/email/deletion.txt")
                <*> readTemplate (fp <> "/email/deletion.html")
                <*> pure (optEmailSender o)
                <*> readText (fp <> "/email/sender.txt"))
        <*> (NewClientEmailTemplate
                <$> readTemplate (fp <> "/email/new-client-subject.txt")
                <*> readTemplate (fp <> "/email/new-client.txt")
                <*> readTemplate (fp <> "/email/new-client.html")
                <*> pure (optEmailSender o)
                <*> readText (fp <> "/email/sender.txt"))
  where
    smsActivationUrl  = template . Text.decodeLatin1 $ optUserSmsActivationUrl  o
    activationUrl     = template . Text.decodeLatin1 $ optUserActivationUrl     o
    teamActivationUrl = template . Text.decodeLatin1 $ optUserTeamActivationUrl o
    passwordResetUrl  = template . Text.decodeLatin1 $ optUserPasswordResetUrl  o
    invitationUrl     = template . Text.decodeLatin1 $ optUserInvitationUrl     o
    deletionUserUrl   = template . Text.decodeLatin1 $ optUserDeletionUserUrl   o

    defLocale = setDefaultLocale (optSettings o)
    templateDir = optTemplateDir o <> "/user"

