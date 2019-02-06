{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Brig.User.Email
    ( sendActivationMail
    , sendVerificationMail
    , sendTeamActivationMail
    , sendPasswordResetMail
    , sendDeletionEmail
    , sendNewClientEmail

      -- * Re-exports
    , validateEmail
    ) where

import Imports
import Brig.App
import Brig.Email
import Brig.Locale (formatDateTime, timeLocale)
import Brig.User.Template
import Brig.Types
import Control.Lens (view)
import Data.Json.Util (fromUTCTimeMillis)
import Data.Range
import Data.Text.Lazy (toStrict)

import qualified Brig.Email      as Email
import qualified Brig.Types.Code as Code
import qualified Data.Text.Ascii as Ascii

sendVerificationMail :: Email -> ActivationPair -> Maybe Locale -> AppIO ()
sendVerificationMail to pair loc = do
    tpl <- verificationEmail . snd <$> userTemplates loc
    let mail = VerificationEmail to pair
    Email.sendMail $ renderVerificationMail mail tpl

sendActivationMail :: Email -> Name -> ActivationPair -> Maybe Locale -> Maybe UserIdentity -> AppIO ()
sendActivationMail to name pair loc ident = do
    tpl <- selectTemplate . snd <$> userTemplates loc
    let mail = ActivationEmail to name pair
    def <- view tplBranding
    Email.sendMail $ renderActivationMail mail tpl def
  where
    selectTemplate =
        if isNothing ident
            then activationEmail
            else activationEmailUpdate

sendPasswordResetMail :: Email -> PasswordResetPair -> Maybe Locale -> AppIO ()
sendPasswordResetMail to pair loc = do
    tpl <- passwordResetEmail . snd <$> userTemplates loc
    let mail = PasswordResetEmail to pair
    Email.sendMail $ renderPwResetMail mail tpl

sendDeletionEmail :: Name -> Email -> Code.Key -> Code.Value -> Locale -> AppIO ()
sendDeletionEmail name email key code locale = do
    tpl <- deletionEmail . snd <$> userTemplates (Just locale)
    let mail = (DeletionEmail email name key code)
    Email.sendMail $ renderDeletionEmail mail tpl

sendNewClientEmail :: Name -> Email -> Client -> Locale -> AppIO ()
sendNewClientEmail name email client locale = do
    tpl <- newClientEmail . snd <$> userTemplates (Just locale)
    let mail = (NewClientEmail locale email name client)
    Email.sendMail $ renderNewClientEmail mail tpl

sendTeamActivationMail :: Email -> Name -> ActivationPair -> Maybe Locale -> Text -> AppIO ()
sendTeamActivationMail to name pair loc team = do
    tpl <- teamActivationEmail . snd <$> userTemplates loc
    let mail = TeamActivationEmail to name team pair
    Email.sendMail $ renderTeamActivationMail mail tpl

-------------------------------------------------------------------------------
-- New Client Email

data NewClientEmail = NewClientEmail
    { nclLocale :: !Locale
    , nclTo     :: !Email
    , nclName   :: !Name
    , nclClient :: !Client
    }

renderNewClientEmail :: NewClientEmail -> NewClientEmailTemplate -> Mail
renderNewClientEmail NewClientEmail{..} NewClientEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", toStrict subj)
                        , ("X-Zeta-Purpose", "NewDevice")
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    from = Address (Just newClientEmailSenderName) (fromEmail newClientEmailSender)
    to   = mkMimeAddress nclName nclTo
    txt  = renderText newClientEmailBodyText replace
    html = renderHtml newClientEmailBodyHtml replace
    subj = renderText newClientEmailSubject  replace

    replace "name"  = fromName nclName
    replace "label" = fromMaybe "N/A" (clientLabel nclClient)
    replace "model" = fromMaybe "N/A" (clientModel nclClient)
    replace "date"  = formatDateTime "%A %e %B %Y, %H:%M - %Z"
                                     (timeLocale nclLocale)
                                     (fromUTCTimeMillis $ clientTime nclClient)
    replace x       = x

-------------------------------------------------------------------------------
-- Deletion Email

data DeletionEmail = DeletionEmail
    { delTo   :: !Email
    , delName :: !Name
    , delKey  :: !Code.Key
    , delCode :: !Code.Value
    }

renderDeletionEmail :: DeletionEmail -> DeletionEmailTemplate -> Mail
renderDeletionEmail DeletionEmail{..} DeletionEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", toStrict subj)
                        , ("X-Zeta-Purpose", "Delete")
                        , ("X-Zeta-Key", key)
                        , ("X-Zeta-Code", code)
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    from = Address (Just deletionEmailSenderName) (fromEmail deletionEmailSender)
    to   = mkMimeAddress delName delTo

    txt  = renderText deletionEmailBodyText replace1
    html = renderHtml deletionEmailBodyHtml replace1
    subj = renderText deletionEmailSubject  replace1

    key  = Ascii.toText (fromRange (Code.asciiKey delKey))
    code = Ascii.toText (fromRange (Code.asciiValue delCode))

    replace1 "url"   = toStrict (renderText deletionEmailUrl replace2)
    replace1 "email" = fromEmail delTo
    replace1 "name"  = fromName delName
    replace1 x       = x

    replace2 "key"  = key
    replace2 "code" = code
    replace2 x      = x

-------------------------------------------------------------------------------
-- Verification Email

data VerificationEmail = VerificationEmail
    { vfTo   :: !Email
    , vfPair :: !ActivationPair
    }

renderVerificationMail :: VerificationEmail -> VerificationEmailTemplate -> Mail
renderVerificationMail VerificationEmail{..} VerificationEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", toStrict subj)
                        , ("X-Zeta-Purpose", "Verification")
                        , ("X-Zeta-Code", Ascii.toText code)
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    (ActivationKey _, ActivationCode code) = vfPair

    from = Address (Just verificationEmailSenderName) (fromEmail verificationEmailSender)
    to   = Address Nothing (fromEmail vfTo)
    txt  = renderText verificationEmailBodyText replace
    html = renderHtml verificationEmailBodyHtml replace
    subj = renderText verificationEmailSubject  replace

    replace "code"  = Ascii.toText code
    replace "email" = fromEmail vfTo
    replace x       = x

-------------------------------------------------------------------------------
-- Activation Email

data ActivationEmail = ActivationEmail
    { acmTo   :: !Email
    , acmName :: !Name
    , acmPair :: !ActivationPair
    }

renderActivationMail :: ActivationEmail -> ActivationEmailTemplate -> (Text -> Text) -> Mail
renderActivationMail ActivationEmail{..} ActivationEmailTemplate{..} branding =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", toStrict subj)
                        , ("X-Zeta-Purpose", "Activation")
                        , ("X-Zeta-Key", Ascii.toText key)
                        , ("X-Zeta-Code", Ascii.toText code)
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    (ActivationKey key, ActivationCode code) = acmPair

    from = Address (Just activationEmailSenderName) (fromEmail activationEmailSender)
    to   = mkMimeAddress acmName acmTo
    txt  = renderText activationEmailBodyText replace
    html = renderHtml activationEmailBodyHtml replace
    subj = renderText activationEmailSubject  replace

    replace "url"   = renderActivationUrl activationEmailUrl acmPair
    replace "email" = fromEmail acmTo
    replace "name"  = fromName acmName
    replace x       = branding x

renderActivationUrl :: Template -> ActivationPair -> Text
renderActivationUrl t (ActivationKey k, ActivationCode c) =
    toStrict $ renderText t replace
  where
    replace "key"  = Ascii.toText k
    replace "code" = Ascii.toText c
    replace x      = x

-------------------------------------------------------------------------------
-- Team Activation Email

data TeamActivationEmail = TeamActivationEmail
    { tacmTo       :: !Email
    , tacmName     :: !Name
    , tacmTeamName :: !Text
    , tacmPair     :: !ActivationPair
    }

renderTeamActivationMail :: TeamActivationEmail -> TeamActivationEmailTemplate -> Mail
renderTeamActivationMail TeamActivationEmail{..} TeamActivationEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", toStrict subj)
                        , ("X-Zeta-Purpose", "Activation")
                        , ("X-Zeta-Key", Ascii.toText key)
                        , ("X-Zeta-Code", Ascii.toText code)
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    (ActivationKey key, ActivationCode code) = tacmPair

    from = Address (Just teamActivationEmailSenderName) (fromEmail teamActivationEmailSender)
    to   = mkMimeAddress tacmName tacmTo
    txt  = renderText teamActivationEmailBodyText replace
    html = renderHtml teamActivationEmailBodyHtml replace
    subj = renderText teamActivationEmailSubject replace

    replace "url"   = renderActivationUrl teamActivationEmailUrl tacmPair
    replace "email" = fromEmail tacmTo
    replace "name"  = fromName tacmName
    replace "team"  = tacmTeamName
    replace x       = x

-------------------------------------------------------------------------------
-- Password Reset Email

data PasswordResetEmail = PasswordResetEmail
    { pwrTo   :: !Email
    , pwrPair :: !PasswordResetPair
    }

renderPwResetMail :: PasswordResetEmail -> PasswordResetEmailTemplate -> Mail
renderPwResetMail PasswordResetEmail{..} PasswordResetEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", toStrict subj)
                        , ("X-Zeta-Purpose", "PasswordReset")
                        , ("X-Zeta-Key", Ascii.toText key)
                        , ("X-Zeta-Code", Ascii.toText code)
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    (PasswordResetKey key, PasswordResetCode code) = pwrPair

    from = Address (Just passwordResetEmailSenderName) (fromEmail passwordResetEmailSender)
    to   = Address Nothing (fromEmail pwrTo)
    txt  = renderText passwordResetEmailBodyText replace
    html = renderHtml passwordResetEmailBodyHtml replace
    subj = renderText passwordResetEmailSubject  replace

    replace "url" = renderPwResetUrl passwordResetEmailUrl pwrPair
    replace x     = x

renderPwResetUrl :: Template -> PasswordResetPair -> Text
renderPwResetUrl t (PasswordResetKey k, PasswordResetCode c) =
    toStrict $ renderText t replace
  where
    replace "key"  = Ascii.toText k
    replace "code" = Ascii.toText c
    replace x      = x
