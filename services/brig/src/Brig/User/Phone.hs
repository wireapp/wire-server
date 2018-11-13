{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Brig.User.Phone
    ( ActivationSms (..)
    , sendActivationSms

    , PasswordResetSms (..)
    , sendPasswordResetSms

    , LoginSms (..)
    , sendLoginSms

    , ActivationCall (..)
    , sendActivationCall

    , LoginCall (..)
    , sendLoginCall

    , DeletionSms (..)
    , sendDeletionSms

      -- * Re-exports
    , validatePhone
    ) where

import Imports
import Brig.App
import Brig.Phone
import Brig.User.Template
import Brig.Types.Activation
import Brig.Types.User
import Brig.Types.User.Auth (LoginCode (..))
import Data.Range
import Data.Text.Lazy (toStrict)

import qualified Brig.Types.Code as Code
import qualified Data.Text.Ascii as Ascii
import qualified Data.Text       as Text
import qualified Ropes.Nexmo     as Nexmo

sendActivationSms :: Phone -> ActivationPair -> Maybe Locale -> AppIO ()
sendActivationSms to (_, c) loc = do
    (loc', tpl) <- userTemplates loc
    sendSms loc' $ renderActivationSms (ActivationSms to c) (activationSms tpl)

sendPasswordResetSms :: Phone -> PasswordResetPair -> Maybe Locale -> AppIO ()
sendPasswordResetSms to (_, c) loc = do
    (loc', tpl) <- userTemplates loc
    sendSms loc' $ renderPasswordResetSms (PasswordResetSms to c) (passwordResetSms tpl)

sendLoginSms :: Phone -> LoginCode -> Maybe Locale -> AppIO ()
sendLoginSms to code loc = do
    (loc', tpl) <- userTemplates loc
    sendSms loc' $ renderLoginSms (LoginSms to code) (loginSms tpl)

sendDeletionSms :: Phone -> Code.Key -> Code.Value -> Locale -> AppIO ()
sendDeletionSms to key code loc = do
    (loc', tpl) <- userTemplates (Just loc)
    sendSms loc' $ renderDeletionSms (DeletionSms to key code) (deletionSms tpl)

sendActivationCall :: Phone -> ActivationPair -> Maybe Locale -> AppIO ()
sendActivationCall to (_, c) loc = do
    (loc', tpl) <- userTemplates loc
    sendCall $ renderActivationCall (ActivationCall to c) (activationCall tpl) loc'

sendLoginCall :: Phone -> LoginCode -> Maybe Locale -> AppIO ()
sendLoginCall to c loc = do
    (loc', tpl) <- userTemplates loc
    sendCall $ renderLoginCall (LoginCall to c) (loginCall tpl) loc'

-------------------------------------------------------------------------------
-- Activation SMS

data ActivationSms = ActivationSms
    { actSmsTo   :: !Phone
    , actSmsCode :: !ActivationCode
    }

renderActivationSms :: ActivationSms -> ActivationSmsTemplate -> SMSMessage
renderActivationSms ActivationSms{..} (ActivationSmsTemplate url t from) =
    SMSMessage from (fromPhone actSmsTo) (toStrict $ renderText t replace)
  where
    replace "code" = codeText
    replace "url"  = renderSmsActivationUrl url codeText
    replace x      = x

    codeText = Ascii.toText (fromActivationCode actSmsCode)

-------------------------------------------------------------------------------
-- Password Reset SMS

data PasswordResetSms = PasswordResetSms
    { pwrSmsTo   :: !Phone
    , pwrSmsCode :: !PasswordResetCode
    }

renderPasswordResetSms :: PasswordResetSms -> PasswordResetSmsTemplate -> SMSMessage
renderPasswordResetSms PasswordResetSms{..} (PasswordResetSmsTemplate t from) =
    SMSMessage from (fromPhone pwrSmsTo) (toStrict $ renderText t replace)
  where
    replace "code" = Ascii.toText (fromPasswordResetCode pwrSmsCode)
    replace x      = x

-------------------------------------------------------------------------------
-- Login SMS

data LoginSms = LoginSms
    { loginSmsTo   :: !Phone
    , loginSmsCode :: !LoginCode
    }

renderLoginSms :: LoginSms -> LoginSmsTemplate -> SMSMessage
renderLoginSms LoginSms{..} (LoginSmsTemplate url t from) =
    SMSMessage from (fromPhone loginSmsTo) (toStrict $ renderText t replace)
  where
    replace "code" = fromLoginCode loginSmsCode
    replace "url"  = renderSmsActivationUrl url (fromLoginCode loginSmsCode)
    replace x      = x

-------------------------------------------------------------------------------
-- Deletion SMS

data DeletionSms = DeletionSms
    { delSmsTo   :: !Phone
    , delSmsKey  :: !Code.Key
    , delSmsCode :: !Code.Value
    }

renderDeletionSms :: DeletionSms -> DeletionSmsTemplate -> SMSMessage
renderDeletionSms DeletionSms{..} (DeletionSmsTemplate url txt from) =
    SMSMessage from (fromPhone delSmsTo) (toStrict $ renderText txt replace1)
  where
    replace1 "code" = Ascii.toText (fromRange (Code.asciiValue delSmsCode))
    replace1 "url"  = toStrict (renderText url replace2)
    replace1 x      = x

    replace2 "key"  = Ascii.toText (fromRange (Code.asciiKey delSmsKey))
    replace2 "code" = Ascii.toText (fromRange (Code.asciiValue delSmsCode))
    replace2 x      = x

-------------------------------------------------------------------------------
-- Activation Call

data ActivationCall = ActivationCall
    { actCallTo   :: !Phone
    , actCallCode :: !ActivationCode
    }

renderActivationCall :: ActivationCall -> ActivationCallTemplate -> Locale -> Nexmo.Call
renderActivationCall ActivationCall{..} (ActivationCallTemplate t) loc =
    Nexmo.Call Nothing
               (fromPhone actCallTo)
               (toStrict $ renderText t replace)
               (Just . Text.toLower $ locToText loc)
               (Just 1)
  where
    replace "code" = toPinPrompt $ Ascii.toText (fromActivationCode actCallCode)
    replace x      = x

-------------------------------------------------------------------------------
-- Login Call

data LoginCall = LoginCall
    { loginCallTo   :: !Phone
    , loginCallCode :: !LoginCode
    }

renderLoginCall :: LoginCall -> LoginCallTemplate -> Locale -> Nexmo.Call
renderLoginCall LoginCall{..} (LoginCallTemplate t) loc =
    Nexmo.Call Nothing
               (fromPhone loginCallTo)
               (toStrict $ renderText t replace)
               (Just . Text.toLower $ locToText loc)
               (Just 1)
  where
    replace "code" = toPinPrompt $ fromLoginCode loginCallCode
    replace x      = x

-- Common Prompt rendering

toPinPrompt :: Text -> Text
toPinPrompt = Text.intercalate "<break time=\"750ms\"/>" . Text.chunksOf 1

-- Common URL rendering

renderSmsActivationUrl :: Template -> Text -> Text
renderSmsActivationUrl t c =
    toStrict $ renderText t replace
  where
    replace "code" = c
    replace x      = x
