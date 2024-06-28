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

module Brig.User.Phone
  ( ActivationSms (..),
    sendActivationSms,
    PasswordResetSms (..),
    sendPasswordResetSms,
    LoginSms (..),
    sendLoginSms,
    ActivationCall (..),
    sendActivationCall,
    LoginCall (..),
    sendLoginCall,
    DeletionSms (..),
    sendDeletionSms,

    -- * Re-exports
    validatePhone,
  )
where

import Brig.App
import Brig.Phone
import Brig.Template
import Brig.Types.Activation
import Brig.Types.User
import Brig.User.Template
import Cassandra (MonadClient)
import Control.Lens (view)
import Control.Monad.Catch
import Data.Code qualified as Code
import Data.Range
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Data.Text.Lazy (toStrict)
import Imports
import Prometheus (MonadMonitor)
import Ropes.Nexmo qualified as Nexmo
import System.Logger.Class qualified as Log
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Password
import Wire.EmailSmsSubsystem.Template (TemplateBranding, renderTextWithBranding)

sendActivationSms ::
  ( MonadClient m,
    MonadReader Env m,
    MonadCatch m,
    Log.MonadLogger m,
    MonadMonitor m
  ) =>
  Phone ->
  ActivationPair ->
  Maybe Locale ->
  m ()
sendActivationSms to (_, c) loc = do
  branding <- view templateBranding
  (loc', tpl) <- userTemplates loc
  sendSms loc' $ renderActivationSms (ActivationSms to c) (activationSms tpl) branding

sendPasswordResetSms ::
  ( MonadClient m,
    MonadReader Env m,
    MonadCatch m,
    Log.MonadLogger m,
    MonadMonitor m
  ) =>
  Phone ->
  PasswordResetPair ->
  Maybe Locale ->
  m ()
sendPasswordResetSms to (_, c) loc = do
  branding <- view templateBranding
  (loc', tpl) <- userTemplates loc
  sendSms loc' $ renderPasswordResetSms (PasswordResetSms to c) (passwordResetSms tpl) branding

sendLoginSms ::
  ( MonadClient m,
    MonadReader Env m,
    MonadCatch m,
    Log.MonadLogger m,
    MonadMonitor m
  ) =>
  Phone ->
  LoginCode ->
  Maybe Locale ->
  m ()
sendLoginSms to code loc = do
  branding <- view templateBranding
  (loc', tpl) <- userTemplates loc
  sendSms loc' $ renderLoginSms (LoginSms to code) (loginSms tpl) branding

sendDeletionSms ::
  ( MonadClient m,
    MonadReader Env m,
    MonadCatch m,
    Log.MonadLogger m,
    MonadMonitor m
  ) =>
  Phone ->
  Code.Key ->
  Code.Value ->
  Locale ->
  m ()
sendDeletionSms to key code loc = do
  branding <- view templateBranding
  (loc', tpl) <- userTemplates (Just loc)
  sendSms loc' $ renderDeletionSms (DeletionSms to key code) (deletionSms tpl) branding

sendActivationCall ::
  ( MonadClient m,
    MonadReader Env m,
    Log.MonadLogger m,
    MonadMonitor m
  ) =>
  Phone ->
  ActivationPair ->
  Maybe Locale ->
  m ()
sendActivationCall to (_, c) loc = do
  branding <- view templateBranding
  (loc', tpl) <- userTemplates loc
  sendCall $ renderActivationCall (ActivationCall to c) (activationCall tpl) loc' branding

sendLoginCall ::
  ( MonadClient m,
    MonadReader Env m,
    Log.MonadLogger m,
    MonadMonitor m
  ) =>
  Phone ->
  LoginCode ->
  Maybe Locale ->
  m ()
sendLoginCall to c loc = do
  branding <- view templateBranding
  (loc', tpl) <- userTemplates loc
  sendCall $ renderLoginCall (LoginCall to c) (loginCall tpl) loc' branding

-------------------------------------------------------------------------------
-- Activation SMS

data ActivationSms = ActivationSms
  { actSmsTo :: !Phone,
    actSmsCode :: !ActivationCode
  }

renderActivationSms :: ActivationSms -> ActivationSmsTemplate -> TemplateBranding -> SMSMessage
renderActivationSms ActivationSms {..} (ActivationSmsTemplate url t from) branding =
  SMSMessage from (fromPhone actSmsTo) (toStrict $ renderTextWithBranding t replace branding)
  where
    replace "code" = codeText
    replace "url" = renderSmsActivationUrl url codeText
    replace x = x
    codeText = Ascii.toText (fromActivationCode actSmsCode)

-------------------------------------------------------------------------------
-- Password Reset SMS

data PasswordResetSms = PasswordResetSms
  { pwrSmsTo :: !Phone,
    pwrSmsCode :: !PasswordResetCode
  }

renderPasswordResetSms :: PasswordResetSms -> PasswordResetSmsTemplate -> TemplateBranding -> SMSMessage
renderPasswordResetSms PasswordResetSms {..} (PasswordResetSmsTemplate t from) branding =
  SMSMessage from (fromPhone pwrSmsTo) (toStrict $ renderTextWithBranding t replace branding)
  where
    replace "code" = Ascii.toText (fromPasswordResetCode pwrSmsCode)
    replace x = x

-------------------------------------------------------------------------------
-- Login SMS

data LoginSms = LoginSms
  { loginSmsTo :: !Phone,
    loginSmsCode :: !LoginCode
  }

renderLoginSms :: LoginSms -> LoginSmsTemplate -> TemplateBranding -> SMSMessage
renderLoginSms LoginSms {..} (LoginSmsTemplate url t from) branding =
  SMSMessage from (fromPhone loginSmsTo) (toStrict $ renderTextWithBranding t replace branding)
  where
    replace "code" = fromLoginCode loginSmsCode
    replace "url" = renderSmsActivationUrl url (fromLoginCode loginSmsCode)
    replace x = x

-------------------------------------------------------------------------------
-- Deletion SMS

data DeletionSms = DeletionSms
  { delSmsTo :: !Phone,
    delSmsKey :: !Code.Key,
    delSmsCode :: !Code.Value
  }

renderDeletionSms :: DeletionSms -> DeletionSmsTemplate -> TemplateBranding -> SMSMessage
renderDeletionSms DeletionSms {..} (DeletionSmsTemplate url txt from) branding =
  SMSMessage from (fromPhone delSmsTo) (toStrict $ renderTextWithBranding txt replace1 branding)
  where
    replace1 "code" = Ascii.toText (fromRange (Code.asciiValue delSmsCode))
    replace1 "url" = toStrict (renderText url replace2)
    replace1 x = x
    replace2 "key" = Ascii.toText (fromRange (Code.asciiKey delSmsKey))
    replace2 "code" = Ascii.toText (fromRange (Code.asciiValue delSmsCode))
    replace2 x = x

-------------------------------------------------------------------------------
-- Activation Call

data ActivationCall = ActivationCall
  { actCallTo :: !Phone,
    actCallCode :: !ActivationCode
  }

renderActivationCall :: ActivationCall -> ActivationCallTemplate -> Locale -> TemplateBranding -> Nexmo.Call
renderActivationCall ActivationCall {..} (ActivationCallTemplate t) loc branding =
  Nexmo.Call
    Nothing
    (fromPhone actCallTo)
    (toStrict $ renderTextWithBranding t replace branding)
    (Just . Text.toLower $ locToText loc)
    (Just 1)
  where
    replace "code" = toPinPrompt $ Ascii.toText (fromActivationCode actCallCode)
    replace x = x

-------------------------------------------------------------------------------
-- Login Call

data LoginCall = LoginCall
  { loginCallTo :: !Phone,
    loginCallCode :: !LoginCode
  }

renderLoginCall :: LoginCall -> LoginCallTemplate -> Locale -> TemplateBranding -> Nexmo.Call
renderLoginCall LoginCall {..} (LoginCallTemplate t) loc branding =
  Nexmo.Call
    Nothing
    (fromPhone loginCallTo)
    (toStrict $ renderTextWithBranding t replace branding)
    (Just . Text.toLower $ locToText loc)
    (Just 1)
  where
    replace "code" = toPinPrompt $ fromLoginCode loginCallCode
    replace x = x

-- Common Prompt rendering

toPinPrompt :: Text -> Text
toPinPrompt = Text.intercalate "<break time=\"750ms\"/>" . Text.chunksOf 1

-- Common URL rendering

renderSmsActivationUrl :: Template -> Text -> Text
renderSmsActivationUrl t c =
  toStrict $ renderText t replace
  where
    replace "code" = c
    replace x = x
