{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.Activation
  ( -- * ActivationTarget
    ActivationTarget (..),
    ActivationKey (..),

    -- * ActivationCode
    ActivationCode (..),

    -- * Activate
    Activate (..),
    ActivationResponse (..),

    -- * SendActivationCode
    SendActivationCode (..),

    -- * Swagger
    modelActivate,
    modelSendActivationCode,
    modelActivationResponse,
  )
where

import Data.Aeson
import Data.ByteString.Conversion
import Data.Json.Util ((#))
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii
import Imports
import Wire.API.User.Identity
import Wire.API.User.Profile

--------------------------------------------------------------------------------
-- ActivationTarget

-- | The target of an activation request.
data ActivationTarget
  = -- | An opaque key for some email or phone number awaiting activation.
    ActivateKey ActivationKey
  | -- | A known phone number awaiting activation.
    ActivatePhone Phone
  | -- | A known email address awaiting activation.
    ActivateEmail Email

instance ToByteString ActivationTarget where
  builder (ActivateKey k) = builder k
  builder (ActivateEmail e) = builder e
  builder (ActivatePhone p) = builder p

-- | An opaque identifier of a 'UserKey' awaiting activation.
newtype ActivationKey = ActivationKey
  {fromActivationKey :: AsciiBase64Url}
  deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON, Generic)

--------------------------------------------------------------------------------
-- ActivationCode

-- | A random code for use with an 'ActivationKey' that is usually transmitted
-- out-of-band, e.g. via email or sms.
newtype ActivationCode = ActivationCode
  {fromActivationCode :: AsciiBase64Url}
  deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON, Generic)

--------------------------------------------------------------------------------
-- Activate

-- | Data for an activation request.
data Activate = Activate
  { activateTarget :: ActivationTarget,
    activateCode :: ActivationCode,
    activateDryrun :: Bool
  }

modelActivate :: Doc.Model
modelActivate = Doc.defineModel "Activate" $ do
  Doc.description "Data for an activation request."
  Doc.property "key" Doc.string' $ do
    Doc.description "An opaque key to activate, as it was sent by the API."
    Doc.optional
  Doc.property "email" Doc.string' $ do
    Doc.description "A known email address to activate."
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "A known phone number to activate."
    Doc.optional
  Doc.property "code" Doc.string' $
    Doc.description "The activation code."
  Doc.property "label" Doc.string' $ do
    Doc.description
      "An optional label to associate with the access cookie, \
      \if one is granted during account activation."
    Doc.optional
  Doc.property "dryrun" Doc.bool' $ do
    Doc.description
      "Whether to perform a dryrun, i.e. to only check whether \
      \activation would succeed. Dry-runs never issue access \
      \cookies or tokens on success but failures still count \
      \towards the maximum failure count."
    Doc.optional

instance ToJSON Activate where
  toJSON (Activate k c d) =
    object
      [key k, "code" .= c, "dryrun" .= d]
    where
      key (ActivateKey ak) = "key" .= ak
      key (ActivateEmail e) = "email" .= e
      key (ActivatePhone p) = "phone" .= p

instance FromJSON Activate where
  parseJSON = withObject "Activation" $ \o ->
    Activate <$> key o
      <*> o .: "code"
      <*> o .:? "dryrun" .!= False
    where
      key o =
        (ActivateKey <$> o .: "key")
          <|> (ActivateEmail <$> o .: "email")
          <|> (ActivatePhone <$> o .: "phone")

-- | Information returned as part of a successful activation.
data ActivationResponse = ActivationResponse
  { -- | The activated / verified user identity.
    activatedIdentity :: UserIdentity,
    -- | Whether this is the first verified identity of the account.
    activatedFirst :: Bool
  }

modelActivationResponse :: Doc.Model
modelActivationResponse = Doc.defineModel "ActivationResponse" $ do
  Doc.description "Response body of a successful activation request"
  Doc.property "email" Doc.string' $ do
    Doc.description "The email address that was activated."
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "The phone number that was activated."
    Doc.optional
  Doc.property "first" Doc.bool' $
    Doc.description "Whether this is the first successful activation (i.e. account activation)."

instance ToJSON ActivationResponse where
  toJSON (ActivationResponse ident first) =
    object $
      "email" .= emailIdentity ident
        # "phone" .= phoneIdentity ident
        # "first" .= first
        # []

instance FromJSON ActivationResponse where
  parseJSON = withObject "ActivationResponse" $ \o ->
    ActivationResponse <$> parseJSON (Object o)
      <*> o .:? "first" .!= False

--------------------------------------------------------------------------------
-- SendActivationCode

-- | Payload for a request to (re-)send an activation code
-- for a phone number or e-mail address. If a phone is used,
-- one can also request a call instead of SMS.
data SendActivationCode = SendActivationCode
  { saUserKey :: Either Email Phone,
    saLocale :: Maybe Locale,
    saCall :: Bool
  }

modelSendActivationCode :: Doc.Model
modelSendActivationCode = Doc.defineModel "SendActivationCode" $ do
  Doc.description
    "Data for requesting an email or phone activation code to be sent. \
    \One of 'email' or 'phone' must be present."
  Doc.property "email" Doc.string' $ do
    Doc.description "Email address to send the code to."
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "E.164 phone number to send the code to."
    Doc.optional
  Doc.property "locale" Doc.string' $ do
    Doc.description "Locale to use for the activation code template."
    Doc.optional
  Doc.property "voice_call" Doc.bool' $ do
    Doc.description "Request the code with a call instead (default is SMS)."
    Doc.optional

instance FromJSON SendActivationCode where
  parseJSON = withObject "SendActivationCode" $ \o -> do
    e <- o .:? "email"
    p <- o .:? "phone"
    SendActivationCode <$> key e p
      <*> o .:? "locale"
      <*> o .:? "voice_call" .!= False
    where
      key (Just _) (Just _) = fail "Only one of 'email' or 'phone' allowed."
      key Nothing Nothing = fail "One of 'email' or 'phone' required."
      key (Just e) Nothing = return $ Left e
      key Nothing (Just p) = return $ Right p
