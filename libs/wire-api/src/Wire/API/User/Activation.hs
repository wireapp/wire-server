{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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
    modelSendActivationCode,
    modelActivationResponse,
  )
where

import Control.Lens ((?~))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Conversion
import Data.Data (Proxy (Proxy))
import Data.Json.Util ((#))
import Data.Schema as Schema (Schema (..), ToSchema (..), description)
import qualified Data.Schema as Schema
import Data.Swagger (ToParamSchema)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii
import Data.Tuple.Extra (fst3, snd3, thd3)
import Imports
import Servant (FromHttpApiData (..))
import Wire.API.User.Identity
import Wire.API.User.Profile
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

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
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ActivationTarget)

instance ToByteString ActivationTarget where
  builder (ActivateKey k) = builder k
  builder (ActivateEmail e) = builder e
  builder (ActivatePhone p) = builder p

-- | An opaque identifier of a 'UserKey' awaiting activation.
newtype ActivationKey = ActivationKey
  {fromActivationKey :: AsciiBase64Url}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToSchema, ToByteString, FromByteString, ToJSON, FromJSON, Arbitrary)

instance ToParamSchema ActivationKey where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance FromHttpApiData ActivationKey where
  parseUrlPiece = fmap ActivationKey . parseUrlPiece

maybeActivationKeyObjectSchema :: Schema.ObjectSchemaP Schema.SwaggerDoc (Maybe ActivationKey, Maybe Phone, Maybe Email) ActivationTarget
maybeActivationKeyObjectSchema =
  Schema.withParser activationKeyTupleObjectSchema maybeActivationKeyTargetFromTuple
  where
    activationKeyTupleObjectSchema :: Schema.ObjectSchema Schema.SwaggerDoc (Maybe ActivationKey, Maybe Phone, Maybe Email)
    activationKeyTupleObjectSchema =
      (,,)
        <$> fst3 Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "key" keyDocs Schema.schema)
        <*> snd3 Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "phone" phoneDocs Schema.schema)
        <*> thd3 Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "email" emailDocs Schema.schema)
      where
        keyDocs = description ?~ "An opaque key to activate, as it was sent by the API."
        phoneDocs = description ?~ "A known phone number to activate."
        emailDocs = description ?~ "A known email address to activate."

    maybeActivationKeyTargetFromTuple :: (Maybe ActivationKey, Maybe Phone, Maybe Email) -> Parser ActivationTarget
    maybeActivationKeyTargetFromTuple = \case
      (Just key, _, _) -> pure $ ActivateKey key
      (_, _, Just email) -> pure $ ActivateEmail email
      (_, Just phone, _) -> pure $ ActivatePhone phone
      _ -> fail "key, email or phone must be present"

maybeActivationTargetToTuple :: ActivationTarget -> (Maybe ActivationKey, Maybe Phone, Maybe Email)
maybeActivationTargetToTuple = \case
  ActivateKey key -> (Just key, Nothing, Nothing)
  ActivatePhone phone -> (Nothing, Just phone, Nothing)
  ActivateEmail email -> (Nothing, Nothing, Just email)

--------------------------------------------------------------------------------
-- ActivationCode

-- | A random code for use with an 'ActivationKey' that is usually transmitted
-- out-of-band, e.g. via email or sms.
-- FUTUREWORK(leif): rename to VerificationCode
newtype ActivationCode = ActivationCode
  {fromActivationCode :: AsciiBase64Url}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToByteString, FromByteString, ToSchema, Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema ActivationCode

instance ToParamSchema ActivationCode where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance FromHttpApiData ActivationCode where
  parseQueryParam = fmap ActivationCode . parseUrlPiece

--------------------------------------------------------------------------------
-- Activate

-- | Data for an activation request.
data Activate = Activate
  { activateTarget :: ActivationTarget,
    activateCode :: ActivationCode,
    activateDryrun :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Activate)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema Activate

instance ToSchema Activate where
  schema =
    Schema.objectWithDocModifier "Activate" objectDocs $
      Activate
        <$> (maybeActivationTargetToTuple . activateTarget) Schema..= maybeActivationKeyObjectSchema
        <*> activateCode Schema..= Schema.fieldWithDocModifier "code" codeDocs schema
        <*> activateDryrun Schema..= Schema.fieldWithDocModifier "dryrun" dryrunDocs schema
    where
      objectDocs = description ?~ "Data for an activation request."
      codeDocs = description ?~ "The activation code."
      dryrunDocs =
        description
          ?~ "Whether to perform a dryrun, i.e. to only check whether \
             \activation would succeed. Dry-runs never issue access \
             \cookies or tokens on success but failures still count \
             \towards the maximum failure count."

-- | Information returned as part of a successful activation.
data ActivationResponse = ActivationResponse
  { -- | The activated / verified user identity.
    activatedIdentity :: UserIdentity,
    -- | Whether this is the first verified identity of the account.
    activatedFirst :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ActivationResponse)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema ActivationResponse

instance ToSchema ActivationResponse where
  schema =
    Schema.objectWithDocModifier "ActivationResponse" (description ?~ "Response body of a successful activation request") $
      ActivationResponse
        <$> activatedIdentity Schema..= userIdentityObjectSchema
        <*> activatedFirst Schema..= (fromMaybe False <$> Schema.optFieldWithDocModifier "first" (description ?~ "Whether this is the first successful activation (i.e. account activation).") Schema.schema)

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
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SendActivationCode)

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

instance ToJSON SendActivationCode where
  toJSON (SendActivationCode userKey locale call) =
    object $
      either ("email" .=) ("phone" .=) userKey
        # "locale" .= locale
        # "voice_call" .= call
        # []

instance FromJSON SendActivationCode where
  parseJSON = withObject "SendActivationCode" $ \o -> do
    e <- o .:? "email"
    p <- o .:? "phone"
    SendActivationCode
      <$> key e p
      <*> o .:? "locale"
      <*> o .:? "voice_call" .!= False
    where
      key (Just _) (Just _) = fail "Only one of 'email' or 'phone' allowed."
      key Nothing Nothing = fail "One of 'email' or 'phone' required."
      key (Just e) Nothing = pure $ Left e
      key Nothing (Just p) = pure $ Right p
