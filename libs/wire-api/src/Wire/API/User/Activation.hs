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
  )
where

import Cassandra qualified as C
import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.Aeson.Types (Parser)
import Data.ByteString.Conversion
import Data.Data (Proxy (Proxy))
import Data.OpenApi (ToParamSchema)
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text.Ascii
import Data.Tuple.Extra (fst3, snd3, thd3)
import Imports
import Servant (FromHttpApiData (..))
import Wire.API.Locale
import Wire.API.User.Identity
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
  deriving newtype (ToSchema, ToByteString, FromByteString, A.ToJSON, A.FromJSON, Arbitrary)

instance ToParamSchema ActivationKey where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance FromHttpApiData ActivationKey where
  parseUrlPiece = fmap ActivationKey . parseUrlPiece

deriving instance C.Cql ActivationKey

--------------------------------------------------------------------------------
-- ActivationCode

-- | A random code for use with an 'ActivationKey' that is usually transmitted
-- out-of-band, e.g. via email or sms.
-- FUTUREWORK(leif): rename to VerificationCode
newtype ActivationCode = ActivationCode
  {fromActivationCode :: AsciiBase64Url}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToByteString, FromByteString, ToSchema, Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema ActivationCode

instance ToParamSchema ActivationCode where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance FromHttpApiData ActivationCode where
  parseQueryParam = fmap ActivationCode . parseUrlPiece

deriving instance C.Cql ActivationCode

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
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema Activate

instance ToSchema Activate where
  schema =
    objectWithDocModifier "Activate" objectDocs $
      Activate
        <$> (maybeActivationTargetToTuple . activateTarget) .= maybeActivationTargetObjectSchema
        <*> activateCode .= fieldWithDocModifier "code" codeDocs schema
        <*> activateDryrun .= fieldWithDocModifier "dryrun" dryRunDocs schema
    where
      objectDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
      objectDocs = description ?~ "Data for an activation request."

      codeDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
      codeDocs = description ?~ "The activation code."

      dryRunDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
      dryRunDocs =
        description
          ?~ "At least one of key, email, or phone has to be present \
             \while key takes precedence over email, and email takes precedence over phone. \
             \Whether to perform a dryrun, i.e. to only check whether \
             \activation would succeed. Dry-runs never issue access \
             \cookies or tokens on success but failures still count \
             \towards the maximum failure count."

      maybeActivationTargetObjectSchema :: ObjectSchemaP SwaggerDoc (Maybe ActivationKey, Maybe Phone, Maybe Email) ActivationTarget
      maybeActivationTargetObjectSchema =
        withParser activationTargetTupleObjectSchema maybeActivationTargetTargetFromTuple
        where
          activationTargetTupleObjectSchema :: ObjectSchema SwaggerDoc (Maybe ActivationKey, Maybe Phone, Maybe Email)
          activationTargetTupleObjectSchema =
            (,,)
              <$> fst3 .= maybe_ (optFieldWithDocModifier "key" keyDocs schema)
              <*> snd3 .= maybe_ (optFieldWithDocModifier "phone" phoneDocs schema)
              <*> thd3 .= maybe_ (optFieldWithDocModifier "email" emailDocs schema)
            where
              keyDocs = description ?~ "An opaque key to activate, as it was sent by the API."
              phoneDocs = description ?~ "A known phone number to activate."
              emailDocs = description ?~ "A known email address to activate."

          maybeActivationTargetTargetFromTuple :: (Maybe ActivationKey, Maybe Phone, Maybe Email) -> Parser ActivationTarget
          maybeActivationTargetTargetFromTuple = \case
            (Just key, _, _) -> pure $ ActivateKey key
            (_, _, Just email) -> pure $ ActivateEmail email
            (_, Just phone, _) -> pure $ ActivatePhone phone
            _ -> fail "key, email or phone must be present"

      maybeActivationTargetToTuple :: ActivationTarget -> (Maybe ActivationKey, Maybe Phone, Maybe Email)
      maybeActivationTargetToTuple = \case
        ActivateKey key -> (Just key, Nothing, Nothing)
        ActivatePhone phone -> (Nothing, Just phone, Nothing)
        ActivateEmail email -> (Nothing, Nothing, Just email)

-- | Information returned as part of a successful activation.
data ActivationResponse = ActivationResponse
  { -- | The activated / verified user identity.
    activatedIdentity :: UserIdentity,
    -- | Whether this is the first verified identity of the account.
    activatedFirst :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ActivationResponse)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema ActivationResponse

instance ToSchema ActivationResponse where
  schema =
    objectWithDocModifier "ActivationResponse" (description ?~ "Response body of a successful activation request") $
      ActivationResponse
        <$> activatedIdentity .= userIdentityObjectSchema
        <*> activatedFirst .= (fromMaybe False <$> optFieldWithDocModifier "first" (description ?~ "Whether this is the first successful activation (i.e. account activation).") schema)

--------------------------------------------------------------------------------
-- SendActivationCode

-- | Payload for a request to (re-)send an activation code
-- for a phone number or e-mail address. If a phone is used,
-- one can also request a call instead of SMS.
data SendActivationCode = SendActivationCode
  { saUserKey :: Email,
    saLocale :: Maybe Locale,
    saCall :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SendActivationCode)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema SendActivationCode

instance ToSchema SendActivationCode where
  schema =
    objectWithDocModifier "SendActivationCode" objectDesc $
      SendActivationCode
        <$> (maybeUserKeyToTuple . saUserKey) .= userKeyObjectSchema
        <*> saLocale .= maybe_ (optFieldWithDocModifier "locale" (description ?~ "Locale to use for the activation code template.") schema)
        <*> saCall .= (fromMaybe False <$> optFieldWithDocModifier "voice_call" (description ?~ "Request the code with a call instead (default is SMS).") schema)
    where
      maybeUserKeyToTuple :: Email -> (Maybe Email, Maybe Phone)
      maybeUserKeyToTuple email = (Just email, Nothing)

      objectDesc :: NamedSwaggerDoc -> NamedSwaggerDoc
      objectDesc =
        description
          ?~ "Data for requesting an email activation code to be sent. \
             \'email' must be present. Phone activation code is not supported any more."

      userKeyObjectSchema :: ObjectSchemaP SwaggerDoc (Maybe Email, Maybe Phone) Email
      userKeyObjectSchema =
        withParser userKeyTupleObjectSchema maybeUserKeyFromTuple
        where
          userKeyTupleObjectSchema :: ObjectSchema SwaggerDoc (Maybe Email, Maybe Phone)
          userKeyTupleObjectSchema =
            (,)
              <$> fst .= maybe_ (optFieldWithDocModifier "email" emailDocs schema)
              <*> snd .= maybe_ (optFieldWithDocModifier "phone" phoneDocs schema)
            where
              emailDocs = description ?~ "Email address to send the code to."
              phoneDocs = description ?~ "Phone number based activation is not supported any more. This field is ignored."

          maybeUserKeyFromTuple :: (Maybe Email, Maybe Phone) -> Parser Email
          maybeUserKeyFromTuple = \case
            (Just _, Just _) -> fail "Only 'email' allowed. The phone option is not supported any more."
            (Just email, Nothing) -> pure email
            (Nothing, Just _) -> fail "The phone option is not supported any more."
            (Nothing, Nothing) -> fail "The 'email' field is required."
