{-# LANGUAGE DisambiguateRecordFields #-}
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

module Wire.API.User.Password
  ( NewPasswordReset (..),
    CompletePasswordReset (..),
    PasswordResetIdentity (..),
    PasswordResetKey (..),
    mkPasswordResetKey,
    PasswordResetCode (..),

    -- * deprecated
    PasswordReset (..),
  )
where

import Cassandra qualified as C
import Control.Lens ((?~))
import Crypto.Hash
import Data.Aeson qualified as A
import Data.Aeson.Types (Parser)
import Data.ByteArray qualified as ByteArray
import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.Id
import Data.Misc (PlainTextPassword8)
import Data.OpenApi qualified as S
import Data.OpenApi.ParamSchema
import Data.Proxy (Proxy (Proxy))
import Data.Range (Ranged (..))
import Data.Schema as Schema
import Data.Text.Ascii
import Data.Tuple.Extra (fst3, snd3, thd3)
import Imports
import Servant (FromHttpApiData (..))
import Wire.API.User.Identity
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- NewPasswordReset

-- | The payload for initiating a password reset.
newtype NewPasswordReset = NewPasswordReset (Either Email Phone)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema NewPasswordReset

instance ToSchema NewPasswordReset where
  schema =
    objectWithDocModifier "NewPasswordReset" objectDesc $
      NewPasswordReset
        <$> (toTuple . unNewPasswordReset) Schema..= newPasswordResetObjectSchema
    where
      unNewPasswordReset :: NewPasswordReset -> Either Email Phone
      unNewPasswordReset (NewPasswordReset v) = v

      objectDesc :: NamedSwaggerDoc -> NamedSwaggerDoc
      objectDesc = description ?~ "Data to initiate a password reset"

      newPasswordResetObjectSchema :: ObjectSchemaP SwaggerDoc (Maybe Email, Maybe Phone) (Either Email Phone)
      newPasswordResetObjectSchema = withParser newPasswordResetTupleObjectSchema fromTuple
        where
          newPasswordResetTupleObjectSchema :: ObjectSchema SwaggerDoc (Maybe Email, Maybe Phone)
          newPasswordResetTupleObjectSchema =
            (,)
              <$> fst .= maybe_ (optFieldWithDocModifier "email" phoneDocs schema)
              <*> snd .= maybe_ (optFieldWithDocModifier "phone" emailDocs schema)
            where
              emailDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
              emailDocs = description ?~ "Email"

              phoneDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
              phoneDocs = description ?~ "Phone"

      fromTuple :: (Maybe Email, Maybe Phone) -> Parser (Either Email Phone)
      fromTuple = \case
        (Just _, Just _) -> fail "Only one of 'email' or 'phone' allowed."
        (Just email, Nothing) -> pure $ Left email
        (Nothing, Just phone) -> pure $ Right phone
        (Nothing, Nothing) -> fail "One of 'email' or 'phone' required."

      toTuple :: Either Email Phone -> (Maybe Email, Maybe Phone)
      toTuple = \case
        Left e -> (Just e, Nothing)
        Right p -> (Nothing, Just p)

--------------------------------------------------------------------------------
-- CompletePasswordReset

-- | The payload for completing a password reset.
data CompletePasswordReset = CompletePasswordReset
  { cpwrIdent :: PasswordResetIdentity,
    cpwrCode :: PasswordResetCode,
    cpwrPassword :: PlainTextPassword8
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CompletePasswordReset)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema CompletePasswordReset

instance ToSchema CompletePasswordReset where
  schema =
    objectWithDocModifier "CompletePasswordReset" objectDocs $
      CompletePasswordReset
        <$> (maybePasswordResetIdentityToTuple . cpwrIdent) .= maybePasswordResetIdentityObjectSchema
        <*> cpwrCode .= fieldWithDocModifier "code" codeDocs schema
        <*> cpwrPassword .= fieldWithDocModifier "password" pwDocs schema
    where
      objectDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
      objectDocs = description ?~ "Data to complete a password reset"

      codeDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
      codeDocs = description ?~ "Password reset code"

      pwDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
      pwDocs = description ?~ "New password (6 - 1024 characters)"

      maybePasswordResetIdentityObjectSchema :: ObjectSchemaP SwaggerDoc (Maybe PasswordResetKey, Maybe Email, Maybe Phone) PasswordResetIdentity
      maybePasswordResetIdentityObjectSchema =
        withParser passwordResetIdentityTupleObjectSchema maybePasswordResetIdentityTargetFromTuple
        where
          passwordResetIdentityTupleObjectSchema :: ObjectSchema SwaggerDoc (Maybe PasswordResetKey, Maybe Email, Maybe Phone)
          passwordResetIdentityTupleObjectSchema =
            (,,)
              <$> fst3 .= maybe_ (optFieldWithDocModifier "key" keyDocs schema)
              <*> snd3 .= maybe_ (optFieldWithDocModifier "email" emailDocs schema)
              <*> thd3 .= maybe_ (optFieldWithDocModifier "phone" phoneDocs schema)
            where
              keyDocs = description ?~ "An opaque key for a pending password reset."
              emailDocs = description ?~ "A known email with a pending password reset."
              phoneDocs = description ?~ "A known phone number with a pending password reset."

          maybePasswordResetIdentityTargetFromTuple :: (Maybe PasswordResetKey, Maybe Email, Maybe Phone) -> Parser PasswordResetIdentity
          maybePasswordResetIdentityTargetFromTuple = \case
            (Just key, _, _) -> pure $ PasswordResetIdentityKey key
            (_, Just email, _) -> pure $ PasswordResetEmailIdentity email
            (_, _, Just phone) -> pure $ PasswordResetPhoneIdentity phone
            _ -> fail "key, email or phone must be present"

      maybePasswordResetIdentityToTuple :: PasswordResetIdentity -> (Maybe PasswordResetKey, Maybe Email, Maybe Phone)
      maybePasswordResetIdentityToTuple = \case
        PasswordResetIdentityKey key -> (Just key, Nothing, Nothing)
        PasswordResetEmailIdentity email -> (Nothing, Just email, Nothing)
        PasswordResetPhoneIdentity phone -> (Nothing, Nothing, Just phone)

--------------------------------------------------------------------------------
-- PasswordResetIdentity

-- | The target identity of a password reset.
data PasswordResetIdentity
  = -- | An opaque identity key for a pending password reset.
    PasswordResetIdentityKey PasswordResetKey
  | -- | A known email address with a pending password reset.
    PasswordResetEmailIdentity Email
  | -- | A known phone number with a pending password reset.
    PasswordResetPhoneIdentity Phone
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PasswordResetIdentity)

-- | Opaque identifier per user (SHA256 of the user ID).
newtype PasswordResetKey = PasswordResetKey
  {fromPasswordResetKey :: AsciiBase64Url}
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToSchema, FromByteString, ToByteString, A.FromJSON, A.ToJSON, Arbitrary)

mkPasswordResetKey :: UserId -> PasswordResetKey
mkPasswordResetKey userId =
  PasswordResetKey
    . encodeBase64Url
    . BS.pack
    . ByteArray.unpack
    $ hashWith SHA256 (toByteString' userId)

instance ToParamSchema PasswordResetKey where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance FromHttpApiData PasswordResetKey where
  parseQueryParam = fmap PasswordResetKey . parseQueryParam

deriving instance C.Cql PasswordResetKey

--------------------------------------------------------------------------------
-- PasswordResetCode

-- | Random code, acting as a very short-lived, single-use password.
newtype PasswordResetCode = PasswordResetCode
  {fromPasswordResetCode :: AsciiBase64Url}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToSchema, FromByteString, ToByteString, A.FromJSON, A.ToJSON)
  deriving (Arbitrary) via (Ranged 6 1024 AsciiBase64Url)

deriving instance C.Cql PasswordResetCode

--------------------------------------------------------------------------------
-- DEPRECATED

data PasswordReset = PasswordReset
  { pwrCode :: PasswordResetCode,
    pwrPassword :: PlainTextPassword8
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PasswordReset)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema PasswordReset

instance ToSchema PasswordReset where
  schema =
    objectWithDocModifier "PasswordReset" objectDocs $
      PasswordReset
        <$> pwrCode .= fieldWithDocModifier "code" codeDocs schema
        <*> pwrPassword .= fieldWithDocModifier "password" pwDocs schema
    where
      objectDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
      objectDocs = description ?~ "Data to complete a password reset"

      codeDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
      codeDocs = description ?~ "Password reset code"

      pwDocs :: NamedSwaggerDoc -> NamedSwaggerDoc
      pwDocs = description ?~ "New password (6 - 1024 characters)"
