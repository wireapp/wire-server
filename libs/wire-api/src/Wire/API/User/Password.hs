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
    PasswordResetCode (..),

    -- * deprecated
    PasswordReset (..),

    -- * Swagger
    modelCompletePasswordReset,
  )
where

import Control.Lens ((?~))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Conversion
import Data.Misc (PlainTextPassword (..))
import Data.Range (Ranged (..))
import qualified Data.Schema as Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii
import Imports
import Wire.API.User.Identity
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- NewPasswordReset

-- | The payload for initiating a password reset.
newtype NewPasswordReset = NewPasswordReset (Either Email Phone)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema.Schema NewPasswordReset

instance Schema.ToSchema NewPasswordReset where
  schema =
    Schema.objectWithDocModifier "NewPasswordReset" objectDesc $
      NewPasswordReset
        <$> (toTuple . unNewPasswordReset) Schema..= newPasswordResetObjectSchema
    where
      unNewPasswordReset :: NewPasswordReset -> Either Email Phone
      unNewPasswordReset (NewPasswordReset v) = v

      objectDesc :: Schema.NamedSwaggerDoc -> Schema.NamedSwaggerDoc
      objectDesc = Schema.description ?~ "Data to initiate a password reset"

      newPasswordResetObjectSchema :: Schema.ObjectSchemaP Schema.SwaggerDoc (Maybe Email, Maybe Phone) (Either Email Phone)
      newPasswordResetObjectSchema = Schema.withParser newPasswordResetTupleObjectSchema fromTuple
        where
          newPasswordResetTupleObjectSchema :: Schema.ObjectSchema Schema.SwaggerDoc (Maybe Email, Maybe Phone)
          newPasswordResetTupleObjectSchema =
            (,)
              <$> fst Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "email" phoneDocs Schema.schema)
              <*> snd Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "phone" emailDocs Schema.schema)
            where
              emailDocs :: Schema.NamedSwaggerDoc -> Schema.NamedSwaggerDoc
              emailDocs = Schema.description ?~ "Email"

              phoneDocs :: Schema.NamedSwaggerDoc -> Schema.NamedSwaggerDoc
              phoneDocs = Schema.description ?~ "Phone"

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
    cpwrPassword :: PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CompletePasswordReset)

modelCompletePasswordReset :: Doc.Model
modelCompletePasswordReset = Doc.defineModel "CompletePasswordReset" $ do
  Doc.description "Data to complete a password reset."
  Doc.property "key" Doc.string' $ do
    Doc.description "An opaque key for a pending password reset."
    Doc.optional
  Doc.property "email" Doc.string' $ do
    Doc.description "A known email with a pending password reset."
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "A known phone number with a pending password reset."
    Doc.optional
  Doc.property "code" Doc.string' $
    Doc.description "Password reset code"
  Doc.property "password" Doc.string' $
    Doc.description "New password (6 - 1024 characters)"

instance ToJSON CompletePasswordReset where
  toJSON (CompletePasswordReset i c pw) =
    object
      [ident i, "code" .= c, "password" .= pw]
    where
      ident (PasswordResetIdentityKey k) = "key" .= k
      ident (PasswordResetEmailIdentity e) = "email" .= e
      ident (PasswordResetPhoneIdentity p) = "phone" .= p

instance FromJSON CompletePasswordReset where
  parseJSON = withObject "CompletePasswordReset" $ \o ->
    CompletePasswordReset <$> ident o <*> o .: "code" <*> o .: "password"
    where
      ident o =
        (PasswordResetIdentityKey <$> o .: "key")
          <|> (PasswordResetEmailIdentity <$> o .: "email")
          <|> (PasswordResetPhoneIdentity <$> o .: "phone")

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
  deriving stock (Eq, Show)
  deriving newtype (FromByteString, ToByteString, FromJSON, ToJSON, Arbitrary)

--------------------------------------------------------------------------------
-- PasswordResetCode

-- | Random code, acting as a very short-lived, single-use password.
newtype PasswordResetCode = PasswordResetCode
  {fromPasswordResetCode :: AsciiBase64Url}
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromByteString, ToByteString, FromJSON, ToJSON)
  deriving (Arbitrary) via (Ranged 6 1024 AsciiBase64Url)

--------------------------------------------------------------------------------
-- DEPRECATED

data PasswordReset = PasswordReset
  { pwrCode :: PasswordResetCode,
    pwrPassword :: PlainTextPassword
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PasswordReset)

instance FromJSON PasswordReset where
  parseJSON = withObject "PasswordReset" $ \o ->
    PasswordReset
      <$> o .: "code"
      <*> o .: "password"
