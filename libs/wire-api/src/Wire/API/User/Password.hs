{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

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

module Wire.API.User.Password
  ( NewPasswordReset (..),
    CompletePasswordReset (..),
    PasswordResetIdentity (..),
    PasswordResetKey (..),
    PasswordResetCode (..),

    -- * deprecated
    PasswordReset (..),

    -- * Swagger
    modelNewPasswordReset,
    modelCompletePasswordReset,
  )
where

import Data.Aeson
import Data.ByteString.Conversion
import Data.Misc (PlainTextPassword (..))
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii
import Imports
import Wire.API.User.Identity

-----------------------------------------------------------------------------
-- NewPasswordReset

-- | The payload for initiating a password reset.
newtype NewPasswordReset = NewPasswordReset (Either Email Phone)
  deriving (Eq, Show, Generic)

modelNewPasswordReset :: Doc.Model
modelNewPasswordReset = Doc.defineModel "NewPasswordReset" $ do
  Doc.description "Data to initiate a password reset"
  Doc.property "email" Doc.string' $ do
    Doc.description "Email"
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "Phone"
    Doc.optional

instance ToJSON NewPasswordReset where
  toJSON (NewPasswordReset ident) =
    object
      [either ("email" .=) ("phone" .=) ident]

instance FromJSON NewPasswordReset where
  parseJSON = withObject "NewPasswordReset" $ \o ->
    NewPasswordReset
      <$> ( (Left <$> o .: "email")
              <|> (Right <$> o .: "phone")
          )

-----------------------------------------------------------------------------
-- CompletePasswordReset

-- | The payload for completing a password reset.
data CompletePasswordReset = CompletePasswordReset
  { cpwrIdent :: !PasswordResetIdentity,
    cpwrCode :: !PasswordResetCode,
    cpwrPassword :: !PlainTextPassword
  }
  deriving (Eq, Show, Generic)

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

-----------------------------------------------------------------------------
-- PasswordResetIdentity

-- | The target identity of a password reset.
data PasswordResetIdentity
  = -- | An opaque identity key for a pending password reset.
    PasswordResetIdentityKey !PasswordResetKey
  | -- | A known email address with a pending password reset.
    PasswordResetEmailIdentity !Email
  | -- | A known phone number with a pending password reset.
    PasswordResetPhoneIdentity !Phone
  deriving (Eq, Show, Generic)

-- | Opaque identifier per user (SHA256 of the user ID).
newtype PasswordResetKey = PasswordResetKey
  {fromPasswordResetKey :: AsciiBase64Url}
  deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON, Generic)

-----------------------------------------------------------------------------
-- PasswordResetCode

-- | Random code, acting as a very short-lived, single-use password.
newtype PasswordResetCode = PasswordResetCode
  {fromPasswordResetCode :: AsciiBase64Url}
  deriving (Eq, Show, FromByteString, ToByteString, FromJSON, ToJSON, Generic)

--------------------------------------------------------------------------------
-- DEPRECATED

data PasswordReset = PasswordReset
  { pwrCode :: !PasswordResetCode,
    pwrPassword :: !PlainTextPassword
  }

instance FromJSON PasswordReset where
  parseJSON = withObject "PasswordReset" $ \o ->
    PasswordReset <$> o .: "code"
      <*> o .: "password"
