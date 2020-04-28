{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Wire.API.User.Identity where

import Control.Applicative (optional)
import Control.Error (hush)
import Data.Aeson hiding ((<?>))
import qualified Data.Aeson.Types as Json
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.ISO3166_CountryCodes
import Data.Json.Util ((#))
import Data.LanguageCodes
import Data.Range
import qualified Data.Text as Text
import Data.Time.Clock
import Imports

-----------------------------------------------------------------------------
-- Email

-- FUTUREWORK: replace this type with 'EmailAddress'
data Email = Email
  { emailLocal :: !Text,
    emailDomain :: !Text
  }
  deriving (Eq, Ord, Generic)

instance Show Email where
  show = Text.unpack . fromEmail

instance FromByteString Email where
  parser = parser >>= maybe (fail "Invalid email") return . parseEmail

instance ToByteString Email where
  builder = builder . fromEmail

instance FromJSON Email where
  parseJSON =
    withText "email" $
      maybe (fail "Invalid email. Expected '<local>@<domain>'.") return
        . parseEmail

instance ToJSON Email where
  toJSON = String . fromEmail

fromEmail :: Email -> Text
fromEmail (Email loc dom) = loc <> "@" <> dom

-- | Parses an email address of the form <local-part>@<domain>.
parseEmail :: Text -> Maybe Email
parseEmail t = case Text.split (== '@') t of
  [localPart, domain] -> Just $! Email localPart domain
  _ -> Nothing

-----------------------------------------------------------------------------
-- Phone

newtype Phone = Phone {fromPhone :: Text} deriving (Eq, Show, ToJSON, Generic)

-- | Parses a phone number in E.164 format with a mandatory leading '+'.
parsePhone :: Text -> Maybe Phone
parsePhone p
  | isValidPhone p = Just $! Phone p
  | otherwise = Nothing

-- | Checks whether a phone number is valid, i.e. it is in E.164 format
-- with a mandatory leading '+' followed by 10-15 digits.
isValidPhone :: Text -> Bool
isValidPhone = either (const False) (const True) . parseOnly e164
  where
    e164 = char '+' *> count 8 digit *> count 7 (optional digit) *> endOfInput

instance FromJSON Phone where
  parseJSON (String s) = case parsePhone s of
    Just p -> return p
    Nothing -> fail "Invalid phone number. Expected E.164 format."
  parseJSON _ = mempty

instance FromByteString Phone where
  parser = parser >>= maybe (fail "Invalid phone") return . parsePhone

instance ToByteString Phone where
  builder = builder . fromPhone

-- | If the budget for SMS and voice calls for a phone number
-- has been exhausted within a certain time frame, this timeout
-- indicates in seconds when another attempt may be made.
newtype PhoneBudgetTimeout = PhoneBudgetTimeout
  {phoneBudgetTimeout :: NominalDiffTime}
  deriving (Eq, Show, Generic)

instance FromJSON PhoneBudgetTimeout where
  parseJSON = withObject "PhoneBudgetTimeout" $ \o ->
    PhoneBudgetTimeout <$> o .: "expires_in"

instance ToJSON PhoneBudgetTimeout where
  toJSON (PhoneBudgetTimeout t) = object ["expires_in" .= t]

-----------------------------------------------------------------------------
-- PhonePrefix (for excluding from SMS/calling)

newtype PhonePrefix = PhonePrefix {fromPhonePrefix :: Text} deriving (Eq, Show, ToJSON, Generic)

-- | Parses a phone number prefix with a mandatory leading '+'.
parsePhonePrefix :: Text -> Maybe PhonePrefix
parsePhonePrefix p
  | isValidPhonePrefix p = Just $ PhonePrefix p
  | otherwise = Nothing

-- | Checks whether a phone number prefix is valid,
-- i.e. it is like a E.164 format phone number, but shorter
-- (with a mandatory leading '+', followed by 1-15 digits.)
isValidPhonePrefix :: Text -> Bool
isValidPhonePrefix = isRight . parseOnly e164Prefix
  where
    e164Prefix = char '+' *> count 1 digit *> count 14 (optional digit) *> endOfInput

-- | get all valid prefixes of a phone number or phone number prefix
-- e.g. from +123456789 get prefixes ["+1", "+12", "+123", ..., "+123456789" ]
allPrefixes :: Text -> [PhonePrefix]
allPrefixes t = catMaybes $ parsePhonePrefix <$> Text.inits t

instance FromJSON PhonePrefix where
  parseJSON = withText "PhonePrefix" $ \s ->
    case parsePhonePrefix s of
      Just p -> return p
      Nothing ->
        fail $
          "Invalid phone number prefix: [" ++ show s
            ++ "]. Expected format similar to E.164 (with 1-15 digits after the +)."

instance FromByteString PhonePrefix where
  parser = parser >>= maybe (fail "Invalid phone") return . parsePhonePrefix

instance ToByteString PhonePrefix where
  builder = builder . fromPhonePrefix

data ExcludedPrefix = ExcludedPrefix
  { phonePrefix :: PhonePrefix,
    comment :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ExcludedPrefix where
  parseJSON = withObject "ExcludedPrefix" $ \o ->
    ExcludedPrefix
      <$> o .: "phone_prefix"
      <*> o .: "comment"

instance ToJSON ExcludedPrefix where
  toJSON (ExcludedPrefix p c) = object ["phone_prefix" .= p, "comment" .= c]

-----------------------------------------------------------------------------
-- UserIdentity

-- | The private unique user identity that is used for login and
-- account recovery.
data UserIdentity
  = FullIdentity !Email !Phone
  | EmailIdentity !Email
  | PhoneIdentity !Phone
  | SSOIdentity !UserSSOId !(Maybe Email) !(Maybe Phone)
  deriving (Eq, Show, Generic)

instance FromJSON UserIdentity where
  parseJSON = withObject "UserIdentity" $ \o -> do
    email <- o .:? "email"
    phone <- o .:? "phone"
    ssoid <- o .:? "sso_id"
    maybe
      (fail "Missing 'email' or 'phone' or 'sso_id'.")
      return
      (newIdentity email phone ssoid)

instance ToJSON UserIdentity where
  toJSON = \case
    FullIdentity em ph -> go (Just em) (Just ph) Nothing
    EmailIdentity em -> go (Just em) Nothing Nothing
    PhoneIdentity ph -> go Nothing (Just ph) Nothing
    SSOIdentity si em ph -> go em ph (Just si)
    where
      go :: Maybe Email -> Maybe Phone -> Maybe UserSSOId -> Value
      go em ph si = object ["email" .= em, "phone" .= ph, "sso_id" .= si]

newIdentity :: Maybe Email -> Maybe Phone -> Maybe UserSSOId -> Maybe UserIdentity
newIdentity email phone (Just sso) = Just $! SSOIdentity sso email phone
newIdentity Nothing Nothing Nothing = Nothing
newIdentity (Just e) Nothing Nothing = Just $! EmailIdentity e
newIdentity Nothing (Just p) Nothing = Just $! PhoneIdentity p
newIdentity (Just e) (Just p) Nothing = Just $! FullIdentity e p

emailIdentity :: UserIdentity -> Maybe Email
emailIdentity (FullIdentity email _) = Just email
emailIdentity (EmailIdentity email) = Just email
emailIdentity (PhoneIdentity _) = Nothing
emailIdentity (SSOIdentity _ (Just email) _) = Just email
emailIdentity (SSOIdentity _ Nothing _) = Nothing

phoneIdentity :: UserIdentity -> Maybe Phone
phoneIdentity (FullIdentity _ phone) = Just phone
phoneIdentity (PhoneIdentity phone) = Just phone
phoneIdentity (EmailIdentity _) = Nothing
phoneIdentity (SSOIdentity _ _ (Just phone)) = Just phone
phoneIdentity (SSOIdentity _ _ Nothing) = Nothing

ssoIdentity :: UserIdentity -> Maybe UserSSOId
ssoIdentity (SSOIdentity ssoid _ _) = Just ssoid
ssoIdentity _ = Nothing

-- | User's external identity.
--
-- Morally this is the same thing as 'SAML.UserRef', but we forget the
-- structure -- i.e. we just store XML-encoded SAML blobs. If the structure
-- of those blobs changes, Brig won't have to deal with it, only Spar will.
--
-- TODO: once we have @/libs/spar-types@ for the wire-sso-sp-server called spar, this type should
-- move there.
data UserSSOId = UserSSOId
  { -- | An XML blob pointing to the identity provider that can confirm
    -- user's identity.
    userSSOIdTenant :: Text,
    -- | An XML blob specifying the user's ID on the identity provider's side.
    userSSOIdSubject :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserSSOId where
  parseJSON = withObject "UserSSOId" $ \obj ->
    UserSSOId
      <$> obj .: "tenant"
      <*> obj .: "subject"

instance ToJSON UserSSOId where
  toJSON (UserSSOId tenant subject) = object ["tenant" .= tenant, "subject" .= subject]
