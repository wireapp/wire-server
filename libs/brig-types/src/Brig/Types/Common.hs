{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- rename to Brig.Types.Account?
module Brig.Types.Common
  ( -- * PhoneBudgetTimeout
    PhoneBudgetTimeout (..),

    -- * PhonePrefix
    PhonePrefix (..),
    parsePhonePrefix,
    isValidPhonePrefix,
    allPrefixes,
    ExcludedPrefix (..),

    -- * re-exports
    Name (..),
    ColourId (..),
    defaultAccentId,
    Email (..),
    fromEmail,
    parseEmail,
    validateEmail,
    Phone (..),
    parsePhone,
    isValidPhone,
    UserIdentity (..),
    newIdentity,
    emailIdentity,
    phoneIdentity,
    ssoIdentity,
    UserSSOId (..),
    Asset (..),
    AssetSize (..),
    Language (..),
    lan2Text,
    parseLanguage,
    Country (..),
    con2Text,
    parseCountry,
    Locale (..),
    locToText,
    parseLocale,
    ManagedBy (..),
    defaultManagedBy,
  )
where

import Control.Applicative (optional)
import Data.Aeson hiding ((<?>))
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import Imports
import Wire.API.User.Identity
import Wire.API.User.Profile

------------------------------------------------------------------------------
--- PhoneBudgetTimeout

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

newtype PhonePrefix = PhonePrefix {fromPhonePrefix :: Text}
  deriving (Eq, Show, ToJSON, Generic)

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
