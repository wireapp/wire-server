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

module Brig.Types.Common {- rename to Brig.Types.Account? -}
  ( Name (..),
    ColourId (..),
    defaultAccentId,
    Email (..),
    fromEmail,
    parseEmail,
    Phone (..),
    parsePhone,
    isValidPhone,
    PhoneBudgetTimeout (..),
    PhonePrefix (..),
    parsePhonePrefix,
    isValidPhonePrefix,
    allPrefixes,
    ExcludedPrefix (..),
    UserIdentity (..),
    newIdentity,
    emailIdentity,
    phoneIdentity,
    ssoIdentity,
    UserSSOId (..),
    Asset (..),
    AssetSize (..),
    Language (..),
    languageParser,
    lan2Text,
    parseLanguage,
    Country (..),
    countryParser,
    con2Text,
    parseCountry,
    Locale (..),
    locToText,
    parseLocale,
    checkAndConvert,
    codeParser,
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
--- Email

fromEmail :: Email -> Text
fromEmail (Email loc dom) = loc <> "@" <> dom

-- | Parses an email address of the form <local-part>@<domain>.
parseEmail :: Text -> Maybe Email
parseEmail t = case Text.split (== '@') t of
  [localPart, domain] -> Just $! Email localPart domain
  _ -> Nothing

------------------------------------------------------------------------------
--- Phone

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

-----------------------------------------------------------------------------
-- Language

languageParser :: Parser Language
languageParser = codeParser "language" $ fmap Language . checkAndConvert isLower

lan2Text :: Language -> Text
lan2Text = Text.toLower . Text.pack . show . fromLanguage

-----------------------------------------------------------------------------
-- Country

countryParser :: Parser Country
countryParser = codeParser "country" $ fmap Country . checkAndConvert isUpper

con2Text :: Country -> Text
con2Text = Text.pack . show . fromCountry

-- Common language / country functions
checkAndConvert :: (Read a) => (Char -> Bool) -> String -> Maybe a
checkAndConvert f t =
  if all f t
    then readMaybe (map toUpper t)
    else fail "Format not supported."

codeParser :: String -> (String -> Maybe a) -> Parser a
codeParser err conv = do
  code <- count 2 anyChar
  maybe (fail err) return (conv code)
