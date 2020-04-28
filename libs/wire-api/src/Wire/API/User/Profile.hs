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

module Wire.API.User.Profile where

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

--------------------------------------------------------------------------------
-- Name

-- | Usually called display name.
newtype Name = Name
  {fromName :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromByteString, ToByteString, Generic)

instance FromJSON Name where
  parseJSON x =
    Name . fromRange
      <$> (parseJSON x :: Json.Parser (Range 1 128 Text))

--------------------------------------------------------------------------------
-- Colour

newtype ColourId = ColourId {fromColourId :: Int32}
  deriving (Eq, Num, Ord, Show, FromJSON, ToJSON, Generic)

defaultAccentId :: ColourId
defaultAccentId = ColourId 0

-----------------------------------------------------------------------------
-- Asset

data AssetSize = AssetComplete | AssetPreview
  deriving (Eq, Show, Enum, Bounded, Generic)

-- Note: Intended to be turned into a sum type to add further asset types.
data Asset = ImageAsset
  { assetKey :: !Text,
    assetSize :: !(Maybe AssetSize)
  }
  deriving (Eq, Show, Generic)

instance FromJSON AssetSize where
  parseJSON = withText "AssetSize" $ \s ->
    case s of
      "preview" -> pure AssetPreview
      "complete" -> pure AssetComplete
      _ -> fail $ "Invalid asset size: " ++ show s

instance ToJSON AssetSize where
  toJSON AssetPreview = String "preview"
  toJSON AssetComplete = String "complete"

instance FromJSON Asset where
  parseJSON = withObject "Asset" $ \o -> do
    typ <- o .: "type"
    key <- o .: "key"
    siz <- o .:? "size"
    case (typ :: Text) of
      "image" -> pure (ImageAsset key siz)
      _ -> fail $ "Invalid asset type: " ++ show typ

instance ToJSON Asset where
  toJSON (ImageAsset k s) =
    object $
      "type" .= ("image" :: Text)
        # "key" .= k
        # "size" .= s
        # []

-----------------------------------------------------------------------------
-- Language

newtype Language = Language {fromLanguage :: ISO639_1} deriving (Ord, Eq, Show, Generic)

languageParser :: Parser Language
languageParser = codeParser "language" $ fmap Language . checkAndConvert isLower

lan2Text :: Language -> Text
lan2Text = Text.toLower . Text.pack . show . fromLanguage

parseLanguage :: Text -> Maybe Language
parseLanguage = hush . parseOnly languageParser

-----------------------------------------------------------------------------
-- Country

newtype Country = Country {fromCountry :: CountryCode} deriving (Ord, Eq, Show, Generic)

countryParser :: Parser Country
countryParser = codeParser "country" $ fmap Country . checkAndConvert isUpper

con2Text :: Country -> Text
con2Text = Text.pack . show . fromCountry

parseCountry :: Text -> Maybe Country
parseCountry = hush . parseOnly countryParser

-----------------------------------------------------------------------------
-- Locale

data Locale = Locale
  { lLanguage :: !Language,
    lCountry :: !(Maybe Country)
  }
  deriving (Eq, Ord, Generic)

locToText :: Locale -> Text
locToText (Locale l c) = lan2Text l <> maybe mempty (("-" <>) . con2Text) c

instance FromJSON Locale where
  parseJSON =
    withText "locale" $
      maybe (fail "Invalid locale. Expected <ISO 639-1>(-<ISO 3166-1-alpha2>)? format") return
        . parseLocale

instance ToJSON Locale where
  toJSON = String . locToText

instance Show Locale where
  show = Text.unpack . locToText

parseLocale :: Text -> Maybe Locale
parseLocale = hush . parseOnly localeParser
  where
    localeParser :: Parser Locale
    localeParser =
      Locale <$> (languageParser <?> "Language code")
        <*> (optional (char '-' *> countryParser) <?> "Country code")

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

-----------------------------------------------------------------------------
-- ManagedBy

-- | Who controls changes to the user profile (where the profile is defined as "all
-- user-editable, user-visible attributes").  See {#DevScimOneWaySync}.
data ManagedBy
  = -- | The profile can be changed in-app; user doesn't show up via SCIM at all.
    ManagedByWire
  | -- | The profile can only be changed via SCIM, with several exceptions:
    --
    --   1. User properties can still be set (because they are used internally by clients
    --      and none of them can be modified via SCIM now or in the future).
    --
    --   2. Password can be changed by the user (SCIM doesn't support setting passwords yet,
    --      but currently SCIM only works with SSO-users who don't even have passwords).
    --
    --   3. The user can still be deleted normally (SCIM doesn't support deleting users yet;
    --      but it's questionable whether this should even count as a /change/ of a user
    --      profile).
    --
    -- There are some other things that SCIM can't do yet, like setting accent IDs, but they
    -- are not essential, unlike e.g. passwords.
    ManagedByScim
  deriving (Eq, Show, Bounded, Enum, Generic)

instance FromJSON ManagedBy where
  parseJSON = withText "ManagedBy" $ \case
    "wire" -> pure ManagedByWire
    "scim" -> pure ManagedByScim
    other -> fail $ "Invalid ManagedBy: " ++ show other

instance ToJSON ManagedBy where
  toJSON = String . \case
    ManagedByWire -> "wire"
    ManagedByScim -> "scim"

defaultManagedBy :: ManagedBy
defaultManagedBy = ManagedByWire
