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

module Wire.API.User.Profile
  ( Name (..),
    mkName,
    ColourId (..),
    defaultAccentId,

    -- * Asset
    Asset (..),
    AssetSize (..),

    -- * Locale
    Locale (..),
    locToText,
    parseLocale,
    Language (..),
    lan2Text,
    parseLanguage,
    Country (..),
    con2Text,
    parseCountry,

    -- * ManagedBy
    ManagedBy (..),
    defaultManagedBy,

    -- * Deprecated
    Pict (..),
    noPict,
  )
where

import Control.Applicative (optional)
import Control.Error (hush, note)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString.Char8 (takeByteString)
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.Range
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as Text
import Imports
import Wire.API.Asset (AssetKey (..))
import Wire.API.User.Orphans ()
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- Name

-- | Usually called display name.
--
-- Length is between 1 and 128 characters.
newtype Name = Name
  {fromName :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromByteString, ToByteString)
  deriving (Arbitrary) via (Ranged 1 128 Text)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Name

mkName :: Text -> Either String Name
mkName txt = Name . fromRange <$> checkedEitherMsg @_ @1 @128 "Name" txt

instance ToSchema Name where
  schema = Name <$> fromName .= untypedRangedSchema 1 128 schema

--------------------------------------------------------------------------------
-- Colour

newtype ColourId = ColourId {fromColourId :: Int32}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToSchema, Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ColourId

defaultAccentId :: ColourId
defaultAccentId = ColourId 0

--------------------------------------------------------------------------------
-- Asset

-- Note: Intended to be turned into a sum type to add further asset types.
data Asset = ImageAsset
  { assetKey :: AssetKey,
    assetSize :: Maybe AssetSize
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Asset)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Asset

instance ToSchema Asset where
  schema =
    object "UserAsset" $
      ImageAsset
        <$> assetKey .= field "key" schema
        <*> assetSize .= maybe_ (optField "size" schema)
        <* const () .= field "type" typeSchema
    where
      typeSchema :: ValueSchema NamedSwaggerDoc ()
      typeSchema =
        enum @Text @NamedSwaggerDoc "AssetType" $
          element "image" ()

data AssetSize = AssetComplete | AssetPreview
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AssetSize)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema AssetSize

instance ToSchema AssetSize where
  schema =
    enum @Text "AssetSize" $
      mconcat
        [ element "preview" AssetPreview,
          element "complete" AssetComplete
        ]

--------------------------------------------------------------------------------
-- Locale

data Locale = Locale
  { lLanguage :: Language,
    lCountry :: Maybe Country
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform Locale)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Locale

instance ToSchema Locale where
  schema = locToText .= parsedText "Locale" (note err . parseLocale)
    where
      err = "Invalid locale. Expected <ISO 639-1>(-<ISO 3166-1-alpha2>)? format"

instance Show Locale where
  show = Text.unpack . locToText

locToText :: Locale -> Text
locToText (Locale l c) = lan2Text l <> foldMap (("-" <>) . con2Text) c

parseLocale :: Text -> Maybe Locale
parseLocale = hush . parseOnly localeParser
  where
    localeParser :: Parser Locale
    localeParser =
      Locale
        <$> (languageParser <?> "Language code")
        <*> (optional (char '-' *> countryParser) <?> "Country code")

--------------------------------------------------------------------------------
-- Language

newtype Language = Language {fromLanguage :: ISO639_1}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Arbitrary, S.ToSchema)

languageParser :: Parser Language
languageParser = codeParser "language" $ fmap Language . checkAndConvert isLower

lan2Text :: Language -> Text
lan2Text = Text.toLower . Text.pack . show . fromLanguage

parseLanguage :: Text -> Maybe Language
parseLanguage = hush . parseOnly languageParser

--------------------------------------------------------------------------------
-- Country

newtype Country = Country {fromCountry :: CountryCode}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Arbitrary, S.ToSchema)

countryParser :: Parser Country
countryParser = codeParser "country" $ fmap Country . checkAndConvert isUpper

con2Text :: Country -> Text
con2Text = Text.pack . show . fromCountry

parseCountry :: Text -> Maybe Country
parseCountry = hush . parseOnly countryParser

--------------------------------------------------------------------------------
-- ManagedBy

-- | Who controls changes to the user profile (where the profile is defined as "all
-- user-editable, user-visible attributes").  See {#SparBrainDump}.
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
  deriving stock (Eq, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ManagedBy)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ManagedBy)

instance ToSchema ManagedBy where
  schema =
    enum @Text "ManagedBy" $
      mconcat
        [ element "wire" ManagedByWire,
          element "scim" ManagedByScim
        ]

instance ToByteString ManagedBy where
  builder ManagedByWire = "wire"
  builder ManagedByScim = "scim"

instance FromByteString ManagedBy where
  parser =
    takeByteString >>= \case
      "wire" -> pure ManagedByWire
      "scim" -> pure ManagedByScim
      x -> fail $ "Invalid ManagedBy value: " <> show x

defaultManagedBy :: ManagedBy
defaultManagedBy = ManagedByWire

--------------------------------------------------------------------------------
-- Deprecated

-- | DEPRECATED
newtype Pict = Pict {fromPict :: [A.Object]}
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Pict

instance ToSchema Pict where
  schema =
    named "Pict" $
      Pict <$> fromPict .= untypedRangedSchema 0 10 (array jsonObject)

instance Arbitrary Pict where
  arbitrary = pure $ Pict []

noPict :: Pict
noPict = Pict []

--------------------------------------------------------------------------------
-- helpers

-- Common language / country functions
checkAndConvert :: (Read a) => (Char -> Bool) -> String -> Maybe a
checkAndConvert f t =
  if all f t
    then readMaybe (map toUpper t)
    else fail "Format not supported."

codeParser :: String -> (String -> Maybe a) -> Parser a
codeParser err conv = do
  code <- count 2 anyChar
  maybe (fail err) pure (conv code)
