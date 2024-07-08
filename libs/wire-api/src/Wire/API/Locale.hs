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

module Wire.API.Locale
  ( Locale (..),
    Language (..),
    Country (..),
    timeLocale,
    formatDateTime,
    deDe,
    frFr,
    locToText,
    parseLocale,
    lan2Text,
    parseLanguage,
    con2Text,
    parseCountry,
  )
where

import Cassandra as C
import Control.Applicative (optional)
import Control.Error.Util (hush, note)
import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.Text
import Data.ISO3166_CountryCodes (CountryCode)
import Data.LanguageCodes (ISO639_1 (DE, FR))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Data.Time.LocalTime (TimeZone (..), utc)
import Imports
import Test.QuickCheck
import Wire.API.User.Orphans ()
import Wire.Arbitrary

timeLocale :: Locale -> TimeLocale
timeLocale (Locale (Language DE) _) = deDe
timeLocale (Locale (Language FR) _) = frFr
timeLocale _ = defaultTimeLocale

formatDateTime :: String -> TimeLocale -> UTCTime -> Text
formatDateTime s l = fromString . formatTime l s

deDe :: TimeLocale
deDe =
  TimeLocale
    { wDays =
        [ ("Sonntag", "Son"),
          ("Montag", "Mon"),
          ("Dienstag", "Die"),
          ("Mittwoch", "Mit"),
          ("Donnerstag", "Don"),
          ("Freitag", "Fre"),
          ("Samstag", "Sam")
        ],
      months =
        [ ("Januar", "Jan"),
          ("Februar", "Feb"),
          ("März", "Mär"),
          ("April", "Apr"),
          ("Mai", "Mai"),
          ("Juni", "Jun"),
          ("Juli", "Jul"),
          ("August", "Aug"),
          ("September", "Sep"),
          ("Oktober", "Okt"),
          ("November", "Nov"),
          ("Dezember", "Dez")
        ],
      amPm = ("", ""),
      dateTimeFmt = "%d. %B %Y %H:%M:%S %Z",
      dateFmt = "%d.%m.%Y",
      timeFmt = "%H:%M:%S",
      time12Fmt = "%H:%M:%S",
      knownTimeZones =
        [ utc,
          TimeZone 60 False "MEZ",
          TimeZone 120 True "MESZ"
        ]
    }

frFr :: TimeLocale
frFr =
  TimeLocale
    { wDays =
        [ ("dimanche", "dim"),
          ("lundi", "lun"),
          ("mardi", "mar"),
          ("mercredi", "mer"),
          ("jeudi", "jeu"),
          ("vendredi", "ven"),
          ("samedi", "sam")
        ],
      months =
        [ ("janvier", "jan"),
          ("février", "fév"),
          ("mars", "mar"),
          ("avril", "avr"),
          ("mai", "mai"),
          ("juin", "jun"),
          ("juillet", "jul"),
          ("août", "aoû"),
          ("septembre", "sep"),
          ("octobre", "oct"),
          ("novembre", "nov"),
          ("décembre", "déc")
        ],
      amPm = ("", ""),
      dateTimeFmt = "%d %B %Y %H h %M %Z",
      dateFmt = "%d/%m/%Y",
      timeFmt = "%H h %M",
      time12Fmt = "%H h %M",
      knownTimeZones =
        [ utc,
          TimeZone 60 False "HNEC",
          TimeZone 120 True "HAEC"
        ]
    }

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

instance C.Cql Language where
  ctype = C.Tagged C.AsciiColumn
  toCql = C.toCql . lan2Text

  fromCql (C.CqlAscii l) = case parseLanguage l of
    Just l' -> pure l'
    Nothing -> Left "Language: ISO 639-1 expected."
  fromCql _ = Left "Language: ASCII expected"

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

instance C.Cql Country where
  ctype = C.Tagged C.AsciiColumn
  toCql = C.toCql . con2Text

  fromCql (C.CqlAscii c) = case parseCountry c of
    Just c' -> pure c'
    Nothing -> Left "Country: ISO 3166-1-alpha2 expected."
  fromCql _ = Left "Country: ASCII expected"

countryParser :: Parser Country
countryParser = codeParser "country" $ fmap Country . checkAndConvert isUpper

con2Text :: Country -> Text
con2Text = Text.pack . show . fromCountry

parseCountry :: Text -> Maybe Country
parseCountry = hush . parseOnly countryParser

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
