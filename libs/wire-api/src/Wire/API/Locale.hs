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
  ( timeLocale,
    formatDateTime,
    deDe,
    frFr,
  )
where

import Data.LanguageCodes (ISO639_1 (DE, FR))
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Data.Time.LocalTime (TimeZone (..), utc)
import Imports
import Wire.API.User

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
