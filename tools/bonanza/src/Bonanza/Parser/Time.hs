{-# LANGUAGE OverloadedStrings #-}

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

module Bonanza.Parser.Time
  ( tai64N,
    iso8601UTC,
    isoDay,
    isoTime,
    commonLogDate,
  )
where

import Control.Applicative (optional)
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor (first)
import qualified Data.List as List
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock.TAI
import Imports hiding (take)

tai64N :: Parser UTCTime
tai64N = do
  secs <- (take 16 >>= int) <&> subtract taiBase
  nano <- take 8 >>= int
  let t = posixSecondsToUTCTime $ posix secs nano
      l = (-1) * fromIntegral (leapSeconds t)
  pure $ addUTCTime l t
  where
    int :: ByteString -> Parser Integer
    int bs =
      either
        (const $ fail "not a hexadecimal number")
        pure
        (parseOnly hexadecimal bs)
    posix :: Integer -> Integer -> POSIXTime
    posix secs nano =
      let picos = toPico secs fSecond + toPico nano fNano
       in realToFrac picos / 10 ^ (12 :: Int)
    taiBase :: Integer
    taiBase = 4611686018427387904

iso8601UTC :: Parser UTCTime
iso8601UTC = UTCTime <$> isoDay <* char 'T' <*> isoTime

isoDay :: Parser Day
isoDay = fromGregorian <$> decimal <* char '-' <*> decimal <* char '-' <*> decimal

isoTime :: Parser DiffTime
isoTime = do
  h <- decimal <* char ':'
  m <- decimal <* char ':'
  s <- decimal
  u <- option 0 (char '.' *> decimal)
  _ <- optional (string "Z" <|> string "+0000")
  pure . picosecondsToDiffTime $
    toPico h fHour
      + toPico m fMinute
      + toPico s fSecond
      + toPico u fMicro

commonLogDate :: Parser UTCTime
commonLogDate = do
  _ <- char '['
  d <- decimal <* char '/'
  m <- (month =<< take 3) <* char '/'
  y <- decimal <* char ':'
  h <- decimal <* char ':'
  m' <- decimal <* char ':'
  s <- decimal <* char ' '
  z <- signed decimal
  _ <- char ']'
  let ld = fromGregorian y m d
      lt = TimeOfDay h m' (fromInteger s)
      tz = hoursToTimeZone z
   in pure $ localTimeToUTC tz (LocalTime ld lt)
  where
    month "Jan" = pure 1
    month "Feb" = pure 2
    month "Mar" = pure 3
    month "Apr" = pure 4
    month "May" = pure 5
    month "Jun" = pure 6
    month "Jul" = pure 7
    month "Aug" = pure 8
    month "Sep" = pure 9
    month "Oct" = pure 10
    month "Nov" = pure 11
    month "Dec" = pure 12
    month x = fail $ "not a valid month name: " ++ show x

--------------------------------------------------------------------------------
-- Internal

toPico :: Integer -> Integer -> Integer
toPico i factor = i * factor

fHour :: Integer
fHour = round $ (3.6 :: Double) * 10 ^ (15 :: Int)

fMinute :: Integer
fMinute = round $ (6.0 :: Double) * 10 ^ (13 :: Int)

fSecond :: Integer
fSecond = 10 ^ (12 :: Int)

fMicro :: Integer
fMicro = 10 ^ (7 :: Int)

fNano :: Integer
fNano = 1000

leapSeconds :: UTCTime -> Int
leapSeconds = fromMaybe def . leapSecondsMap . utctDay
  where
    def = 37 -- NOTE: This does not work for dates < 1972

-- Source: https://www.ietf.org/timezones/data/leap-seconds.list
leapSecondsMap :: LeapSecondMap
leapSecondsMap v = List.lookup v $ map (first read) leap
  where
    leap =
      [ ("1972-01-01", 10),
        ("1972-07-01", 11),
        ("1973-01-01", 12),
        ("1974-01-01", 13),
        ("1975-01-01", 14),
        ("1976-01-01", 15),
        ("1977-01-01", 16),
        ("1978-01-01", 17),
        ("1979-01-01", 18),
        ("1980-01-01", 19),
        ("1981-07-01", 20),
        ("1982-07-01", 21),
        ("1983-07-01", 22),
        ("1985-07-01", 23),
        ("1988-01-01", 24),
        ("1990-01-01", 25),
        ("1991-01-01", 26),
        ("1992-07-01", 27),
        ("1993-07-01", 28),
        ("1994-07-01", 29),
        ("1996-01-01", 30),
        ("1997-07-01", 31),
        ("1999-01-01", 32),
        ("2006-01-01", 33),
        ("2009-01-01", 34),
        ("2012-07-01", 35),
        ("2015-07-01", 36),
        ("2017-01-01", 37)
      ]
