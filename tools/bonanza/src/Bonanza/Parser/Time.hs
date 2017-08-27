{-# LANGUAGE OverloadedStrings #-}

module Bonanza.Parser.Time
    ( tai64N
    , iso8601UTC
    , isoDay
    , isoTime
    , commonLogDate
    )
where

import Control.Applicative
import Data.ByteString       (ByteString)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock.TAI
import Prelude               hiding (take)

import Data.Attoparsec.ByteString.Char8


tai64N :: Parser UTCTime
tai64N = do
    secs <- take 16 >>= int >>= return . subtract taiBase
    nano <- take  8 >>= int
    let t = posixSecondsToUTCTime $ posix secs nano
        l = (-1) * fromIntegral (leapSeconds t)
    return $ addUTCTime l t
  where
    int :: ByteString -> Parser Integer
    int bs = either (const $ fail "not a hexadecimal number")
                    (return . id)
                    (parseOnly hexadecimal bs)

    posix :: Integer -> Integer -> POSIXTime
    posix secs nano = let picos = toPico secs fSecond + toPico nano fNano
                       in realToFrac picos / 10^(12 :: Int)

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
    return . picosecondsToDiffTime
           $ toPico h fHour
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
     in return $ localTimeToUTC tz (LocalTime ld lt)
  where
    month "Jan" = return 1
    month "Feb" = return 2
    month "Mar" = return 3
    month "Apr" = return 4
    month "May" = return 5
    month "Jun" = return 6
    month "Jul" = return 7
    month "Aug" = return 8
    month "Sep" = return 9
    month "Oct" = return 10
    month "Nov" = return 11
    month "Dec" = return 12
    month x     = fail $ "not a valid month name: " ++ show x

--------------------------------------------------------------------------------
-- Internal

toPico :: Integer -> Integer -> Integer
toPico i factor = i * factor

fHour :: Integer
fHour = round $ (3.6 :: Double) * 10^(15 :: Int)

fMinute :: Integer
fMinute = round $ (6.0 :: Double) * 10^(13 :: Int)

fSecond :: Integer
fSecond = 10^(12 :: Int)

fMicro :: Integer
fMicro = 10^(7 :: Int)

fNano :: Integer
fNano = 1000

leapSeconds :: UTCTime -> Integer
leapSeconds = leapSecondsTable . utctDay

leapSecondsTable :: LeapSecondTable
leapSecondsTable = parseTAIUTCDATFile $ unlines
    [ "1961 JAN  1 =JD 2437300.5  TAI-UTC=   1.4228180 S + (MJD - 37300.) X 0.001296 S"
    , "1961 AUG  1 =JD 2437512.5  TAI-UTC=   1.3728180 S + (MJD - 37300.) X 0.001296 S"
    , "1962 JAN  1 =JD 2437665.5  TAI-UTC=   1.8458580 S + (MJD - 37665.) X 0.0011232S"
    , "1963 NOV  1 =JD 2438334.5  TAI-UTC=   1.9458580 S + (MJD - 37665.) X 0.0011232S"
    , "1964 JAN  1 =JD 2438395.5  TAI-UTC=   3.2401300 S + (MJD - 38761.) X 0.001296 S"
    , "1964 APR  1 =JD 2438486.5  TAI-UTC=   3.3401300 S + (MJD - 38761.) X 0.001296 S"
    , "1964 SEP  1 =JD 2438639.5  TAI-UTC=   3.4401300 S + (MJD - 38761.) X 0.001296 S"
    , "1965 JAN  1 =JD 2438761.5  TAI-UTC=   3.5401300 S + (MJD - 38761.) X 0.001296 S"
    , "1965 MAR  1 =JD 2438820.5  TAI-UTC=   3.6401300 S + (MJD - 38761.) X 0.001296 S"
    , "1965 JUL  1 =JD 2438942.5  TAI-UTC=   3.7401300 S + (MJD - 38761.) X 0.001296 S"
    , "1965 SEP  1 =JD 2439004.5  TAI-UTC=   3.8401300 S + (MJD - 38761.) X 0.001296 S"
    , "1966 JAN  1 =JD 2439126.5  TAI-UTC=   4.3131700 S + (MJD - 39126.) X 0.002592 S"
    , "1968 FEB  1 =JD 2439887.5  TAI-UTC=   4.2131700 S + (MJD - 39126.) X 0.002592 S"
    , "1972 JAN  1 =JD 2441317.5  TAI-UTC=  10.0       S + (MJD - 41317.) X 0.0      S"
    , "1972 JUL  1 =JD 2441499.5  TAI-UTC=  11.0       S + (MJD - 41317.) X 0.0      S"
    , "1973 JAN  1 =JD 2441683.5  TAI-UTC=  12.0       S + (MJD - 41317.) X 0.0      S"
    , "1974 JAN  1 =JD 2442048.5  TAI-UTC=  13.0       S + (MJD - 41317.) X 0.0      S"
    , "1975 JAN  1 =JD 2442413.5  TAI-UTC=  14.0       S + (MJD - 41317.) X 0.0      S"
    , "1976 JAN  1 =JD 2442778.5  TAI-UTC=  15.0       S + (MJD - 41317.) X 0.0      S"
    , "1977 JAN  1 =JD 2443144.5  TAI-UTC=  16.0       S + (MJD - 41317.) X 0.0      S"
    , "1978 JAN  1 =JD 2443509.5  TAI-UTC=  17.0       S + (MJD - 41317.) X 0.0      S"
    , "1979 JAN  1 =JD 2443874.5  TAI-UTC=  18.0       S + (MJD - 41317.) X 0.0      S"
    , "1980 JAN  1 =JD 2444239.5  TAI-UTC=  19.0       S + (MJD - 41317.) X 0.0      S"
    , "1981 JUL  1 =JD 2444786.5  TAI-UTC=  20.0       S + (MJD - 41317.) X 0.0      S"
    , "1982 JUL  1 =JD 2445151.5  TAI-UTC=  21.0       S + (MJD - 41317.) X 0.0      S"
    , "1983 JUL  1 =JD 2445516.5  TAI-UTC=  22.0       S + (MJD - 41317.) X 0.0      S"
    , "1985 JUL  1 =JD 2446247.5  TAI-UTC=  23.0       S + (MJD - 41317.) X 0.0      S"
    , "1988 JAN  1 =JD 2447161.5  TAI-UTC=  24.0       S + (MJD - 41317.) X 0.0      S"
    , "1990 JAN  1 =JD 2447892.5  TAI-UTC=  25.0       S + (MJD - 41317.) X 0.0      S"
    , "1991 JAN  1 =JD 2448257.5  TAI-UTC=  26.0       S + (MJD - 41317.) X 0.0      S"
    , "1992 JUL  1 =JD 2448804.5  TAI-UTC=  27.0       S + (MJD - 41317.) X 0.0      S"
    , "1993 JUL  1 =JD 2449169.5  TAI-UTC=  28.0       S + (MJD - 41317.) X 0.0      S"
    , "1994 JUL  1 =JD 2449534.5  TAI-UTC=  29.0       S + (MJD - 41317.) X 0.0      S"
    , "1996 JAN  1 =JD 2450083.5  TAI-UTC=  30.0       S + (MJD - 41317.) X 0.0      S"
    , "1997 JUL  1 =JD 2450630.5  TAI-UTC=  31.0       S + (MJD - 41317.) X 0.0      S"
    , "1999 JAN  1 =JD 2451179.5  TAI-UTC=  32.0       S + (MJD - 41317.) X 0.0      S"
    , "2006 JAN  1 =JD 2453736.5  TAI-UTC=  33.0       S + (MJD - 41317.) X 0.0      S"
    , "2009 JAN  1 =JD 2454832.5  TAI-UTC=  34.0       S + (MJD - 41317.) X 0.0      S"
    , "2012 JUL  1 =JD 2456109.5  TAI-UTC=  35.0       S + (MJD - 41317.) X 0.0      S"
    , "2015 JUL  1 =JD 2457204.5  TAI-UTC=  36.0       S + (MJD - 41317.) X 0.0      S"
    , "2017 JAN  1 =JD 2457754.5  TAI-UTC=  37.0       S + (MJD - 41317.) X 0.0      S"
    ]
