{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Json.Util
    ( append
    , toFieldName
    , (#)
    , UTCTimeMillis (..)
    , ToJSONObject  (..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char (toLower, isUpper)
import Data.Time.Clock
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Text (pack)

append :: Pair -> [Pair] -> [Pair]
append (_, Null) pp = pp
append p         pp = p:pp
{-# INLINE append #-}

infixr 5 #

(#) :: Pair -> [Pair] -> [Pair]
(#) = append
{-# INLINE (#) #-}

-----------------------------------------------------------------------------
-- UTCTimeMillis

-- | A newtype wrapper for 'UTCTime' that formats timestamps in JSON with
-- millisecond precision instead of the default picosecond precision.
newtype UTCTimeMillis = UTCTimeMillis UTCTime

instance ToJSON UTCTimeMillis where
    toJSON (UTCTimeMillis t) = String $ pack $ formatTime defaultTimeLocale format t
      where
        format = "%FT%T." ++ formatMillis t ++ "Z"
        formatMillis = take 3 . formatTime defaultTimeLocale "%q"

instance FromJSON UTCTimeMillis where
    parseJSON = fmap UTCTimeMillis . parseJSON

-----------------------------------------------------------------------------
-- ToJSONObject

class ToJSONObject a where
    toJSONObject :: a -> Object

instance ToJSONObject Object where
    toJSONObject = id

-----------------------------------------------------------------------------
-- toFieldName

-- | Convenient helper to convert field names to use as JSON fields.
-- it removes the prefix (assumed to be anything before an uppercase
-- character) and lowers the first character
--
-- Example:
-- newtype TeamName = TeamName { tnName :: Text }
-- deriveJSON toFieldName ''TeamName
--
-- would generate {To/From}JSON instances where
-- the field name is "name"

toFieldName :: Options
toFieldName = defaultOptions{ fieldLabelModifier = lowerFirst . dropPrefix }
  where
    lowerFirst :: String -> String
    lowerFirst (x:xs) = toLower x : xs
    lowerFirst []     = ""

    dropPrefix :: String -> String
    dropPrefix = dropWhile (not . isUpper)
