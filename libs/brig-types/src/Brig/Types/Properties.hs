{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Brig.Types.Properties where

import Imports
import Data.Hashable (Hashable)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Text.Ascii

newtype PropertyKey = PropertyKey
    { propertyKeyName :: AsciiPrintable }
    deriving (Eq, Ord, Show, FromByteString, ToByteString, FromJSON, ToJSON,
              FromJSONKey, ToJSONKey, Generic, Hashable)

newtype PropertyValue = PropertyValue
    { propertyValueJson :: Value }
    deriving (Eq, Show, FromJSON, ToJSON, Generic, Hashable)

newtype PropertyKeysAndValues = PropertyKeysAndValues [(PropertyKey, PropertyValue)]
    deriving (Eq, Show, Generic, Hashable)

instance ToJSON PropertyKeysAndValues where
    toJSON (PropertyKeysAndValues kvs) = object [ toText k .= v | (PropertyKey k, v) <- kvs ]
