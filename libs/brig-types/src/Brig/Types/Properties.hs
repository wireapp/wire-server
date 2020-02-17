{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Properties where

import Data.Aeson
import Data.ByteString.Conversion
import Data.Hashable (Hashable)
import Data.Text.Ascii
import Imports

newtype PropertyKey
  = PropertyKey
      {propertyKeyName :: AsciiPrintable}
  deriving
    ( Eq,
      Ord,
      Show,
      FromByteString,
      ToByteString,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      Generic,
      Hashable
    )

newtype PropertyValue
  = PropertyValue
      {propertyValueJson :: Value}
  deriving (Eq, Show, FromJSON, ToJSON, Generic, Hashable)

newtype PropertyKeysAndValues = PropertyKeysAndValues [(PropertyKey, PropertyValue)]
  deriving (Eq, Show, Generic, Hashable)

instance ToJSON PropertyKeysAndValues where
  toJSON (PropertyKeysAndValues kvs) = object [toText k .= v | (PropertyKey k, v) <- kvs]
