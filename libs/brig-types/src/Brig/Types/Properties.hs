{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Brig.Types.Properties where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Text.Ascii

newtype PropertyKey = PropertyKey
    { propertyKeyName :: AsciiPrintable }
    deriving (Eq, Ord, Show, FromByteString, ToByteString, FromJSON, ToJSON, ToJSONKey,
              Generic, Hashable)

newtype PropertyValue = PropertyValue
    { propertyValueJson :: Value }
    deriving (Eq, Show, FromJSON, ToJSON, Generic, Hashable)
