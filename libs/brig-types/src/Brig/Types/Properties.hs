{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Brig.Types.Properties where

import Data.Aeson
import Data.ByteString.Conversion
import Data.Text.Ascii

import qualified Data.Aeson.Parser               as P
import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.ByteString.Lazy            as Lazy

newtype PropertyKey = PropertyKey
    { propertyKeyName :: AsciiPrintable }
    deriving (Eq, Ord, Show, FromByteString, ToByteString, FromJSON, ToJSON)

newtype PropertyValue = PropertyValue
    { propertyValueJson :: Value }
    deriving (Eq, Show)

-- JSON

instance ToJSON PropertyValue where
    toJSON = propertyValueJson

-- Since property values are allowed to be JSON literals,
-- explicit parsing is necessary instead of using the FromJSON instance
-- through decode / eitherDecode et al, which enforce the top-level structure
-- to be an array / object.
parsePropertyValue :: Lazy.ByteString -> Either String PropertyValue
parsePropertyValue = fmap PropertyValue . P.eitherResult . P.parse P.value
