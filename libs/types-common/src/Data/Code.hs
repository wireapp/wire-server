{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Types for verification codes.
module Data.Code where

import Data.Aeson hiding (Value)
import Data.ByteString.Conversion
import Data.Int
import Data.Range
import Data.Scientific (toBoundedInteger)
import Data.Text.Ascii
import Data.Time.Clock

#ifdef WITH_CQL
import Database.CQL.Protocol hiding (unpack, Value)
#endif

-- | A scoped identifier for a 'Value' with an associated 'Timeout'.
newtype Key = Key { asciiKey :: Range 20 20 AsciiBase64Url }
    deriving (Eq, Show, FromJSON, ToJSON, FromByteString, ToByteString)

-- | A secret value bound to a 'Key' and a 'Timeout'.
newtype Value = Value { asciiValue :: Range 6 20 AsciiBase64Url }
    deriving (Eq, Show, FromJSON, ToJSON, FromByteString, ToByteString)

newtype Timeout = Timeout
    { timeoutDiffTime :: NominalDiffTime }
    deriving (Eq, Show, Ord, Enum, Num, Fractional, Real, RealFrac)

-- | A 'Timeout' is rendered as an integer representing the number of seconds remaining.
instance ToByteString Timeout where
    builder (Timeout t) = builder (round t :: Int32)

-- | A 'Timeout' is rendered in JSON as an integer representing the
-- number of seconds remaining.
instance ToJSON Timeout where
    toJSON (Timeout t) = toJSON (round t :: Int32)

-- | A 'Timeout' is parsed from JSON as an integer representing the
-- number of seconds remaining.
instance FromJSON Timeout where
    parseJSON = withScientific "Timeout" $ \n ->
        let t = toBoundedInteger n :: Maybe Int32 in
        maybe (fail "Invalid timeout value")
              (pure . Timeout . fromIntegral)
              t

#ifdef WITH_CQL
deriving instance Cql Key
deriving instance Cql Value
#endif
