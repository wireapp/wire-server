module Util.Timeout
  ( Timeout (..),
    module Data.Time.Clock,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.Time.Clock
import Imports
import Test.QuickCheck (Arbitrary (arbitrary), choose)

newtype Timeout = Timeout
  { timeoutDiff :: NominalDiffTime
  }
  deriving newtype (Eq, Enum, Ord, Num, Real, Fractional, RealFrac, Show)

instance Arbitrary Timeout where
  arbitrary = Timeout . fromIntegral <$> choose (60 :: Int, 10 * 24 * 3600)

instance Read Timeout where
  readsPrec i s =
    case readsPrec i s of
      [(x :: Int, s')] -> [(Timeout (fromIntegral x), s')]
      _ -> []

instance FromJSON Timeout where
  parseJSON (Number n) =
    let defaultV = 3600
        bounded = toBoundedInteger n :: Maybe Int64
     in pure $
          Timeout $
            fromIntegral @Int $
              maybe defaultV fromIntegral bounded
  parseJSON v = typeMismatch "activationTimeout" v
