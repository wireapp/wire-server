{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Wire.Data.Timeout where

import Data.Aeson
import Data.Aeson.Types
import Data.Data (Proxy (..))
import Data.Int
import Data.Schema
import Data.Scientific
import qualified Data.Swagger as Swagger
import qualified Data.Swagger.Internal.Schema as Swagger
import qualified Data.Yaml as Y
import Test.QuickCheck
import Prelude
import Data.Time.Clock

newtype Timeout = Timeout
  { timeoutDiff :: NominalDiffTime
  }
  deriving newtype (Eq, Enum, Ord, Num, Real, Fractional, RealFrac, Show)

instance Arbitrary Timeout where
  arbitrary = Timeout . fromInteger . fromIntegral
      <$> chooseBoundedIntegral (minBound :: Int64, maxBound :: Int64)

instance Read Timeout where
  readsPrec i s =
    case readsPrec i s of
      [(x :: Int, s')] -> [(Timeout (fromIntegral x), s')]
      _ -> []

instance FromJSON Timeout where
  parseJSON (Y.Number n) =
    let defaultV = 3600
        bounded = toBoundedInteger n :: Maybe Int64
     in pure $
          Timeout $
            fromIntegral @Int $
              maybe defaultV fromIntegral bounded
  parseJSON v = typeMismatch "activationTimeout" v

instance ToJSON Timeout where
  toJSON (Timeout t) = toJSON t

instance ToSchema Timeout where
  schema = genericToSchema

instance Swagger.ToSchema Timeout where
  declareNamedSchema = Swagger.plain . Swagger.paramSchemaToSchema

instance Swagger.ToParamSchema Timeout where
  toParamSchema _ = Swagger.toParamSchema (Proxy @Int64)
