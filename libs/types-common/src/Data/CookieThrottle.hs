{-# LANGUAGE GeneralizedNewtypeDeriving#-}
module Data.CookieThrottle where

import Prelude
import Data.RetryAfter
import qualified Data.Schema as Schema
import Data.Aeson
import qualified Data.Swagger as Swagger
import qualified Data.Swagger.Internal.Schema as SwaggerInternal
import Data.Proxy

-- | Temporal Throttling - The fields are:
--
-- * Min. standard deviation cookie creation
-- * Wait time when the min deviation is violated
--
-- Both fields are in seconds.
data CookieThrottle
  = StdDevThrottle { stdEnv :: StdDev, retryAfter :: RetryAfter}
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON) via Schema.Schema CookieThrottle

newtype StdDev = StdDev {unpack :: Double}
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

instance Schema.ToSchema StdDev where schema = Schema.genericToSchema

instance Swagger.ToSchema StdDev where
 declareNamedSchema = SwaggerInternal.plain . Swagger.paramSchemaToSchema

instance Swagger.ToParamSchema StdDev where
  toParamSchema _ = Swagger.toParamSchema (Proxy @Double)

-- TODO: derive FromJSON & ToJSON via ToSchema
instance Schema.ToSchema CookieThrottle where
  schema = Schema.object "CookieThrottle" $
    StdDevThrottle
      <$> stdEnv Schema..= Schema.field "stdDev" Schema.schema
      <*> retryAfter Schema..= Schema.field "retryAfter" Schema.schema
