{-# OPTIONS_GHC -Wno-orphans #-}

module Stern.Servant.Orphans where

import Imports

import Data.Aeson (Value)
import Data.Id
import Data.Proxy
import "swagger2" Data.Swagger
import Data.UUID as UUID
import Servant.API
import Servant.Swagger.UI
import Servant.Swagger.UI.Core


instance FromHttpApiData (Id U) where
  parseUrlPiece = maybe (Left "UUID.fromText failed") (pure . Id) . UUID.fromText

instance ToParamSchema (Id U) where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToSchema (SwaggerSchemaUI' dir api) where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)

instance ToSchema (SwaggerUiHtml dir any) where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)

instance ToSchema Swagger where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)

instance ToSchema NoContent where
  declareNamedSchema _ = declareNamedSchema (Proxy @())  -- TODO: is there a more accurate way to do this?

instance ToSchema Value where
  declareNamedSchema _ = declareNamedSchema (Proxy @())  -- TODO: is there a more accurate way to do this?
