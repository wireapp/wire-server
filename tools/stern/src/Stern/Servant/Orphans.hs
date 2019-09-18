{-# OPTIONS_GHC -Wno-orphans #-}

module Stern.Servant.Orphans where

import Imports

import Control.Monad.Catch (throwM, catch)
import Data.Aeson (Value, encode)
import Data.Id
import Data.Proxy
import Data.String.Conversions (cs)
import "swagger2" Data.Swagger
import Data.UUID as UUID
import Network.HTTP.Types.Status
import Network.Wai.Utilities
import Servant.API
import Servant.Server
import Servant.Swagger
import Servant.Swagger.UI
import Servant.Swagger.UI.Core
import Stern.App
import Stern.Intra
import Stern.Servant.Types

import qualified Data.Metrics.Servant as Metrics


instance FromHttpApiData (Id U) where
  parseUrlPiece = maybe (Left "UUID.fromText failed") (pure . Id) . UUID.fromText

instance ToParamSchema (Id U) where
  toParamSchema _ = toParamSchema (Proxy @UUID)
    -- FUTUREWORK: @& description .~ Just ("User ID" :: Text)@ would be nice here, but will
    -- require a patch to swagger2, I think.

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


instance HasServer api ctx => HasServer (NoSwagger :> api) ctx where
  type ServerT (NoSwagger :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasSwagger (NoSwagger :> api) where
  toSwagger _ = mempty

instance Metrics.RoutesToPaths api => Metrics.RoutesToPaths (NoSwagger :> api) where
  getRoutes = mempty

instance Metrics.RoutesToPaths Raw where
  getRoutes = mempty


instance MonadIntra App where
  type StripException App = App
  throwRpcError = throwM
  catchRpcErrors = (`catch` throwM . translate)
    where
      translate :: Error -> ServantErr
      translate err@(Error s l _) = ServantErr
        { errHTTPCode     = statusCode s
        , errReasonPhrase = cs l
        , errBody         = encode err
        , errHeaders      = [("Content-Type", "application/json")]
        }
