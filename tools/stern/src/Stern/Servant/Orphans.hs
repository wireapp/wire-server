{-# OPTIONS_GHC -Wno-orphans #-}

module Stern.Servant.Orphans where

import Imports

import Brig.Types.User
import Brig.Types.Intra
import Control.Monad.Catch (throwM, catch)
import Data.Aeson (Value, encode)
import Data.Id
import Data.Proxy
import Data.Range
import Data.String.Conversions (cs)
import "swagger2" Data.Swagger
import Data.UUID as UUID
import GHC.TypeLits
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
import Stern.Types

import qualified Data.Metrics.Servant as Metrics


instance FromHttpApiData (Id any) where
  parseUrlPiece = maybe (Left "UUID.fromText failed") (pure . Id) . UUID.fromText

instance FromHttpApiData HandlesQuery where
  parseUrlPiece = undefined

instance FromHttpApiData UserIdsQuery where
  parseUrlPiece = undefined

instance FromHttpApiData Email where
  parseUrlPiece = undefined

instance FromHttpApiData Phone where
  parseUrlPiece = undefined

instance FromHttpApiData (Range (lower :: Nat) (upper :: Nat) Int32) where
  parseUrlPiece = undefined

instance FromHttpApiData InvoiceId where
  parseUrlPiece = undefined


instance ToParamSchema (Id any) where
  toParamSchema _ = toParamSchema (Proxy @UUID)
    -- FUTUREWORK: @& description .~ Just (... :: Text)@ would be nice here, but will require
    -- a patch to swagger2, I think.  and we need to think of a clever way to get from "any"
    -- in the instance head back to "AnyId".  (the dumb way would also work, just writing 5
    -- instances.)

instance ToParamSchema HandlesQuery where
  toParamSchema = undefined

instance ToParamSchema UserIdsQuery where
  toParamSchema = undefined

instance ToParamSchema Email where
  toParamSchema = undefined

instance ToParamSchema Phone where
  toParamSchema = undefined

instance ToParamSchema InvoiceId where
  toParamSchema = undefined


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

instance ToSchema UserAccount where
  declareNamedSchema = undefined

instance ToSchema ConnectionStatus where
  declareNamedSchema = undefined

instance ToSchema TeamInfo where
  declareNamedSchema = undefined

instance ToSchema SetLegalHoldStatus where
  declareNamedSchema = undefined

instance ToSchema SetSSOStatus where
  declareNamedSchema = undefined

instance ToSchema EmailUpdate where
  declareNamedSchema = undefined

instance ToSchema PhoneUpdate where
  declareNamedSchema = undefined

instance ToSchema TeamBillingInfo where
  declareNamedSchema = undefined

instance ToSchema TeamBillingInfoUpdate where
  declareNamedSchema = undefined


instance HasSwagger (NoSwagger :> api) where
  toSwagger _ = mempty

instance HasSwagger ((SwaggerDesc (sym :: Symbol) thing) :> api) where
  toSwagger = undefined

instance HasSwagger api => HasSwagger (Notes (sym :: Symbol) :> api) where
  toSwagger = undefined


instance HasServer api ctx => HasServer (NoSwagger :> api) ctx where
  type ServerT (NoSwagger :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasServer (something :> api) ctx => HasServer (SwaggerDesc (sym :: Symbol) something :> api) ctx where
  type ServerT (SwaggerDesc sym something :> api) m = ServerT (something :> api) m
  route _ = route (Proxy @(something :> api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(something :> api))

instance HasServer api ctx => HasServer (Notes (sym :: Symbol) :> api) ctx where
  type ServerT (Notes sym :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)


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
