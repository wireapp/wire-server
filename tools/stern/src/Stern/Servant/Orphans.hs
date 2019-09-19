{-# OPTIONS_GHC -Wno-orphans #-}

module Stern.Servant.Orphans where

import Imports

import Brig.Types.Client
import Brig.Types.Properties
import Brig.Types.Servant.Orphans ()
import Brig.Types.User
import Brig.Types.User.Auth
import Control.Monad.Catch (throwM, catch)
import Data.Aeson (encode)
import Data.ByteString.Conversion as BSC
import Data.LegalHold
import Data.Proxy
import Data.String.Conversions (cs)
import "swagger2" Data.Swagger
import Data.Text.Ascii
import Galley.Types
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import GHC.TypeLits
import Gundeck.Types.Notification
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


instance FromHttpApiData HandlesQuery where
  parseUrlPiece = fmap translate . parseUrlPiece
    where translate (List handles) = HandlesQuery handles

instance FromHttpApiData UserIdsQuery where
  parseUrlPiece = fmap translate . parseUrlPiece
    where translate (List handles) = UserIdsQuery handles


instance ToParamSchema HandlesQuery  where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToParamSchema UserIdsQuery where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToParamSchema InvoiceId where
  toParamSchema _ = toParamSchema (Proxy @Text)


instance ToSchema (SwaggerSchemaUI' dir api) where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)

instance ToSchema (SwaggerUiHtml dir any) where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)

instance ToSchema Swagger where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)


instance ToSchema ConsentLog
instance ToSchema ConsentValue
instance ToSchema Perm
instance ToSchema Permissions
instance ToSchema PhoneUpdate
instance ToSchema SetLegalHoldStatus
instance ToSchema SetSSOStatus
instance ToSchema Team
instance ToSchema TeamBillingInfo
instance ToSchema TeamBillingInfoUpdate
instance ToSchema TeamBinding
instance ToSchema TeamData
instance ToSchema TeamInfo
instance ToSchema TeamMember
instance ToSchema TeamMemberInfo
instance ToSchema TeamStatus
instance ToSchema UserConnectionsByStatus
instance ToSchema UserLegalHoldStatus
instance ToSchema PropertyValue
instance ToSchema MarketoResult
instance ToSchema (AsciiText Printable)
instance ToSchema UserMetaInfo
instance ToSchema UserProperties
instance ToSchema PropertyKey

instance ToSchema QueuedNotification where
  declareNamedSchema = undefined

instance ToSchema Conversation where
  declareNamedSchema = undefined

instance ToSchema Client where
  declareNamedSchema = undefined

instance ToSchema CookieList where
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
