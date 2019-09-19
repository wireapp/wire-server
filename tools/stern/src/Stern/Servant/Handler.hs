module Stern.Servant.Handler where

import Imports hiding (head)

import Brig.Types.Common
import Brig.Types.Intra
import Brig.Types.User
import Control.Exception (throwIO)
import Control.Lens (view)
import Control.Monad.Catch (catch)
import Control.Monad.Trans.Except
import Data.Aeson (Value)
import Data.Id
import Data.Proxy
import Data.Range
import Data.String.Conversions (cs)
import "swagger2" Data.Swagger
import GHC.TypeLits (symbolVal)
import Network.Wai
import Servant.API.ContentTypes
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic
import Servant.Swagger
import Servant.Swagger.UI
import Stern.App hiding (Handler, runHandler, App)
import Stern.Intra as Intra
import Stern.Servant.Orphans ()
import Stern.Servant.Types
import Stern.Types

import qualified Data.Metrics.Middleware as Metrics


----------------------------------------------------------------------
-- middleware

middleware :: Env -> Middleware
middleware env innerapp req cont = if rootPrefix `isPrefixOf` pathInfo req
  then app env req cont
  else innerapp req cont
  where
    rootPrefix :: [Text]
    rootPrefix = [cs $ symbolVal (Proxy @RootPrefix)]


----------------------------------------------------------------------
-- custom handler type, natural transformation

-- | Like Stern.App.AppT, but with a servant handler inside.
type App = AppT IO

app :: Env -> Application
app env = serve
    (Proxy @(ToServant API AsApi))
    (hoistServer (Proxy @(ToServant API AsApi)) (appToServantHandler env) (genericServerT server))

appToServantHandler :: Env -> App a -> Handler a
appToServantHandler env (AppT m) = Handler . ioToExceptT $ m `runReaderT` env
  where
    ioToExceptT :: IO a -> ExceptT ServantErr IO a
    ioToExceptT action = ExceptT $ (Right <$> action) `catch` \(e :: ServantErr) -> pure (Left e)

servantHandlerToApp :: Handler a -> App a
servantHandlerToApp (Handler exc) = AppT . ReaderT . const . ioToExceptT $ exc
  where
    ioToExceptT :: ExceptT ServantErr IO a -> IO a
    ioToExceptT action = either throwIO pure =<< runExceptT action


----------------------------------------------------------------------
-- swagger docs

swaggerDoc :: Swagger
swaggerDoc = toSwagger (genericApi (Proxy :: Proxy API))


----------------------------------------------------------------------
-- handlers

server :: API (AsServerT App)
server = API
  { _apiSwaggerDoc = hoistServer (Proxy @(SwaggerSchemaUI "api-docs" "swagger.json")) servantHandlerToApp
                     (swaggerSchemaUIServer swaggerDoc)

  , _apiInternalGetStatus  = apiInternalGetStatus
  , _apiInternalHeadStatus = apiInternalHeadStatus
  , _apiInternalMonitoring = apiInternalMonitoring

  , _apiSuspendUser               = apiSuspendUser
  , _apiUnsuspendUser             = apiUnsuspendUser
  , _apiUsersByEmail              = apiUsersByEmail
  , _apiUsersByPhone              = apiUsersByPhone
  , _apiUsersByIds                = apiUsersByIds
  , _apiUsersByHandles            = apiUsersByHandles
  , _apiUserConnections           = apiUserConnections
  , _apiUsersConnections          = apiUsersConnections
  , _apiUserSearchOnBehalf        = apiUserSearchOnBehalf
  , _apiRevokeIdentity            = apiRevokeIdentity
  , _apiChangeEmail               = apiChangeEmail
  , _apiChangePhone               = apiChangePhone
  , _apiDeleteUser                = apiDeleteUser
  , _apiCheckBlacklistStatus      = apiCheckBlacklistStatus
  , _apiBlacklistUser             = apiBlacklistUser
  , _apiWhitelistUser             = apiWhitelistUser
  , _apiTeamInfoByEmail           = apiTeamInfoByEmail
  , _apiTeamInfo                  = apiTeamInfo
  , _apiGetFeatureStatusLegalHold = apiGetFeatureStatusLegalHold
  , _apiPutFeatureStatusLegalHold = apiPutFeatureStatusLegalHold
  , _apiGetFeatureStatusSSO       = apiGetFeatureStatusSSO
  , _apiPutFeatureStatusSSO       = apiPutFeatureStatusSSO
  , _apiGetTeamInvoice            = apiGetTeamInvoice
  , _apiGetTeamBilling            = apiGetTeamBilling
  , _apiPutTeamBilling            = apiPutTeamBilling
  , _apiPostTeamBilling           = apiPostTeamBilling
  , _apiGetConsentLog             = apiGetConsentLog
  , _apiGetMetaInfo               = apiGetMetaInfo
  }


apiInternalGetStatus :: Monad m => m NoContent
apiInternalGetStatus = pure NoContent

apiInternalHeadStatus :: Monad m => m NoContent
apiInternalHeadStatus = pure NoContent

apiInternalMonitoring :: (MonadIO m, MonadReader Env m) => m Value
apiInternalMonitoring = view metrics >>= Metrics.render


apiSuspendUser :: UserId -> MonadIntra m => m NoContent
apiSuspendUser uid = do
  Intra.putUserStatus Suspended uid
  pure NoContent

apiUnsuspendUser :: UserId -> MonadIntra m => m NoContent
apiUnsuspendUser uid = do
  Intra.putUserStatus Active uid
  pure NoContent

apiUsersByEmail :: Maybe Email -> MonadIntra m => m [UserAccount]
apiUsersByEmail = undefined

apiUsersByPhone :: Maybe Brig.Types.Common.Phone -> MonadIntra m => m [UserAccount]
apiUsersByPhone = undefined

apiUsersByIds :: Maybe UserIdsQuery -> MonadIntra m => m [UserAccount]
apiUsersByIds = undefined

apiUsersByHandles :: Maybe HandlesQuery -> MonadIntra m => m [UserAccount]
apiUsersByHandles = undefined

apiUserConnections :: UserId -> MonadIntra m => m UserConnectionsByStatus
apiUserConnections = undefined

apiUsersConnections :: Maybe UserIdsQuery -> MonadIntra m => m [ConnectionStatus]
apiUsersConnections = undefined

apiUserSearchOnBehalf
  :: UserId -> Maybe Text -> Maybe (Range 1 100 Int32)
  -> MonadIntra m => m [ConnectionStatus]
apiUserSearchOnBehalf = undefined

apiRevokeIdentity :: Maybe Email -> Maybe Phone -> MonadIntra m => m NoContent
apiRevokeIdentity = undefined

apiChangeEmail :: UserId -> EmailUpdate -> MonadIntra m => m NoContent
apiChangeEmail = undefined

apiChangePhone :: UserId -> PhoneUpdate -> MonadIntra m => m NoContent
apiChangePhone = undefined

apiDeleteUser :: UserId -> Maybe Email -> Maybe Phone -> MonadIntra m => m NoContent
apiDeleteUser = undefined

apiCheckBlacklistStatus :: Maybe Email -> Maybe Phone -> MonadIntra m => m NoContent
apiCheckBlacklistStatus = undefined

apiBlacklistUser :: Maybe Email -> Maybe Phone -> MonadIntra m => m NoContent
apiBlacklistUser = undefined

apiWhitelistUser :: Maybe Email -> Maybe Phone -> MonadIntra m => m NoContent
apiWhitelistUser = undefined

apiTeamInfoByEmail :: Maybe Email -> MonadIntra m => m TeamInfo
apiTeamInfoByEmail = undefined

apiTeamInfo :: TeamId -> MonadIntra m => m TeamInfo
apiTeamInfo = undefined

apiGetFeatureStatusLegalHold :: TeamId -> MonadIntra m => m SetLegalHoldStatus
apiGetFeatureStatusLegalHold = undefined

apiPutFeatureStatusLegalHold :: TeamId -> SetLegalHoldStatus -> MonadIntra m => m NoContent
apiPutFeatureStatusLegalHold = undefined

apiGetFeatureStatusSSO :: TeamId -> MonadIntra m => m SetSSOStatus
apiGetFeatureStatusSSO = undefined

apiPutFeatureStatusSSO :: TeamId -> SetSSOStatus -> MonadIntra m => m NoContent
apiPutFeatureStatusSSO = undefined

apiGetTeamInvoice :: TeamId -> InvoiceId -> MonadIntra m => m NoContent
apiGetTeamInvoice = undefined

apiGetTeamBilling :: TeamId -> MonadIntra m => m TeamBillingInfo
apiGetTeamBilling = undefined

apiPutTeamBilling :: TeamId -> TeamBillingInfoUpdate -> MonadIntra m => m TeamBillingInfo
apiPutTeamBilling = undefined

apiPostTeamBilling :: TeamId -> TeamBillingInfo -> MonadIntra m => m TeamBillingInfo
apiPostTeamBilling = undefined

apiGetConsentLog :: Maybe Email -> MonadIntra m => m ConsentLog
apiGetConsentLog = undefined

apiGetMetaInfo :: Maybe UserId -> MonadIntra m => m UserMetaInfo
apiGetMetaInfo = undefined
