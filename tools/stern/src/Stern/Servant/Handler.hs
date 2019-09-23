module Stern.Servant.Handler where

import Imports hiding (head)

import Brig.Types.Common
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Search as Search
import Brig.Types.User
import Control.Exception (throwIO, ErrorCall(..))
import Control.Lens (view)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Trans.Except
import Data.Aeson (Value, encode)
import Data.ByteString.Conversion (List(List))
import Data.Id
import Data.Proxy
import Data.Range
import Data.String.Conversions (cs)
import "swagger2" Data.Swagger
import GHC.TypeLits (symbolVal)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Utilities (Error(..))
import Network.Wai.Utilities.Server (runHandlers)
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
import Text.Show.Pretty (ppShow)

import qualified Control.Monad.Catch as Catch
import qualified Data.Metrics.Middleware as Metrics
import qualified System.Logger.Class as Log


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
app = (`mkAppGeneric` server)

mkAppGeneric
  :: forall routes (api :: *)
   . ( GenericServant routes (AsServerT App)
     , api ~ ToServant routes AsApi
     , ServerT api App ~ ToServant routes (AsServerT App)
     , HasServer api '[]
     )
  => Env -> routes (AsServerT App) -> Application
mkAppGeneric env srv = mkApp @api env (genericServerT srv)

mkApp
  :: forall (api :: *)
   . HasServer api '[]
  => Env -> ServerT api App -> Application
mkApp env = serve (Proxy @api) . hoistServer (Proxy @api) (appToServantHandler env)


appToServantHandler :: Env -> App a -> Handler a
appToServantHandler env (AppT m) = Handler . ioToExceptT $ m `runReaderT` env
  where
    ioToExceptT :: IO a -> ExceptT ServantErr IO a
    ioToExceptT action = ExceptT $ (Right <$> action) `catch` (`runHandlers`
      [ mkHandler translateError
      , mkHandler translateAny
      ])

    mkHandler :: Exception e => (e -> ServantErr) -> Catch.Handler IO (Either ServantErr a)
    mkHandler trans = Catch.Handler $ pure . Left . trans

    translateError :: Error -> ServantErr
    translateError e@(Error s l _) = servantErr (statusCode s) (cs l) e

    translateAny :: SomeException -> ServantErr
    translateAny e = servantErr 500 "error" e

    servantErr :: Show e => Int -> String -> e -> ServantErr
    servantErr s l e = ServantErr
      { errHTTPCode     = s
      , errReasonPhrase = l
      , errBody         = cs $ ppShow e
      , errHeaders      = [("Content-Type", "text/ascii")]
      }

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
  , _apiDeleteUser                = mutuallyExclusive . apiDeleteUser
  , _apiCheckBlacklistStatus      = mutuallyExclusive apiCheckBlacklistStatus
  , _apiBlacklistUser             = mutuallyExclusive apiBlacklistUser
  , _apiWhitelistUser             = mutuallyExclusive apiWhitelistUser
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

apiUsersByEmail :: Email -> MonadIntra m => m [UserAccount]
apiUsersByEmail = Intra.getUserProfilesByIdentity . Left

apiUsersByPhone :: Phone -> MonadIntra m => m [UserAccount]
apiUsersByPhone = Intra.getUserProfilesByIdentity . Right

apiUsersByIds :: UserIdsQuery -> MonadIntra m => m [UserAccount]
apiUsersByIds (UserIdsQuery uids) = Intra.getUserProfiles $ Left uids

apiUsersByHandles :: HandlesQuery -> MonadIntra m => m [UserAccount]
apiUsersByHandles (HandlesQuery handles) = Intra.getUserProfiles $ Right handles

apiUserConnections :: UserId -> MonadIntra m => m UserConnectionsByStatus
apiUserConnections = fmap groupByStatus . Intra.getUserConnections

apiUsersConnections :: UserIdsQuery -> MonadIntra m => m [ConnectionStatus]
apiUsersConnections (UserIdsQuery uids) = Intra.getUsersConnections (List uids)

apiUserSearchOnBehalf
  :: UserId -> Text -> Maybe (Range 1 100 Int32)
  -> MonadIntra m => m (SearchResult Search.Contact)
apiUserSearchOnBehalf uid query size =
    Intra.getContacts uid query (maybe 30 fromRange size)

apiRevokeIdentity :: Maybe Email -> Maybe Phone -> MonadIntra m => m NoContent
apiRevokeIdentity em ph = do
  mutuallyExclusive Intra.revokeIdentity em ph
  pure NoContent

apiChangeEmail :: UserId -> EmailUpdate -> MonadIntra m => m NoContent
apiChangeEmail uid upd = do
  Intra.changeEmail uid upd
  pure NoContent

apiChangePhone :: UserId -> PhoneUpdate -> MonadIntra m => m NoContent
apiChangePhone uid upd = noContent $ Intra.changePhone uid upd

apiDeleteUser :: UserId -> Either Email Phone -> MonadIntra m => m NoContent
apiDeleteUser uid emailOrPhone = do
  usrs <- Intra.getUserProfilesByIdentity emailOrPhone
  case usrs of
    ((accountUser -> u) : _) ->
      if userId u == uid
      then do
        canBeDel <- Intra.getUserBindingTeam uid >>= \case
            Nothing  -> pure True
            Just tid -> Intra.canBeDeleted uid tid
        unless canBeDel $
            throwRpcError $ Error status403 "no-other-owner"
                "You are trying to remove or downgrade the last owner. \
                \Promote another team member before proceeding."
        Log.info $ userMsg uid . Log.msg (Log.val "Deleting account")
        void $ Intra.deleteAccount uid
        return NoContent
      else do
        throwRpcError $ Error status400 "match-error" "email or phone did not match UserId"
    _ -> throwRpcError $ Error status404 "not-found" "Not found"

apiCheckBlacklistStatus :: Either Email Phone -> MonadIntra m => m BlackListStatus
apiCheckBlacklistStatus emailOrPhone = Intra.isBlacklisted emailOrPhone >>= \case
  True  -> pure BlackListed
  False -> throwRpcError $ Error status404 "not-blacklisted" (cs $ encode NotBlackListed)

apiBlacklistUser :: Either Email Phone -> MonadIntra m => m NoContent
apiBlacklistUser = noContent . Intra.setBlacklistStatus True

apiWhitelistUser :: Either Email Phone -> MonadIntra m => m NoContent
apiWhitelistUser = noContent . Intra.setBlacklistStatus False

apiTeamInfoByEmail :: Email -> MonadIntra m => m TeamInfo
apiTeamInfoByEmail em = do
  acc <- (listToMaybe <$> Intra.getUserProfilesByIdentity (Left em)) >>= handleUser
  tid <- (Intra.getUserBindingTeam . userId . accountUser $ acc)     >>= handleTeam
  Intra.getTeamInfo tid
  where
    handleUser = ifNothing (Error status404 "no-user" "No such user with that email")
    handleTeam = ifNothing (Error status404 "no-binding-team" "No such binding team")

apiTeamInfo :: TeamId -> MonadIntra m => m TeamInfo
apiTeamInfo = Intra.getTeamInfo

apiGetFeatureStatusLegalHold :: TeamId -> MonadIntra m => m SetLegalHoldStatus
apiGetFeatureStatusLegalHold = Intra.getLegalholdStatus

apiPutFeatureStatusLegalHold :: TeamId -> SetLegalHoldStatus -> MonadIntra m => m NoContent
apiPutFeatureStatusLegalHold tid = noContent . Intra.setLegalholdStatus tid

apiGetFeatureStatusSSO :: TeamId -> MonadIntra m => m SetSSOStatus
apiGetFeatureStatusSSO = Intra.getSSOStatus

apiPutFeatureStatusSSO :: TeamId -> SetSSOStatus -> MonadIntra m => m NoContent
apiPutFeatureStatusSSO tid = noContent . Intra.setSSOStatus tid

apiGetTeamInvoice :: TeamId -> InvoiceId -> MonadIntra m => m NoContent
apiGetTeamInvoice _ _ = throwM $ ErrorCall "apiGetTeamInvoice"
  -- 307 redirect or only status200 with url in body?  check out how stern behaves in production.

apiGetTeamBilling :: TeamId -> MonadIntra m => m TeamBillingInfo
apiGetTeamBilling _ = throwM $ ErrorCall "apiGetTeamBilling"

apiPutTeamBilling :: TeamId -> TeamBillingInfoUpdate -> MonadIntra m => m TeamBillingInfo
apiPutTeamBilling _ _ = throwM $ ErrorCall "apiPutTeamBilling"

apiPostTeamBilling :: TeamId -> TeamBillingInfo -> MonadIntra m => m TeamBillingInfo
apiPostTeamBilling _ _ = throwM $ ErrorCall "apiPostTeamBilling"

apiGetConsentLog :: Email -> MonadIntra m => m ConsentLog
apiGetConsentLog _ = throwM $ ErrorCall "apiGetConsentLog"

apiGetMetaInfo :: UserId -> MonadIntra m => m UserMetaInfo
apiGetMetaInfo _ = throwM $ ErrorCall "apiGetMetaInfo"


----------------------------------------------------------------------
-- Utilities

groupByStatus :: [UserConnection] -> UserConnectionsByStatus
groupByStatus conns = UserConnectionsByStatus
    { _ucbsAccepted = byStatus Accepted conns
    , _ucbsSent     = byStatus Sent conns
    , _ucbsPending  = byStatus Pending conns
    , _ucbsBlocked  = byStatus Blocked conns
    , _ucbsIgnored  = byStatus Ignored conns
    , _ucbsTotal    = length conns
    }
  where
    byStatus :: Relation -> [UserConnection] -> Int
    byStatus s = length . filter ((==) s . ucStatus)

ifNothing :: Error -> Maybe a -> MonadIntra m => m a
ifNothing e = maybe (throwRpcError e) return

noSuchUser :: Maybe a -> MonadIntra m => m a
noSuchUser = ifNothing (Error status404 "no-user" "No such user")

mutuallyExclusive :: MonadIntra m => (Either a b -> m c) -> Maybe a -> Maybe b -> m c
mutuallyExclusive _ Nothing  Nothing
  = throwRpcError $ Error status400 "missing-params" "Please pick one of the two available query params."
mutuallyExclusive _ (Just _) (Just _)
  = throwRpcError $ Error status400 "mutally-exclusive-params" "Please pick one of the two available query params."
mutuallyExclusive f (Just l) Nothing
  = f (Left l)
mutuallyExclusive f Nothing  (Just r)
  = f (Right r)

noContent :: Monad m => m a -> m NoContent
noContent action = action >> pure NoContent
