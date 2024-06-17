module Federator.Interpreter
  ( runWaiErrors,
    serveServant,
  )
where

import Control.Lens
import Control.Monad.Codensity
import Control.Monad.Except
import Data.Aeson (encode)
import Data.Id
import Data.Kind
import Data.Metrics.Servant qualified as Metrics
import Data.Text qualified as T
import Data.Text.Lazy qualified as LText
import Federator.Discovery
import Federator.Env
import Federator.Error
import Federator.Error.ServerError
import Federator.Metrics (Metrics, interpretMetrics)
import Federator.Options
import Federator.Remote
import Federator.Service
import Federator.Validation
import HTTP2.Client.Manager (Http2Manager)
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Utilities (getRequestId)
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.Server (federationRequestIdHeaderName, requestIdMiddleware)
import Network.Wai.Utilities.Server qualified as Wai
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.TinyLog
import Servant (ServerError (..), serve)
import Servant hiding (ServerError, respond, serve)
import Servant.Client (mkClientEnv)
import Servant.Client.Core
import Util.Options (Endpoint (..))
import Wire.API.FederationUpdate qualified as FedUp (getFederationDomainConfigs)
import Wire.API.MakesFederatedCall (Component (Brig))
import Wire.API.Routes.FederationDomainConfig qualified as FedUp (FederationDomainConfigs)
import Wire.Network.DNS.Effect
import Wire.Sem.Logger.TinyLog

class ErrorEffects (ee :: [Type]) r where
  type Row ee :: EffectRow
  runWaiErrorsEither ::
    Sem (Append (Row ee) r) (Either Wai.Error a) ->
    Sem r (Either Wai.Error a)

runWaiErrors ::
  forall ee r a.
  (ErrorEffects ee r, Member (Error Servant.ServerError) r) =>
  Sem (Append (Row ee) r) a ->
  Sem r a
runWaiErrors action = do
  x <- runWaiErrorsEither @ee . fmap Right $ action
  case x of
    Left e -> throw $ waiToServant e
    Right a -> pure a

instance ErrorEffects '[] r where
  type Row '[] = '[]
  runWaiErrorsEither = id

instance
  ( Member TinyLog (Append (Row ee) r),
    AsWai e,
    ErrorEffects ee r
  ) =>
  ErrorEffects (e ': ee) r
  where
  type Row (e ': ee) = (Error e ': Row ee)
  runWaiErrorsEither action = do
    runWaiErrorsEither @ee $ runWaiErrorEither @e action

runWaiErrorEither ::
  (AsWai e, Member TinyLog r) =>
  Sem (Error e ': r) (Either Wai.Error a) ->
  Sem r (Either Wai.Error a)
runWaiErrorEither =
  fmap join
    . runError
    . flip catch logError
    . mapError toWai
    . raiseUnder
  where
    logError ::
      ( Member (Error Wai.Error) r,
        Member TinyLog r
      ) =>
      Wai.Error ->
      Sem r a
    logError e = do
      err $ Wai.logErrorMsg e
      throw e

serveServant ::
  forall (api :: Type).
  (HasServer api '[], Metrics.RoutesToPaths api) =>
  Env ->
  Int ->
  ServerT api (Sem AllEffects) ->
  IO ()
serveServant env port server = do
  let hoistApp :: RequestId -> Server api
      hoistApp rid =
        hoistServerWithContext (Proxy @api) (Proxy @'[]) (runFederator env rid) server
  Warp.run port
    . requestIdMiddleware env._applog federationRequestIdHeaderName
    . Wai.catchErrors (view applog env) federationRequestIdHeaderName
    . Metrics.servantPrometheusMiddleware (Proxy @api)
    $ app hoistApp
  where
    app :: (RequestId -> Server api) -> Wai.Application
    app mkServerFromReqId req cont = do
      let rid = getRequestId federationRequestIdHeaderName req
      serve (Proxy @api) (mkServerFromReqId rid) req cont

type AllEffects =
  '[ Metrics,
     Remote,
     DiscoverFederator,
     DNSLookup, -- needed by DiscoverFederator
     ServiceStreaming,
     Input RunSettings,
     Input Http2Manager, -- needed by Remote
     Input FedUp.FederationDomainConfigs, -- needed for the domain list and federation policy.
     Input Env, -- needed by Service
     Input RequestId,
     Error ValidationError,
     Error RemoteError,
     Error Federator.Error.ServerError.ServerError,
     Error DiscoveryFailure,
     Error Servant.ServerError,
     TinyLog,
     Embed IO,
     Embed (Codensity IO)
   ]

runFederator :: Env -> RequestId -> Sem AllEffects a -> Handler a
runFederator env rid =
  Handler
    . ExceptT
    . lowerCodensity
    . runM
    . runEmbedded (liftIO @(Codensity IO))
    . loggerToTinyLogReqId rid (view applog env)
    . runError
    . runWaiErrors
      @'[ ValidationError,
          RemoteError,
          Federator.Error.ServerError.ServerError,
          DiscoveryFailure
        ]
    . runInputConst rid
    . runInputConst env
    . runInputSem (embed @IO (getFederationDomainConfigs env))
    . runInputSem (embed @IO (readIORef (view http2Manager env)))
    . runInputConst (view runSettings env)
    . interpretServiceHTTP
    . runDNSLookupWithResolver (view dnsResolver env)
    . runFederatorDiscovery
    . interpretRemote
    . interpretMetrics

waiToServant :: Wai.Error -> Servant.ServerError
waiToServant waierr =
  ServerError
    { errHTTPCode = HTTP.statusCode (Wai.code waierr),
      errReasonPhrase = LText.unpack (Wai.label waierr),
      errBody = encode waierr,
      errHeaders = [("Content-Type", "application/json")]
    }

getFederationDomainConfigs :: Env -> IO FedUp.FederationDomainConfigs
getFederationDomainConfigs env = do
  let mgr = env ^. httpManager
      Endpoint h p = env ^. service $ Brig
      baseurl = BaseUrl Http (T.unpack h) (fromIntegral p) ""
      clientEnv = mkClientEnv mgr baseurl
  FedUp.getFederationDomainConfigs clientEnv >>= \case
    Right v -> pure v
    Left e -> error $ show e
