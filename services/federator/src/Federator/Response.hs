-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Federator.Response
  ( defaultHeaders,
    serveServant,
    serveServant1,
    runFederator,
    runFederatorIO,
    runWaiError,
    runWaiErrors,
    streamingResponseToWai,
    AllEffects1,
  )
where

import Control.Lens
import Control.Monad.Codensity
import Data.Aeson (encode)
import Data.ByteString.Builder
import Data.Id
import Data.Kind
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
import Network.Wai (Middleware)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Utilities (getRequestId)
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.Server (federationRequestIdHeaderName)
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
import Servant.Server.Generic
import Servant.Types.SourceT
import Util.Options (Endpoint (..))
import Wire.API.FederationUpdate qualified as FedUp (getFederationDomainConfigs)
import Wire.API.MakesFederatedCall (Component (Brig))
import Wire.API.Routes.FederationDomainConfig qualified as FedUp (FederationDomainConfigs)
import Wire.Network.DNS.Effect
import Wire.Sem.Logger.TinyLog

defaultHeaders :: [HTTP.Header]
defaultHeaders = [("Content-Type", "application/json")]

class ErrorEffects (ee :: [Type]) r where
  type Row ee :: EffectRow
  runWaiErrors ::
    Sem (Append (Row ee) r) Wai.Response ->
    Sem r Wai.Response

  runWaiErrorsEither ::
    Sem (Append (Row ee) r) (Either Wai.Error a) ->
    Sem r (Either Wai.Error a)

-- TODO: Rename
runWaiErrorsEitherTheGreat ::
  forall ee r a.
  ErrorEffects ee r =>
  Sem (Append (Row ee) r) a ->
  Sem r (Either Wai.Error a)
runWaiErrorsEitherTheGreat = runWaiErrorsEither @ee . fmap Right

instance ErrorEffects '[] r where
  type Row '[] = '[]
  runWaiErrors = id
  runWaiErrorsEither = id

instance
  ( Member TinyLog (Append (Row ee) r),
    AsWai e,
    ErrorEffects ee r
  ) =>
  ErrorEffects (e ': ee) r
  where
  type Row (e ': ee) = (Error e ': Row ee)
  runWaiErrors = runWaiErrors @ee . runWaiError @e
  runWaiErrorsEither action = do
    runWaiErrorsEither @ee $ runWaiErrorEither @e action

runWaiError ::
  (AsWai e, Member TinyLog r) =>
  Sem (Error e ': r) Wai.Response ->
  Sem r Wai.Response
runWaiError =
  fmap (either (errorResponse defaultHeaders) id)
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
  forall routes.
  (HasServer (ToServantApi routes) '[], GenericServant routes AsServer, Server (ToServantApi routes) ~ ToServant routes AsServer) =>
  Middleware ->
  routes AsServer ->
  Env ->
  Int ->
  IO ()
serveServant middleware server env port =
  Warp.run port
    . Wai.catchErrorsWithRequestId getRequestId' (view applog env) []
    . middleware
    $ app
  where
    app :: Wai.Application
    app =
      genericServe server

    getRequestId' :: Wai.Request -> Maybe ByteString
    getRequestId' = lookup "Wire-Origin-Request-Id" . Wai.requestHeaders

serveServant1 ::
  forall (api :: Type).
  (HasServer api '[]) =>
  Middleware ->
  (RequestId -> Server api) ->
  Env ->
  Int ->
  IO ()
serveServant1 middleware mkServer env port =
  Warp.run port
    . Wai.catchErrors (view applog env) federationRequestIdHeaderName []
    . middleware
    $ app
  where
    app :: Wai.Application
    app req cont = do
      let rid = getRequestId federationRequestIdHeaderName req
      serve (Proxy @api) (mkServer rid) req cont

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
     Error ValidationError,
     Error RemoteError,
     Error Federator.Error.ServerError.ServerError,
     Error DiscoveryFailure,
     TinyLog,
     Embed IO,
     Embed (Codensity IO)
   ]

type AllEffects1 =
  '[ Metrics,
     Remote,
     DiscoverFederator,
     DNSLookup, -- needed by DiscoverFederator
     ServiceStreaming,
     Input RunSettings,
     Input Http2Manager, -- needed by Remote
     Input FedUp.FederationDomainConfigs, -- needed for the domain list and federation policy.
     Input Env, -- needed by Service
     Error ValidationError,
     Error RemoteError,
     Error Federator.Error.ServerError.ServerError,
     Error DiscoveryFailure,
     TinyLog,
     Embed (Codensity IO),
     Embed IO
   ]

-- | Run Sem action containing HTTP handlers. All errors have to been handled
-- already by this point.
runFederator :: Env -> RequestId -> Sem AllEffects Wai.Response -> Codensity IO Wai.Response
runFederator env rid =
  runM
    . runEmbedded @IO @(Codensity IO) liftIO
    . loggerToTinyLogReqId rid (view applog env)
    . runWaiErrors
      @'[ ValidationError,
          RemoteError,
          Federator.Error.ServerError.ServerError,
          DiscoveryFailure
        ]
    . runInputConst env
    . runInputSem (embed @IO (getFederationDomainConfigs env))
    . runInputSem (embed @IO (readIORef (view http2Manager env)))
    . runInputConst (view runSettings env)
    . interpretServiceHTTP
    . runDNSLookupWithResolver (view dnsResolver env)
    . runFederatorDiscovery
    . interpretRemote
    . interpretMetrics

toHandler :: IO (Either Wai.Error a) -> Handler a
toHandler action = do
  liftIO action >>= \case
    Left e -> throwError $ waiToServant e
    Right a -> pure a

waiToServant :: Wai.Error -> Servant.ServerError
waiToServant waierr =
  ServerError
    { errHTTPCode = HTTP.statusCode (Wai.code waierr),
      errReasonPhrase = LText.unpack (Wai.label waierr),
      errBody = encode waierr,
      errHeaders = []
    }

runFederatorIO :: Env -> RequestId -> Sem AllEffects1 a -> Handler a
runFederatorIO env rid =
  toHandler
    . runM
    . runEmbedded (lowerCodensity)
    . loggerToTinyLogReqId rid (view applog env)
    . runWaiErrorsEitherTheGreat
      @'[ ValidationError,
          RemoteError,
          Federator.Error.ServerError.ServerError,
          DiscoveryFailure
        ]
    . runInputConst env
    . runInputSem (embed @IO (getFederationDomainConfigs env))
    . runInputSem (embed @IO (readIORef (view http2Manager env)))
    . runInputConst (view runSettings env)
    . interpretServiceHTTP
    . runDNSLookupWithResolver (view dnsResolver env)
    . runFederatorDiscovery
    . interpretRemote
    . interpretMetrics

getFederationDomainConfigs :: Env -> IO FedUp.FederationDomainConfigs
getFederationDomainConfigs env = do
  let mgr = env ^. httpManager
      Endpoint h p = env ^. service $ Brig
      baseurl = BaseUrl Http (T.unpack h) (fromIntegral p) ""
      clientEnv = mkClientEnv mgr baseurl
  FedUp.getFederationDomainConfigs clientEnv >>= \case
    Right v -> pure v
    Left e -> error $ show e

streamingResponseToWai :: StreamingResponse -> Wai.Response
streamingResponseToWai resp =
  let headers = toList (responseHeaders resp)
      status = responseStatusCode resp
      streamingBody output flush =
        foreach
          (const (pure ()))
          (\chunk -> output (byteString chunk) *> flush)
          (responseBody resp)
   in Wai.responseStream status headers streamingBody
