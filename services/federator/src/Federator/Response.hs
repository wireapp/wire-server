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
    serve,
    runWaiError,
    runWaiErrors,
    streamingResponseToWai,
  )
where

import Control.Lens
import Control.Monad.Codensity
import Data.ByteString.Builder
import Data.Kind
import Federator.Discovery
import Federator.Env
import Federator.Error
import Federator.Error.ServerError
import Federator.Options
import Federator.Remote
import Federator.Service
import Federator.Validation
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Utilities.Error as Wai
import qualified Network.Wai.Utilities.Server as Wai
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.TinyLog
import Servant.Client.Core
import Servant.Types.SourceT
import Wire.Network.DNS.Effect
import Wire.Sem.Logger.TinyLog

defaultHeaders :: [HTTP.Header]
defaultHeaders = [("Content-Type", "application/json")]

class ErrorEffects (ee :: [Type]) r where
  type Row ee :: EffectRow
  runWaiErrors ::
    Sem (Append (Row ee) r) Wai.Response ->
    Sem r Wai.Response

instance ErrorEffects '[] r where
  type Row '[] = '[]
  runWaiErrors = id

instance
  ( Member TinyLog (Append (Row ee) r),
    AsWai e,
    ErrorEffects ee r
  ) =>
  ErrorEffects (e ': ee) r
  where
  type Row (e ': ee) = (Error e ': Row ee)
  runWaiErrors = runWaiErrors @ee . runWaiError @e

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

serve ::
  (Wai.Request -> Sem AllEffects Wai.Response) ->
  Env ->
  Int ->
  IO ()
serve action env port =
  Warp.run port
    . Wai.catchErrors (view applog env) []
    $ app
  where
    app :: Wai.Application
    app req respond =
      runCodensity (runFederator env (action req)) respond

type AllEffects =
  '[ Remote,
     DiscoverFederator,
     DNSLookup, -- needed by DiscoverFederator
     ServiceStreaming,
     Input RunSettings,
     Input TLSSettings, -- needed by Remote
     Input Env, -- needed by Service
     Error ValidationError,
     Error RemoteError,
     Error ServerError,
     Error DiscoveryFailure,
     TinyLog,
     Embed IO,
     Embed (Codensity IO)
   ]

-- | Run Sem action containing HTTP handlers. All errors have to been handled
-- already by this point.
runFederator :: Env -> Sem AllEffects Wai.Response -> Codensity IO Wai.Response
runFederator env =
  runM
    . runEmbedded @IO @(Codensity IO) liftIO
    . loggerToTinyLogReqId (view requestId env) (view applog env)
    . runWaiErrors
      @'[ ValidationError,
          RemoteError,
          ServerError,
          DiscoveryFailure
        ]
    . runInputConst env
    . runInputSem (embed @IO (readIORef (view tls env)))
    . runInputConst (view runSettings env)
    . interpretServiceHTTP
    . runDNSLookupWithResolver (view dnsResolver env)
    . runFederatorDiscovery
    . interpretRemote

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
