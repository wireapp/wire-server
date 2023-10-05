{-# LANGUAGE RecordWildCards #-}

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

-- | The entry point for Spar.
--
-- (Well, as close to the entry point as we can get. The executable is produced by
-- @exec/Main.hs@, but it's just a wrapper over 'runServer'.)
module Spar.Run
  ( initCassandra,
    runServer,
    mkApp,
  )
where

import qualified Bilge
import Cassandra as Cas
import qualified Cassandra.Schema as Cas
import qualified Cassandra.Settings as Cas
import Control.Lens (to, (^.))
import Data.Id
import Data.List.NonEmpty as NE
import Data.Metrics.Servant (servantPrometheusMiddleware)
import Data.Proxy (Proxy (Proxy))
import qualified Data.UUID as UUID
import Data.UUID.V4 as UUID
import Imports
import Network.Wai (Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Utilities.Request (lookupRequestId)
import qualified Network.Wai.Utilities.Server as WU
import qualified OpenSSL.Session as OpenSSL
import qualified SAML2.WebSSO as SAML
import Spar.API (SparAPI, app)
import Spar.App
import qualified Spar.Data as Data
import Spar.Data.Instances ()
import Spar.Options
import Spar.Orphans ()
import System.Logger (Logger, msg, val, (.=), (~~))
import qualified System.Logger as Log
import qualified System.Logger.Extended as Log
import Util.Options (CassandraOpts, endpoint, filterNodesByDatacentre, host, keyspace, port, tlsCert, useTLS)
import Wire.API.Routes.Version.Wai
import Wire.Sem.Logger.TinyLog

----------------------------------------------------------------------
-- cassandra

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra opts lgr = do
  let cassOpts = cassandra opts
  mbSSLContext <- createSSLContext cassOpts
  connectString <-
    maybe
      (Cas.initialContactsPlain (cassOpts ^. endpoint . host))
      (Cas.initialContactsDisco "cassandra_spar" . cs)
      (discoUrl opts)
  let basicCASSettings =
        Cas.defSettings
          & Cas.setLogger (Cas.mkLogger (Log.clone (Just "cassandra.spar") lgr))
          & Cas.setContacts (NE.head connectString) (NE.tail connectString)
          & Cas.setPortNumber (fromIntegral $ cassOpts ^. endpoint . port)
          & Cas.setKeyspace (Keyspace $ cassOpts ^. keyspace)
          & Cas.setMaxConnections 4
          & Cas.setMaxStreams 128
          & Cas.setPoolStripes 4
          & Cas.setSendTimeout 3
          & Cas.setResponseTimeout 10
          & Cas.setProtocolVersion V4
          & Cas.setPolicy (Cas.dcFilterPolicyIfConfigured lgr (cassOpts ^. filterNodesByDatacentre))
      casSettings = maybe basicCASSettings (\sslCtx -> Cas.setSSLContext sslCtx basicCASSettings) mbSSLContext
  cas <- Cas.init casSettings
  runClient cas $ Cas.versionCheck Data.schemaVersion
  pure cas
  where
    createSSLContext :: CassandraOpts -> IO (Maybe OpenSSL.SSLContext)
    createSSLContext cassOpts
      | cassOpts ^. useTLS = do
          sslContext <- OpenSSL.context
          maybe (pure ()) (OpenSSL.contextSetCAFile sslContext) (cassOpts ^. tlsCert)
          OpenSSL.contextSetVerificationMode
            sslContext
            OpenSSL.VerifyPeer
              { vpFailIfNoPeerCert = True,
                vpClientOnce = True,
                vpCallback = Nothing
              }
          pure $ Just sslContext
      | otherwise = pure Nothing

----------------------------------------------------------------------
-- servant / wai / warp

-- | FUTUREWORK: figure out how to call 'Network.Wai.Utilities.Server.newSettings' here.  For once,
-- this would create the "Listening on..." log message there, but it may also have other benefits.
runServer :: Opts -> IO ()
runServer sparCtxOpts = do
  let settings = Warp.defaultSettings & Warp.setHost (fromString shost) . Warp.setPort sport
      shost :: String = sparCtxOpts ^. to saml . SAML.cfgSPHost
      sport :: Int = sparCtxOpts ^. to saml . SAML.cfgSPPort
  (wrappedApp, ctxOpts) <- mkApp sparCtxOpts
  let logger = sparCtxLogger ctxOpts
  Log.info logger . Log.msg $ "Listening on " <> shost <> ":" <> show sport
  WU.runSettingsWithShutdown settings wrappedApp Nothing

mkApp :: Opts -> IO (Application, Env)
mkApp sparCtxOpts = do
  let logLevel = samlToLevel $ saml sparCtxOpts ^. SAML.cfgLogLevel
  sparCtxLogger <- Log.mkLogger logLevel (logNetStrings sparCtxOpts) (logFormat sparCtxOpts)
  sparCtxCas <- initCassandra sparCtxOpts sparCtxLogger
  sparCtxHttpManager <- Bilge.newManager Bilge.defaultManagerSettings
  let sparCtxHttpBrig =
        Bilge.host (sparCtxOpts ^. to brig . host . to cs)
          . Bilge.port (sparCtxOpts ^. to brig . port)
          $ Bilge.empty
  let sparCtxHttpGalley =
        Bilge.host (sparCtxOpts ^. to galley . host . to cs)
          . Bilge.port (sparCtxOpts ^. to galley . port)
          $ Bilge.empty
  let wrappedApp =
        versionMiddleware (fold (disabledAPIVersions sparCtxOpts))
          . WU.heavyDebugLogging heavyLogOnly logLevel sparCtxLogger
          . servantPrometheusMiddleware (Proxy @SparAPI)
          . WU.catchErrors sparCtxLogger []
          -- Error 'Response's are usually not thrown as exceptions, but logged in
          -- 'renderSparErrorWithLogging' before the 'Application' can construct a 'Response'
          -- value, when there is still all the type information around.  'WU.catchErrors' is
          -- still here for errors outside the power of the 'Application', like network
          -- outages.
          . SAML.setHttpCachePolicy
          . lookupRequestIdMiddleware sparCtxLogger
          $ \sparCtxRequestId -> app Env {..}
      heavyLogOnly :: (Wai.Request, LByteString) -> Maybe (Wai.Request, LByteString)
      heavyLogOnly out@(req, _) =
        if Wai.requestMethod req == "POST" && Wai.pathInfo req == ["sso", "finalize-login"]
          then Just out
          else Nothing
  pure (wrappedApp, let sparCtxRequestId = Bilge.RequestId "N/A" in Env {..})

lookupRequestIdMiddleware :: Logger -> (RequestId -> Wai.Application) -> Wai.Application
lookupRequestIdMiddleware logger mkapp req cont = do
  case lookupRequestId req of
    Just rid -> do
      mkapp (RequestId rid) req cont
    Nothing -> do
      localRid <- RequestId . cs . UUID.toText <$> UUID.nextRandom
      Log.info logger $ "request-id" .= localRid ~~ "request" .= (show req) ~~ msg (val "generated a new request id for local request")
      mkapp localRid req cont
