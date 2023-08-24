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
import Control.Lens
import Data.Default (def)
import Data.List.NonEmpty as NE
import Data.Metrics.Servant (servantPrometheusMiddleware)
import Data.Proxy (Proxy (Proxy))
import Imports
import Network.Wai (Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Utilities.Request (lookupRequestId)
import qualified Network.Wai.Utilities.Server as WU
import qualified SAML2.WebSSO as SAML
import Spar.API (SparAPI, app)
import Spar.App
import qualified Spar.Data as Data
import Spar.Data.Instances ()
import Spar.Options
import Spar.Orphans ()
import System.Logger.Class (Logger)
import qualified System.Logger.Extended as Log
import Util.Options (endpoint, filterNodesByDatacentre, host, keyspace, port)
import Wire.API.Routes.Version.Wai
import Wire.Sem.Logger.TinyLog

----------------------------------------------------------------------
-- cassandra

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra opts lgr = do
  let cassOpts = cassandra opts
  connectString <-
    maybe
      (Cas.initialContactsPlain (cassOpts ^. endpoint . host))
      (Cas.initialContactsDisco "cassandra_spar" . cs)
      (discoUrl opts)
  cas <-
    Cas.init $
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
  runClient cas $ Cas.versionCheck Data.schemaVersion
  pure cas

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
          . lookupRequestIdMiddleware
          $ \sparCtxRequestId -> app Env {..}
      heavyLogOnly :: (Wai.Request, LByteString) -> Maybe (Wai.Request, LByteString)
      heavyLogOnly out@(req, _) =
        if Wai.requestMethod req == "POST" && Wai.pathInfo req == ["sso", "finalize-login"]
          then Just out
          else Nothing
  pure (wrappedApp, let sparCtxRequestId = Bilge.RequestId "N/A" in Env {..})

lookupRequestIdMiddleware :: (Bilge.RequestId -> Application) -> Application
lookupRequestIdMiddleware mkapp req cont = do
  let reqid = maybe def Bilge.RequestId $ lookupRequestId req
  mkapp reqid req cont
