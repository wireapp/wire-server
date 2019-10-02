{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

-- | The entry point for Spar.
--
-- (Well, as close to the entry point as we can get. The executable is produced by
-- @exec/Main.hs@, but it's just a wrapper over 'runServer'.)
module Spar.Run
  ( initCassandra
  , runServer, mkApp
  ) where

import Imports
import Bilge
import Cassandra as Cas
import Control.Lens
import Data.Default (def)
import Data.List.NonEmpty as NE
import Data.Metrics.Servant (servantPrometheusMiddleware)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions
import Network.Wai (Application)
import Network.Wai.Utilities.Request (lookupRequestId)
import Spar.API (app, API)
import Spar.API.Swagger ()
import Spar.App
import Spar.Data.Instances ()
import Spar.Orphans ()
import Spar.Types as Types
import System.Logger.Class (Logger)
import Util.Options (casEndpoint, casKeyspace, epHost, epPort)

import qualified Cassandra.Schema as Cas
import qualified Cassandra.Settings as Cas
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Utilities.Server as WU
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified System.Logger.Extended as Log


----------------------------------------------------------------------
-- cassandra

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra opts lgr = do
    connectString <- maybe
               (Cas.initialContactsPlain (Types.cassandra opts ^. casEndpoint . epHost))
               (Cas.initialContactsDisco "cassandra_spar")
               (cs <$> Types.discoUrl opts)
    cas <- Cas.init $ Cas.defSettings
      & Cas.setLogger (Cas.mkLogger (Log.clone (Just "cassandra.spar") lgr))
      & Cas.setContacts (NE.head connectString) (NE.tail connectString)
      & Cas.setPortNumber (fromIntegral $ Types.cassandra opts ^. casEndpoint . epPort)
      & Cas.setKeyspace (Keyspace $ Types.cassandra opts ^. casKeyspace)
      & Cas.setMaxConnections 4
      & Cas.setMaxStreams 128
      & Cas.setPoolStripes 4
      & Cas.setSendTimeout 3
      & Cas.setResponseTimeout 10
      & Cas.setProtocolVersion V4
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
      sport :: Int    = sparCtxOpts ^. to saml . SAML.cfgSPPort
  (wrappedApp, ctxOpts) <- mkApp sparCtxOpts
  let logger = sparCtxLogger ctxOpts
  Log.info logger . Log.msg $ "Listening on " <> shost <> ":" <> show sport
  WU.runSettingsWithShutdown settings wrappedApp 5

mkApp :: Opts -> IO (Application, Env)
mkApp sparCtxOpts = do
  let logLevel = toLevel $ saml sparCtxOpts ^. SAML.cfgLogLevel
  sparCtxLogger <- Log.mkLogger logLevel (logNetStrings sparCtxOpts) (logFormat sparCtxOpts)
  sparCtxCas <- initCassandra sparCtxOpts sparCtxLogger
  sparCtxHttpManager <- newManager defaultManagerSettings
  let sparCtxHttpBrig =
          Bilge.host (sparCtxOpts ^. to brig . epHost . to cs)
        . Bilge.port (sparCtxOpts ^. to brig . epPort)
        $ Bilge.empty
  let sparCtxHttpGalley =
          Bilge.host (sparCtxOpts ^. to galley . epHost . to cs)
        . Bilge.port (sparCtxOpts ^. to galley . epPort)
        $ Bilge.empty
  let wrappedApp
        = WU.heavyDebugLogging heavyLogOnly logLevel sparCtxLogger
        . servantPrometheusMiddleware (Proxy @API)
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
  pure (wrappedApp, let sparCtxRequestId = RequestId "N/A" in Env {..})

lookupRequestIdMiddleware :: (RequestId -> Application) -> Application
lookupRequestIdMiddleware mkapp req cont = do
  let reqid = maybe def RequestId $ lookupRequestId req
  mkapp reqid req cont
