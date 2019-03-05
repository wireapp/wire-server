{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

-- | The entry point for Spar.
--
-- (Well, as close to the entry point as we can get. The executable is produced by
-- @exec/Main.hs@, but it's just a wrapper over 'runServer'.)
module Spar.Run
  ( initCassandra
  , mkLogger
  , runServer
  ) where

import Imports
import Bilge
import Cassandra as Cas
import Control.Lens
import Data.Default (def)
import Data.List.NonEmpty as NE
import Data.Metrics (metrics)
import Data.Metrics.Servant (routesToPaths)
import Data.String.Conversions
import Network.Wai (Application, Middleware)
import Network.Wai.Utilities.Request (lookupRequestId)
import Spar.API (app, API)
import Spar.API.Swagger ()
import Spar.App
import Spar.Data.Instances ()
import Spar.Orphans ()
import Spar.Types as Types
import System.Logger (Logger)
import Util.Options (casEndpoint, casKeyspace, epHost, epPort)

import qualified Cassandra.Schema as Cas
import qualified Cassandra.Settings as Cas
import qualified Data.Metrics.Types as Metrics
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Prometheus as Promth
import qualified Network.Wai.Utilities.Server as WU
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified System.Logger as Log


----------------------------------------------------------------------
-- cassandra

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra opts lgr = do
    connectString <- maybe
               (Cas.initialContactsPlain (Types.cassandra opts ^. casEndpoint . epHost))
               (Cas.initialContactsDisco "cassandra_spar")
               (cs <$> Types.discoUrl opts)
    cas <- Cas.init (Log.clone (Just "cassandra.spar") lgr) $ Cas.defSettings
      & Cas.setContacts (NE.head connectString) (NE.tail connectString)
      & Cas.setPortNumber (fromIntegral $ Types.cassandra opts ^. casEndpoint . epPort)
      & Cas.setKeyspace (Keyspace $ Types.cassandra opts ^. casKeyspace)
      & Cas.setMaxConnections 4
      & Cas.setMaxStreams 128
      & Cas.setPoolStripes 4
      & Cas.setSendTimeout 3
      & Cas.setResponseTimeout 10
      & Cas.setProtocolVersion V3
    runClient cas $ Cas.versionCheck Data.schemaVersion
    pure cas


----------------------------------------------------------------------
-- logger

mkLogger :: Opts -> IO Logger
mkLogger opts = Log.new $ Log.defSettings
  & Log.setLogLevel (toLevel $ saml opts ^. SAML.cfgLogLevel)
  & Log.setOutput Log.StdOut
  & Log.setFormat Nothing
  & Log.setNetStrings (logNetStrings opts)


----------------------------------------------------------------------
-- servant / wai / warp

-- | FUTUREWORK: figure out how to call 'Network.Wai.Utilities.Server.newSettings' here.  For once,
-- this would create the "Listening on..." log message there, but it may also have other benefits.
runServer :: Opts -> IO ()
runServer sparCtxOpts = do
  sparCtxLogger <- mkLogger sparCtxOpts
  mx <- metrics
  sparCtxCas <- initCassandra sparCtxOpts sparCtxLogger
  let settings = Warp.defaultSettings & Warp.setHost (fromString shost) . Warp.setPort sport
      shost :: String = sparCtxOpts ^. to saml . SAML.cfgSPHost
      sport :: Int    = sparCtxOpts ^. to saml . SAML.cfgSPPort
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
        = WU.catchErrors sparCtxLogger mx
        . promthRun
        . SAML.setHttpCachePolicy
        . lookupRequestIdMiddleware
        $ \sparCtxRequestId -> app Env {..}
  Log.info sparCtxLogger . Log.msg $ "Listening on " <> shost <> ":" <> show sport
  WU.runSettingsWithShutdown settings wrappedApp 5

lookupRequestIdMiddleware :: (RequestId -> Application) -> Application
lookupRequestIdMiddleware mkapp req cont = do
  let reqid = maybe def RequestId $ lookupRequestId req
  mkapp reqid req cont

promthRun :: Middleware
promthRun = Promth.prometheus conf . Promth.instrumentHandlerValue promthNormalize
  where
    conf = Promth.def
      { Promth.prometheusEndPoint = ["i", "metrics"]
      , Promth.prometheusInstrumentApp = False
      }

promthNormalize :: Wai.Request -> Text
promthNormalize req = pathInfo
  where
    mPathInfo  = Metrics.treeLookup (routesToPaths @API) $ cs <$> Wai.pathInfo req
    pathInfo   = cs $ fromMaybe "N/A" mPathInfo
