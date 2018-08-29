{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.Run (initCassandra, mkLogger, runServer) where

import Bilge
import Cassandra as Cas
import Data.Int
import Data.List.NonEmpty as NE
import Data.Metrics (metrics)
import Data.String.Conversions
import Data.String (fromString)
import Lens.Micro
import Network.HTTP.Client (responseTimeoutMicro)
import Network.Wai (Application)
import Network.Wai.Utilities.Request (lookupRequestId)
import Spar.API
import Spar.API.Instances ()
import Spar.API.Swagger ()
import Spar.App
import Spar.Data.Instances ()
import Spar.Options
import Spar.Options as Options
import System.Logger (Logger)
import Util.Options (casEndpoint, casKeyspace)
import Util.Options (epHost, epPort)

import qualified Cassandra.Schema as Cas
import qualified Cassandra.Settings as Cas
import qualified Network.Connection as TLS
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.TLS as TLS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Utilities.Server as WU
import qualified SAML2.WebSSO as SAML
import qualified Spar.Options as Opts
import qualified System.Logger as Log


----------------------------------------------------------------------
-- cassandra

schemaVersion :: Int32
schemaVersion = 0

initCassandra :: Opts.Opts -> Logger -> IO ClientState
initCassandra opts lgr = do
    connectString <- maybe
               (Cas.initialContactsDNS (Opts.cassandra opts ^. casEndpoint . epHost))
               (Cas.initialContactsDisco "cassandra_spar")
               (cs <$> Opts.discoUrl opts)
    cas <- Cas.init (Log.clone (Just "cassandra.spar") lgr) $ Cas.defSettings
      & Cas.setContacts (NE.head connectString) (NE.tail connectString)
      & Cas.setPortNumber (fromIntegral $ Options.cassandra opts ^. casEndpoint . epPort)
      & Cas.setKeyspace (Keyspace $ Options.cassandra opts ^. casKeyspace)
      & Cas.setMaxConnections 4
      & Cas.setMaxStreams 128
      & Cas.setPoolStripes 4
      & Cas.setSendTimeout 3
      & Cas.setResponseTimeout 10
      & Cas.setProtocolVersion V3
    runClient cas $ Cas.versionCheck schemaVersion
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

runServer :: Opts -> IO ()
runServer sparCtxOpts = do
  sparCtxLogger <- mkLogger sparCtxOpts
  mx <- metrics
  sparCtxCas <- initCassandra sparCtxOpts sparCtxLogger
  let settings = Warp.defaultSettings
        & Warp.setHost (fromString $ sparCtxOpts ^. to saml . SAML.cfgSPHost)
        . Warp.setPort (sparCtxOpts ^. to saml . SAML.cfgSPPort)
  sparCtxHttpManager
    <- let tlsSettings = (TLS.mkManagerSettings (TLS.TLSSettings clientSettings) Nothing)
             { managerResponseTimeout     = responseTimeoutMicro (10 * 1000 * 1000)
             , managerConnCount           = 10   -- (this is the default)
             , managerIdleConnectionCount = 512  -- (this is the default)
             }
           clientSettings = TLS.defaultParamsClient mempty mempty
             -- TODO: consult 'initHttpManager' in brig on how to tweak these settings.
       in newManager tlsSettings
  let sparCtxHttpBrig = Bilge.host (sparCtxOpts ^. to brig . epHost . to cs)
                      . Bilge.port (sparCtxOpts ^. to brig . epPort)
                      $ Bilge.empty
  let wrappedApp
    -- . WU.measureRequests mx _
        -- TODO: we need the swagger sitemap from servant for this.  we also want this to be
        -- prometheus-compatible.  not sure about the order in which to do these.
        = WU.catchErrors sparCtxLogger mx
        . SAML.setHttpCachePolicy
        . lookupRequestIdMiddleware
        $ \sparCtxRequestId -> app Env {..}
  WU.runSettingsWithShutdown settings wrappedApp 5


lookupRequestIdMiddleware :: (RequestId -> Application) -> Application
lookupRequestIdMiddleware mkapp req cont = do
  let reqid = maybe mempty RequestId $ lookupRequestId req
  mkapp reqid req cont
