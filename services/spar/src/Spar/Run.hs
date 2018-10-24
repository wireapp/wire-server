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

module Spar.Run
  ( initCassandra
  , mkLogger
  , runServer
  ) where

import Bilge
import Cassandra as Cas
import Control.Lens
import Data.List.NonEmpty as NE
import Data.Metrics (metrics)
import Data.String.Conversions
import Data.String (fromString)
import Network.HTTP.Types.Status (status413)
import Network.Wai (Application, Middleware, pathInfo, requestBodyLength, RequestBodyLength(..), responseLBS)
import Network.Wai.Utilities.Request (lookupRequestId)
import Spar.API (app)
import Spar.API.Instances ()
import Spar.API.Swagger ()
import Spar.App
import Spar.Data.Instances ()
import Spar.Types as Types
import System.Logger (Logger)
import Util.Options (casEndpoint, casKeyspace, epHost, epPort)

import qualified Cassandra.Schema as Cas
import qualified Cassandra.Settings as Cas
import qualified Data.List as List
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

runServer :: Opts -> IO ()
runServer sparCtxOpts = do
  sparCtxLogger <- mkLogger sparCtxOpts
  mx <- metrics
  sparCtxCas <- initCassandra sparCtxOpts sparCtxLogger
  let settings = Warp.defaultSettings
        & Warp.setHost (fromString $ sparCtxOpts ^. to saml . SAML.cfgSPHost)
        . Warp.setPort (sparCtxOpts ^. to saml . SAML.cfgSPPort)
  sparCtxHttpManager <- newManager defaultManagerSettings
  let sparCtxHttpBrig = Bilge.host (sparCtxOpts ^. to brig . epHost . to cs)
                      . Bilge.port (sparCtxOpts ^. to brig . epPort)
                      $ Bilge.empty
  let wrappedApp
        = responseSizeLimitMiddleware authnRespSizeLimit
        . WU.catchErrors sparCtxLogger mx
        . Promth.prometheus Promth.def { Promth.prometheusEndPoint = ["i", "monitoring"] }
        . SAML.setHttpCachePolicy
        . lookupRequestIdMiddleware
        $ \sparCtxRequestId -> app Env {..}
  WU.runSettingsWithShutdown settings wrappedApp 5

lookupRequestIdMiddleware :: (RequestId -> Application) -> Application
lookupRequestIdMiddleware mkapp req cont = do
  let reqid = maybe mempty RequestId $ lookupRequestId req
  mkapp reqid req cont

-- | `/sso/finalize-login` is computationally expensive, e.g. if bodies are crafted to impose
-- maximum cost on signature verification.
--
-- FUTUREWORK: move this to saml2-web-sso?
responseSizeLimitMiddleware :: Int -> Middleware
responseSizeLimitMiddleware limit applic req cont = do
  if ["sso", "finalize-login"] `List.isPrefixOf` pathInfo req
    then case requestBodyLength req of
           KnownLength len
             -> if fromIntegral len < limit
                then applic req cont
                else cont $ responseLBS status413 mempty "body too large"
           ChunkedBody
             -> cont $ responseLBS status413 mempty "chunked body not supported"
    else applic req cont

-- | AuthnResponse documents in all vendor tests were always around 10k or smaller.  multiply this
-- by 8 to be safe.
--
-- TODO: move this to "Options".
authnRespSizeLimit :: Int
authnRespSizeLimit = 80 * 1000
