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
import Cassandra.Util (initCassandraForService)
import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Control.Lens (to, (^.), (^?), _Just)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Coerce (coerce)
import Data.Credentials (Credentials (..))
import Data.Domain
import qualified Data.HashSet as HashSet
import Data.Id
import Data.LanguageCodes (ISO639_1 (EN))
import Data.Metrics.Servant (servantPrometheusMiddleware)
import Data.Proxy (Proxy (Proxy))
import Data.Qualified
import qualified Data.Set as Set
import Data.Text.Encoding
import qualified Database.Bloodhound as ES
import HTTP2.Client.Manager (Http2Manager, http2ManagerWithSSLCtx)
import qualified Hasql.Pool.Extended as Hasql
import Imports
import Network.HTTP.Client (Manager, ManagerSettings (..), newManager, responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL (opensslManagerSettings)
import Network.URI
import Network.Wai (Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gunzip as GZip
import Network.Wai.Utilities.Server
import qualified Network.Wai.Utilities.Server as WU
import qualified OpenSSL.Session as SSL
import qualified SAML2.WebSSO as SAML
import Spar.API (SparAPI, app)
import Spar.App
import qualified Spar.Data as Data
import Spar.Data.Instances ()
import Spar.Options as Opt
import Spar.Orphans ()
import System.Logger (Logger)
import qualified System.Logger as Log
import qualified System.Logger.Extended as Log
import qualified URI.ByteString as URI
import Util.Options
import qualified Web.Scim.Schema.Common as Scim
import Wire.API.Routes.Version (expandVersionExp)
import Wire.API.Routes.Version.Wai
import Wire.API.User (Language (Language), Locale (Locale))
import qualified Wire.AWSSubsystem.AWS as AWSI
import Wire.AuthenticationSubsystem.Config (AuthenticationSubsystemConfig (..), ZAuthEnv)
import qualified Wire.AuthenticationSubsystem.Config as AuthenticationSubsystem
import Wire.DeleteQueue.Types (InternalEventsOpts (..), QueueEnv (..), QueueOpts (..))
import Wire.FederationAPIAccess.Interpreter (FederationAPIAccessConfig (..))
import Wire.IndexedUserStore.ElasticSearch (ESConn (..), IndexedUserStoreConfig (..))
import Wire.RateLimit.Interpreter (newRateLimitEnv)
import Wire.ScimSubsystem.Interpreter
import qualified Wire.StompSubsystem.Stomp as Stomp
import Wire.UserSubsystem.UserSubsystemConfig (UserSubsystemConfig (..))

----------------------------------------------------------------------
-- cassandra

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra opts lgr =
  initCassandraForService
    (Opt.cassandra opts)
    "spar"
    (Opt.discoUrl opts)
    (Just Data.schemaVersion)
    lgr

initZAuth :: Opts -> IO ZAuthEnv
initZAuth o = do
  let zOpts = Opt.zauth o
      privateKeys = Opt.privateKeys zOpts
      publicKeys = Opt.publicKeys zOpts
  sk <- AuthenticationSubsystem.readKeys privateKeys
  pk <- AuthenticationSubsystem.readKeys publicKeys
  case (sk, pk) of
    (Nothing, _) -> error ("No private key in: " <> privateKeys)
    (_, Nothing) -> error ("No public key in: " <> publicKeys)
    (Just s, Just p) -> AuthenticationSubsystem.mkZAuthEnv s p (Opt.authSettings zOpts)

----------------------------------------------------------------------
-- internal events queue

initInternalEvents :: Logger -> Opts -> AWSI.Env -> IO QueueEnv
initInternalEvents lgr opts aws = case opts.internalEvents.internalEventsQueue of
  StompQueueOpts q -> do
    stomp :: Stomp.Env <- case (opts.stompOptions, opts.settings.stomp) of
      (Just s, Just c) -> Stomp.mkEnv lgr s <$> initCredentials c
      (Just _, Nothing) -> error "STOMP is configured but stomp credentials are not set"
      (Nothing, Just _) -> error "stomp credentials are present but STOMP is not configured"
      (Nothing, Nothing) -> error "stomp is selected for internal events, but not configured"
    pure (StompQueueEnv stomp q)
  SqsQueueOpts q -> do
    let throttleMillis = fromMaybe 500 opts.settings.sqsThrottleMillis
    SqsQueueEnv aws throttleMillis <$> AWSI.getQueueUrl (aws ^. AWSI.amazonkaEnv) q

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
  let logLevel = saml sparCtxOpts ^. SAML.cfgLogLevel
  sparCtxLogger <- Log.mkLogger logLevel (logNetStrings sparCtxOpts) (logFormat sparCtxOpts)
  sparCtxCas <- initCassandra sparCtxOpts sparCtxLogger
  sparCtxHttpManager <- Bilge.newManager Bilge.defaultManagerSettings
  sparCtxHttp2Manager <- initHttp2Manager
  let sparCtxHttpBrig =
        Bilge.host (sparCtxOpts ^. to brig . to host . to encodeUtf8)
          . Bilge.port (sparCtxOpts ^. to brig . to port)
          $ Bilge.empty
  let sparCtxHttpGalley =
        Bilge.host (sparCtxOpts ^. to galley . to host . to encodeUtf8)
          . Bilge.port (sparCtxOpts ^. to galley . to port)
          $ Bilge.empty
  let sparCtxHttpGalleyEndpoint = galley sparCtxOpts
  let disabledVersions = Set.fromList . mconcat $ Set.toList . expandVersionExp <$> Set.toList sparCtxOpts.disabledAPIVersions
  let sparCtxRequestId = RequestId defRequestId

  (sparCtxScimSubsystemConfig, sparCtxLocalUnit) <- do
    let bsUri :: URI.URI
        bsUri = sparCtxOpts.scimBaseUri

        crash :: String -> IO a
        crash msg = throwIO (ErrorCall $ "spar.yaml: scimBaseUri must be absolute URI containing server domain: " <> show (bsUri, msg))

    scimUri :: Scim.URI <- do
      maybe (crash "no parse") (pure . Scim.URI)
        . parseURI
        . UTF8.toString
        . URI.normalizeURIRef' URI.noNormalization
        $ bsUri

    localUnit :: Local () <- do
      bs <-
        maybe (crash "no host") (pure . URI.hostBS)
          . (^? URI.authorityL . _Just . URI.authorityHostL)
          $ bsUri
      either crash (pure . (`toLocalUnsafe` ())) (mkDomainFromBS bs)

    pure (ScimSubsystemConfig scimUri, localUnit)

  -- Initialize all the required subsystem configs
  let sparCtxHttpGundeckEndpoint = gundeck sparCtxOpts
  sparCtxZAuthEnv <- initZAuth sparCtxOpts
  let localUnit = toLocalUnsafe sparCtxOpts.settings.federationDomain ()
      sparCtxAuthenticationSubsystemConfig =
        AuthenticationSubsystemConfig
          { zauthEnv = sparCtxZAuthEnv,
            allowlistEmailDomains = sparCtxOpts.settings.allowlistEmailDomains,
            local = localUnit,
            userCookieRenewAge = sparCtxOpts.settings.userCookieRenewAge,
            userCookieLimit = sparCtxOpts.settings.userCookieLimit,
            userCookieThrottle = sparCtxOpts.settings.userCookieThrottle
          }
      sparCtxPasswordHashingOptions = sparCtxOpts.settings.passwordHashingOptions
  let sparCtxUserTemplates = undefined
  let sparCtxTeamTemplates = undefined
  let sparCtxTemplateBranding = undefined
  -- sparCtxUserTemplates <- loadUserTemplates (emailSMS sparCtxOpts).templateDir
  -- sparCtxTeamTemplates <- loadTeamTemplates (emailSMS sparCtxOpts).templateDir
  -- let sparCtxTemplateBranding = genTemplateBranding (emailSMS sparCtxOpts).templateBranding
  sparCtxRateLimit <- newRateLimitEnv sparCtxOpts.settings.passwordHashingRateLimit
  (esEnv, esIndexName) <- mkIndexEnv sparCtxOpts.elasticsearch
  let sparCtxFederationAPIAccessConfig =
        FederationAPIAccessConfig
          { ownDomain = sparCtxOpts.settings.federationDomain,
            federatorEndpoint = sparCtxOpts.federatorInternal,
            http2Manager = sparCtxHttp2Manager,
            requestId = sparCtxRequestId
          }
      mainESEnv = esEnv
      sparCtxIndexedUserStoreConfig =
        IndexedUserStoreConfig
          { conn =
              ESConn
                { env = mainESEnv,
                  indexName = esIndexName
                },
            additionalConn = Nothing
          }
      blockedDomains =
        sparCtxOpts.settings.customerExtensions
          ^? _Just
            . to (coerce @_ @(HashSet Domain) . Opt.domainsBlockedForRegistration)
          & fromMaybe HashSet.empty
      sparCtxUserSubsystemConfig =
        UserSubsystemConfig
          { emailVisibilityConfig = sparCtxOpts.settings.emailVisibility,
            defaultLocale = fromMaybe (Locale (Language EN) Nothing) sparCtxOpts.settings.defaultUserLocale,
            searchSameTeamOnly = fromMaybe False sparCtxOpts.settings.searchSameTeamOnly,
            maxTeamSize = sparCtxOpts.settings.maxTeamSize,
            activationCodeTimeout = sparCtxOpts.settings.activationTimeout,
            blockedDomains = blockedDomains
          }
  sparCtxHasqlPool <- Hasql.initPostgresPool (postgresql sparCtxOpts) (postgresqlPassword sparCtxOpts)
  let sparCtxSmtp = Nothing -- Spar doesn't send emails directly
  sparCtxAws <- AWSI.mkEnv sparCtxLogger sparCtxOpts.aws Nothing sparCtxHttpManager
  sparCtxInternalEvents <- initInternalEvents sparCtxLogger sparCtxOpts sparCtxAws

  let ctx0 = Env {..}
  let heavyLogOnly :: (Wai.Request, LByteString) -> Maybe (Wai.Request, LByteString)
      heavyLogOnly out@(req, _) =
        if Wai.requestMethod req == "POST" && Wai.pathInfo req == ["sso", "finalize-login"]
          then Just out
          else Nothing
  let middleware =
        versionMiddleware (foldMap expandVersionExp (disabledAPIVersions sparCtxOpts))
          . requestIdMiddleware (ctx0.sparCtxLogger) defaultRequestIdHeaderName
          . WU.heavyDebugLogging heavyLogOnly logLevel sparCtxLogger defaultRequestIdHeaderName
          . servantPrometheusMiddleware (Proxy @SparAPI)
          . GZip.gunzip
          . WU.catchErrors sparCtxLogger defaultRequestIdHeaderName
          -- Error 'Response's are usually not thrown as exceptions, but logged in
          -- 'renderSparErrorWithLogging' before the 'Application' can construct a 'Response'
          -- value, when there is still all the type information around.  'WU.catchErrors' is
          -- still here for errors outside the power of the 'Application', like network
          -- outages.
          . SAML.setHttpCachePolicy
  pure (middleware $ app ctx0, ctx0)

mkIndexEnv :: Opt.ElasticSearchOpts -> IO (ES.BHEnv, ES.IndexName)
mkIndexEnv esOpts = do
  mEsCreds :: Maybe Credentials <- for esOpts.credentials initCredentials

  let mkBhEnv skipVerifyTls mCustomCa mCreds url = do
        mgr <- initHttpManagerWithTLSConfig skipVerifyTls mCustomCa
        let bhe = ES.mkBHEnv url mgr
        pure $ maybe bhe (\creds -> bhe {ES.bhRequestHook = ES.basicAuthHook (ES.EsUsername creds.username) (ES.EsPassword creds.password)}) mCreds
  bhEnv <- mkBhEnv esOpts.insecureSkipVerifyTls esOpts.caCert mEsCreds esOpts.url
  pure (bhEnv, esOpts.index)

initHttpManagerWithTLSConfig :: Bool -> Maybe FilePath -> IO Manager
initHttpManagerWithTLSConfig skipTlsVerify mCustomCa = do
  -- See Note [SSL context]
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  if skipTlsVerify
    then SSL.contextSetVerificationMode ctx SSL.VerifyNone
    else
      SSL.contextSetVerificationMode ctx $
        SSL.VerifyPeer True True Nothing
  case mCustomCa of
    Nothing -> SSL.contextSetDefaultVerifyPaths ctx
    Just customCa -> do
      filePath <- canonicalizePath customCa
      SSL.contextSetCAFile ctx filePath
  -- Unfortunately, there are quite some AWS services we talk to
  -- (e.g. SES, Dynamo) that still only support TLSv1.
  -- Ideally: SSL.contextAddOption ctx SSL_OP_NO_TLSv1
  newManager
    (opensslManagerSettings (pure ctx))
      { managerConnCount = 1024,
        managerIdleConnectionCount = 4096,
        managerResponseTimeout = responseTimeoutMicro 10000000
      }

initHttp2Manager :: IO Http2Manager
initHttp2Manager = do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
  SSL.contextAddOption ctx SSL.SSL_OP_NO_TLSv1
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextSetDefaultVerifyPaths ctx
  http2ManagerWithSSLCtx ctx
