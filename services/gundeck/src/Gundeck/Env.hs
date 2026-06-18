{-# LANGUAGE TemplateHaskell #-}

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

module Gundeck.Env where

import Bilge hiding (host, port)
import Cassandra (ClientState)
import Cassandra.Util (initCassandraForService)
import Control.AutoUpdate
import Control.Concurrent.Async (Async)
import Control.Exception (ErrorCall (..), throwIO)
import Control.Lens (makeLenses, (^.))
import Control.Retry (capDelay, exponentialBackoff)
import Crypto.ECC (Curve_P256R1)
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.ECDSA qualified as ECDSA
import Data.Bifunctor (first)
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Char8 qualified as BSChar8
import Data.Id
import Data.Misc (Milliseconds (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.X509.CertificateStore as CertStore
import Database.Redis qualified as Redis
import Gundeck.Aws qualified as Aws
import Gundeck.Options as Opt hiding (host, port)
import Gundeck.Options qualified as O
import Gundeck.Push.Web.Ssrf (isPrivateLiteralHost)
import Gundeck.Redis qualified as Redis
import Gundeck.Redis.HedisExtensions qualified as Redis
import Gundeck.ThreadBudget
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended (initPostgresPool)
import Imports
import Network.AMQP (Channel)
import Network.AMQP.Extended qualified as Q
import Network.HTTP.Client
  ( host,
    managerSetProxy,
    noProxy,
    responseTimeoutMicro,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.TLS as TLS
import Network.TLS.Extra qualified as TLS
import System.Logger qualified as Log
import System.Logger.Extended qualified as Logger
import URI.ByteString (schemeBSL, strictURIParserOptions, uriSchemeL)
import URI.ByteString qualified as URI
import Wire.PostgresMigrations qualified as PgMigrations

-- | The server's static VAPID P-256 keypair (RFC 8292), parsed once at startup
-- from 'WebPushOpts'. The public key is exposed to web clients via
-- @GET /push/web/vapid-public-key@ so they can pass it as
-- @applicationServerKey@ to @pushManager.subscribe()@. The private key signs
-- the per-request VAPID JWTs.
data VapidKeyPair = VapidKeyPair
  { _vkpPrivate :: !(ECDSA.PrivateKey Curve_P256R1),
    _vkpPublic :: !(ECDSA.PublicKey Curve_P256R1),
    -- | Uncompressed P-256 public point (@0x04 || X || Y@, 65 bytes),
    -- base64url-encoded without padding. This is the wire format expected by
    -- browsers as @applicationServerKey@.
    _vkpPublicB64 :: !Text
  }

makeLenses ''VapidKeyPair

data Env = Env
  { _reqId :: !RequestId,
    _options :: !Opts,
    _applog :: !Logger.Logger,
    _manager :: !Manager,
    _cstate :: !ClientState,
    _rstate :: !Redis.RobustConnection,
    _rstateAdditionalWrite :: !(Maybe Redis.RobustConnection),
    _awsEnv :: !Aws.Env,
    _time :: !(IO Milliseconds),
    _threadBudgetState :: !(Maybe ThreadBudgetState),
    -- | VAPID keypair used to authenticate web push requests (RFC 8292).
    -- 'Nothing' when web push is disabled (no @webpush:@ config section).
    _vapid :: !(Maybe VapidKeyPair),
    -- | Hasql connection pool backing the Postgres stores. Currently only web
    -- push subscription storage ('Wire.WebPushStore') uses it; native push
    -- remains on Cassandra. Acquired unconditionally at startup and migrated
    -- via 'Wire.PostgresMigrations.runAllMigrations' (runs every migration
    -- embedded in @wire-subsystems@, idempotent on a shared schema).
    _pgPool :: !Hasql.Pool,
    -- | Hardened HTTP 'Manager' used exclusively by web push dispatch
    -- ('Gundeck.Push.Web') to POST encrypted notifications to browser push
    -- services (RFC 8030). Hardening: proxy disabled via
    -- 'noProxy' and a 'managerModifyRequest' hook rejects literal private /
    -- loopback IP hosts ('isPrivateLiteralHost') as an SSRF defense on top of
    -- the registration-time @_endpointAllowlist@. 'Nothing' when web push is
    -- disabled (no @webpush:@ config section), in which case dispatch is never
    -- reached.
    _webPushManager :: !(Maybe Manager),
    _rabbitMqChannel :: MVar Channel
  }

makeLenses ''Env

createEnv :: Opts -> IO ([Async ()], Env)
createEnv o = do
  l <- Logger.mkLogger (o ^. logLevel) (o ^. logNetStrings) (o ^. logFormat)
  n <-
    newManager
      tlsManagerSettings
        { managerConnCount = o ^. settings . httpPoolSize,
          managerIdleConnectionCount = 3 * (o ^. settings . httpPoolSize),
          managerResponseTimeout = responseTimeoutMicro 5000000
        }

  redisUsername <- BSChar8.pack <$$> lookupEnv "REDIS_USERNAME"
  redisPassword <- BSChar8.pack <$$> lookupEnv "REDIS_PASSWORD"
  (rThread, r) <- createRedisPool l (o ^. redis) redisUsername redisPassword "main-redis"

  (rAdditionalThreads, rAdditional) <- case o ^. redisAdditionalWrite of
    Nothing -> pure ([], Nothing)
    Just additionalRedis -> do
      additionalRedisUsername <- BSChar8.pack <$$> lookupEnv "REDIS_ADDITIONAL_WRITE_USERNAME"
      addtionalRedisPassword <- BSChar8.pack <$$> lookupEnv "REDIS_ADDITIONAL_WRITE_PASSWORD"
      (rAddThread, rAdd) <- createRedisPool l additionalRedis additionalRedisUsername addtionalRedisPassword "additional-write-redis"
      pure ([rAddThread], Just rAdd)

  p <-
    initCassandraForService
      (o ^. cassandra)
      "gundeck"
      (o ^. discoUrl)
      Nothing
      l

  a <- Aws.mkEnv l o n
  io <-
    mkAutoUpdate
      defaultUpdateSettings
        { updateAction = Ms . round . (* 1000) <$> getPOSIXTime
        }
  mtbs <- mkThreadBudgetState `mapM` (o ^. settings . maxConcurrentNativePushes)

  -- Postgres pool for the web push subscription store (and any future
  -- Postgres-backed gundeck subsystem). Acquired unconditionally, mirroring
  -- galley/brig: even though only web push uses it today, a required pool keeps
  -- the startup contract uniform and lets us run
  -- 'Wire.PostgresMigrations.runAllMigrations' (idempotent — re-running
  -- already-applied migrations is a no-op, so a shared schema with galley/brig
  -- is safe).
  pool <- initPostgresPool (o ^. postgresqlPool) (o ^. postgresql) (o ^. postgresqlPassword)
  PgMigrations.runAllMigrations pool l

  -- VAPID keypair, only when the @webpush:@ config section is present. The env
  -- var @GUNDECK_WEBPUSH_VAPID_PRIVATE_KEY@ overrides the YAML value, mirroring
  -- the @REDIS_PASSWORD@ pattern: the private key is a long-lived secret and
  -- should be injected via the environment in production rather than committed
  -- to the config file. Missing/malformed key fails fast here.
  --
  -- Note: an empty-but-set env var (@"\"\""@) overrides the YAML value with the
  -- empty string, which then fails fast at decode — unlike @REDIS_PASSWORD@,
  -- where empty means "no auth". This is intentional: an empty P-256 key is
  -- never valid, so surfacing it as an error is safer than silently falling
  -- back to the YAML value.
  mVapid <- forM (o ^. webpush) $ \wp -> do
    envVapidKey <- lookupEnv "GUNDECK_WEBPUSH_VAPID_PRIVATE_KEY"
    let cfgVapidKey = wp ^. vapidPrivateKey
        vapidKey = maybe cfgVapidKey Text.pack envVapidKey
    case mkVapidKeyPair (wp ^. vapidSubject) vapidKey of
      Left err ->
        throwIO (ErrorCall ("gundeck.yaml/webpush: " <> err))
      Right kp -> pure kp

  -- Hardened HTTP manager for web push dispatch. Built only
  -- when the @webpush:@ section is present, so non-web-push deployments pay no
  -- extra connection pool. See '_webPushManager' for the hardening rationale.
  mWebPushManager <- forM (o ^. webpush) $ \_ -> mkWebPushManager o

  rabbitMqChannelMVar <- Q.mkRabbitMqChannelMVar l (Just "gundeck") (o ^. rabbitmq)
  pure $! (rThread : rAdditionalThreads,) $! Env (RequestId defRequestId) o l n p r rAdditional a io mtbs mVapid pool mWebPushManager rabbitMqChannelMVar

reqIdMsg :: RequestId -> Logger.Msg -> Logger.Msg
reqIdMsg = ("request" Logger..=) . unRequestId
{-# INLINE reqIdMsg #-}

createRedisPool :: Logger.Logger -> RedisEndpoint -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Async (), Redis.RobustConnection)
createRedisPool l ep username password identifier = do
  customCertStore <- case ep._tlsCa of
    Nothing -> pure Nothing
    Just caPath -> CertStore.readCertificateStore caPath
  let defClientParams = defaultParamsClient (Text.unpack ep._host) ""
      tlsParams =
        guard ep._enableTls
          $> defClientParams
            { clientHooks =
                if ep._insecureSkipVerifyTls
                  then defClientParams.clientHooks {onServerCertificate = \_ _ _ _ -> pure []}
                  else defClientParams.clientHooks,
              clientShared =
                case customCertStore of
                  Nothing -> defClientParams.clientShared
                  Just sharedCAStore -> defClientParams.clientShared {sharedCAStore},
              clientSupported =
                defClientParams.clientSupported
                  { supportedVersions = [TLS.TLS13, TLS.TLS12],
                    supportedCiphers = TLS.ciphersuite_strong
                  }
            }
  let redisConnInfo =
        Redis.defaultConnectInfo
          { Redis.connectHost = Text.unpack $ ep ^. O.host,
            Redis.connectPort = Redis.PortNumber (fromIntegral $ ep ^. O.port),
            Redis.connectUsername = username,
            Redis.connectAuth = password,
            Redis.connectTimeout = Just (secondsToNominalDiffTime 5),
            Redis.connectMaxConnections = 100,
            Redis.connectTLSParams = tlsParams
          }

  Log.info l $
    Log.msg (Log.val $ "starting connection to " <> identifier <> "...")
      . Log.field "connectionMode" (show $ ep ^. O.connectionMode)
      . Log.field "connInfo" (safeShowConnInfo redisConnInfo)
  let connectWithRetry = Redis.connectRobust l (capDelay 1000000 (exponentialBackoff 50000))
  r <- case ep ^. O.connectionMode of
    Master -> connectWithRetry $ Redis.checkedConnect redisConnInfo
    Cluster -> connectWithRetry $ Redis.checkedConnectCluster redisConnInfo
  Log.info l $ Log.msg (Log.val $ "Established connection to " <> identifier <> ".")
  pure r

safeShowConnInfo :: Redis.ConnectInfo -> String
safeShowConnInfo connInfo = show $ connInfo {Redis.connectAuth = "[REDACTED]" <$ Redis.connectAuth connInfo}

--------------------------------------------------------------------------------
-- Hardened HTTP manager for web push dispatch
--------------------------------------------------------------------------------

-- | Build the 'Manager' used to POST encrypted web push notifications to
-- browser push-service endpoints (RFC 8030). Hardening:
--
-- * @'managerSetProxy' 'noProxy'@ — never honour proxy environment variables
--   ('HTTP_PROXY' \/ 'http_proxy'). A misconfigured proxy in the deployment
--   environment could otherwise intercept the (already-encrypted) POSTs or,
--   worse, route them to an attacker-controlled upstream. Push services are
--   always directly reachable over the public internet.
--
-- * A 'managerModifyRequest' hook that rejects any host which is a literal
--   private \/ loopback \/ link-local IP (or the hostname @"localhost"@) via
--   'Gundeck.Push.Web.Ssrf.isPrivateLiteralHost'. This is the SSRF
--   belt-and-suspenders behind the registration-time @_endpointAllowlist@.
--
-- * The connection count and idle count mirror the regular '_manager', so the
--   two pools share the same sizing assumptions; web push traffic is low
--   volume compared to native push \/ SNS.
--
-- * The 5-second response timeout mirrors '_manager'; push services are
--   expected to acknowledge quickly (201 \/ 202 with an empty body).
mkWebPushManager :: Opts -> IO Manager
mkWebPushManager o =
  newManager
    (managerSetProxy noProxy tlsManagerSettings)
      { managerConnCount = o ^. settings . httpPoolSize,
        managerIdleConnectionCount = 3 * (o ^. settings . httpPoolSize),
        managerResponseTimeout = responseTimeoutMicro 5000000,
        managerModifyRequest = \req -> do
          let h = host req
          when (isPrivateLiteralHost h) $
            throwIO
              ( ErrorCall
                  ( "gundeck web push: refusing to POST to private/loopback host: "
                      <> show h
                  )
              )
          pure req
      }

--------------------------------------------------------------------------------
-- VAPID keypair parsing (pure, for unit testing)
--------------------------------------------------------------------------------

-- | Validate the VAPID subject and derive the public key from the private key.
-- Both inputs are taken from 'WebPushOpts'; errors are human-readable so they
-- surface cleanly as a startup crash from 'createEnv'.
mkVapidKeyPair ::
  -- | VAPID subject (RFC 8292 §3 @sub@ claim).
  Text ->
  -- | Private key, base64url-encoded raw 32-byte P-256 scalar.
  Text ->
  Either String VapidKeyPair
mkVapidKeyPair subject keyText = do
  validateVapidSubject subject
  parseVapidKeyPair keyText

-- | Decode a base64url raw 32-byte P-256 private scalar, validate it lies in
-- the range @[1, n-1]@ (where @n@ is the P-256 group order), and derive the
-- corresponding public point @Q = d*G@. Returns the keypair plus the
-- base64url-encoded uncompressed public point for the client-facing endpoint.
--
-- crypton's 'ECDSA.decodePrivate' only checks the byte length, so an explicit
-- 'ECDSA.scalarIsValid' check is required to reject the degenerate @d=0@
-- (which would yield the point at infinity) and scalars @>= n@.
parseVapidKeyPair :: Text -> Either String VapidKeyPair
parseVapidKeyPair keyText = do
  let keyBs = encodeUtf8 keyText
  raw <-
    first ("private key is not valid base64url: " <>) $
      B64U.decodeUnpadded keyBs
  case ECDSA.decodePrivate (Proxy @Curve_P256R1) raw of
    CryptoFailed err ->
      Left ("private key is not a valid P-256 scalar (expected 32 bytes): " <> show err)
    CryptoPassed priv
      | not (ECDSA.scalarIsValid (Proxy @Curve_P256R1) priv) ->
          Left "private key scalar is out of range (must be in [1, n-1]; got 0 or >= n)"
      | otherwise ->
          let pub = ECDSA.toPublic (Proxy @Curve_P256R1) priv
              pubBs :: ByteString
              pubBs = ECDSA.encodePublic (Proxy @Curve_P256R1) pub
              pubB64 = decodeUtf8 (B64U.encodeUnpadded pubBs)
           in Right (VapidKeyPair priv pub pubB64)

-- | Validate the VAPID @sub@ject is a @mailto:@ or @https:@ URL, per RFC 8292
-- §3. Any other scheme is rejected: the subject identifies the application
-- server operator to the push service, and the two allowed schemes are the
-- only interoperable ones.
--
-- Note: plain @http:@ is rejected even though RFC 8292 §3 permits it (and only
-- recommends @https:@). This is intentional: the VAPID JWT is a bearer token
-- identifying the server, and a cleartext transport would let an attacker
-- substitute their own @aud@ origin. Requiring @https:@ (or @mailto:@, which
-- carries no request) is a strict, safer stance.
validateVapidSubject :: Text -> Either String ()
validateVapidSubject subj =
  case URI.parseURI strictURIParserOptions (encodeUtf8 subj) of
    Left e -> Left ("subject is not a valid URI: " <> show e)
    Right uri ->
      let scheme = uri ^. uriSchemeL . schemeBSL
       in if scheme == "mailto" || scheme == "https"
            then Right ()
            else
              Left $
                "subject must use a mailto: or https: scheme, got: "
                  <> show scheme
