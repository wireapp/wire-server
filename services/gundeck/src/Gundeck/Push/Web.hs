-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Web push dispatch loop (RFC 8030 application server).
--
-- This is the runtime counterpart to 'Gundeck.Push.Native': where native push
-- delegates to AWS SNS, web push has gundeck itself act as the RFC 8030
-- application server. For each subscription it encrypts a notice payload
-- (RFC 8291 via 'Gundeck.Push.Web.Crypto.encryptPayload'), signs a VAPID JWT
-- (RFC 8292 via 'Gundeck.Push.Web.Crypto.signVapid'), and POSTs the encrypted
-- body to the browser push-service endpoint, using a hardened HTTP 'Manager'
-- with SSRF egress filtering ('Env._webPushManager').
--
-- Dispatch is structured to mirror 'Gundeck.Push.Native': @push@ fans out over
-- addresses under the push concurrency budget, and @push1@ handles a single
-- recipient with response classification and bounded retry.
module Gundeck.Push.Web
  ( push,

    -- * Pure helpers (exported for unit testing)
    ResponseOutcome (..),
    classifyResponse,
    urgencyFrom,
    ttlFrom,
    endpointOrigin,
  )
where

import Control.Lens (Lens', view, (^.))
import Control.Monad.Catch (Handler (..), handle, throwM)
import Control.Retry
  ( RetryPolicy,
    RetryStatus,
    capDelay,
    exponentialBackoff,
    limitRetries,
    recovering,
  )
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Id (UserId)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Gundeck.Env
import Gundeck.Monad
import Gundeck.Options
import Gundeck.Push.Native.Types (NativePush (npPriority))
import Gundeck.Push.Web.Crypto
  ( CryptoError,
    EncryptedBody (..),
    VapidHeaders (..),
    encryptPayload,
    signVapid,
  )
import Gundeck.Push.Web.Runner (runWebPush)
import Gundeck.Push.Web.Serialise (serialise)
import Imports
import Network.HTTP.Client
  ( HttpException (..),
    HttpExceptionContent (..),
    Request,
    RequestBody (..),
    httpLbs,
    method,
    parseRequest,
    requestBody,
    requestHeaders,
    responseStatus,
  )
import Network.HTTP.Types
  ( Status,
    hAuthorization,
    hContentEncoding,
    statusCode,
  )
import Prometheus qualified as Prom
import System.Logger.Class (field, msg, val, (~~))
import System.Logger.Class qualified as Log
import UnliftIO (handleAny, mapConcurrently, pooledMapConcurrentlyN_)
import Wire.API.Push.V2 (Priority (..))
import Wire.API.Push.V2.WebSubscription (EndpointUrl (..))
import Wire.WebPushStore (WebPushAddress (..))
import Wire.WebPushStore qualified as Store

--------------------------------------------------------------------------------
-- Public API

-- | Deliver a native push payload to all given web push subscriptions.
--
-- Mirrors 'Gundeck.Push.Native.push': empty address lists are a no-op, a
-- singleton skips the concurrency overhead, and a list fans out under the
-- configured per-push concurrency budget (reused from native push for v1;
-- see '_perNativePushConcurrency').
push :: NativePush -> [WebPushAddress] -> Gundeck ()
push _ [] = pure ()
push m [a] = push1 m a
push m addrs = do
  perPushConcurrency <- view (options . settings . perNativePushConcurrency)
  case perPushConcurrency of
    Nothing -> void $ mapConcurrently (push1 m) addrs
    Just chunkSize -> pooledMapConcurrentlyN_ chunkSize (push1 m) addrs

--------------------------------------------------------------------------------
-- Per-recipient dispatch

-- | Encrypt, sign, and POST a single notice, with bounded retry on transient
-- failures (RFC 8030 §5 response codes, transport errors). Mirrors
-- 'Gundeck.Push.Native.push1'.
--
-- Permanent failures (payload too large, recipient key invalid, non-retryable
-- HTTP status) are classified and counted without retrying. Transient failures
-- (429, 5xx, transport timeout) are retried with exponential backoff up to
-- 'webPushRetryPolicy' limits; retry exhaustion propagates to the catch-all
-- handler as an unexpected error.
push1 :: NativePush -> WebPushAddress -> Gundeck ()
push1 m a =
  handleAny onUnexpectedError
    . handle onPrepareError
    $ do
      -- All three are constructed together by 'createEnv' when the @webpush:@
      -- section is present. A 'Nothing' here indicates a startup wiring bug,
      -- not a runtime condition.
      mgr <- require webPushManager
      kp <- require vapid
      wp <- require (options . webpush)
      let uid = a.wpaUser
      -- 1. Serialise plaintext (RFC 8030 \/ 8291 §4 size guard).
      plaintext <- case serialise m uid of
        Left _ -> throwM WebPushPrepareTooLarge
        Right bs -> pure bs
      -- 2. Encrypt body (RFC 8291).
      EncryptedBody body <-
        liftIO (encryptPayload a.wpaKeys plaintext) >>= \case
          Left cryptoErr -> throwM (WebPushPrepareCryptoFailure cryptoErr)
          Right encryptedBody -> pure encryptedBody
      -- 3. Sign VAPID JWT (RFC 8292).
      vapidHeaders <-
        liftIO $
          signVapid
            kp
            (wp ^. vapidSubject)
            (endpointOrigin a.wpaEndpoint)
      -- 4. Build the HTTP request.
      let ttl = ttlFrom (wp ^. defaultTTL)
          urgency = urgencyFrom m.npPriority
      req <- liftIO (buildRequest a.wpaEndpoint body vapidHeaders ttl urgency)
      -- 5. Submit with bounded retry on transient failures.
      recovering webPushRetryPolicy webPushRetryHandlers $ \_ -> do
        resp <- liftIO (httpLbs req mgr)
        case classifyResponse (responseStatus resp) of
          ResponseSuccess -> onSuccess uid
          ResponseGone -> onGone a
          ResponseTooLarge -> onTooLarge uid
          ResponseRetryable -> liftIO (throwM WebPushRetrySignal)
          ResponseError {} -> onPermanentHttpError uid
  where
    onPrepareError :: WebPushPrepareError -> Gundeck ()
    onPrepareError = \case
      WebPushPrepareTooLarge ->
        onTooLarge a.wpaUser
      WebPushPrepareCryptoFailure cryptoErr -> do
        Prom.incCounter webPushErrorCounter
        Log.err $
          field "user" (toByteString a.wpaUser)
            ~~ field "error" (show cryptoErr)
            ~~ msg (val "Web push encryption failed")
      WebPushConfigMissing -> do
        Prom.incCounter webPushErrorCounter
        Log.err $
          msg
            ( val
                "Web push dispatch invoked with web push disabled \
                \(manager / VAPID keypair / options missing)"
            )

    onUnexpectedError :: SomeException -> Gundeck ()
    onUnexpectedError ex = do
      Prom.incCounter webPushErrorCounter
      Log.err $
        field "user" (toByteString a.wpaUser)
          ~~ field "error" (displayException ex)
          ~~ msg (val "Web push failed")

--------------------------------------------------------------------------------
-- Response handlers

onSuccess :: UserId -> Gundeck ()
onSuccess uid = do
  Prom.incCounter webPushSuccessCounter
  Log.debug $
    field "user" (toByteString uid)
      ~~ msg (val "Web push success")

-- | The push service returned 404 \/ 410: the subscription no longer exists.
-- Delete the row so future dispatches skip it. Mirrors
-- 'Gundeck.Push.Native.deleteTokens' for the native transport. Per the
onGone :: WebPushAddress -> Gundeck ()
onGone a = handleAny logDeleteFailure $ do
  Prom.incCounter webPushGoneCounter
  Log.info $
    field "user" (toByteString a.wpaUser)
      ~~ field "endpoint_origin" (endpointOrigin a.wpaEndpoint)
      ~~ field "cause" ("Gone" :: Text)
      ~~ msg (val "Web push endpoint gone, deleting subscription")
  pool <- view pgPool
  runWebPush pool (Store.deleteSubscription a.wpaUser a.wpaEndpoint) >>= \case
    Left storeErr ->
      Log.err $
        field "user" (toByteString a.wpaUser)
          ~~ field "error" (show storeErr)
          ~~ msg (val "Failed to delete gone web push subscription")
    Right () -> pure ()
  where
    logDeleteFailure ex =
      Log.err $
        field "user" (toByteString a.wpaUser)
          ~~ field "error" (displayException ex)
          ~~ msg (val "Unexpected failure deleting gone web push subscription")

onTooLarge :: UserId -> Gundeck ()
onTooLarge uid = do
  Prom.incCounter webPushTooLargeCounter
  Log.warn $
    field "user" (toByteString uid)
      ~~ msg (val "Web push payload too large")

onPermanentHttpError :: UserId -> Gundeck ()
onPermanentHttpError uid = do
  Prom.incCounter webPushErrorCounter
  Log.warn $
    field "user" (toByteString uid)
      ~~ msg (val "Web push failed with non-retryable status")

--------------------------------------------------------------------------------
-- Request construction

-- | Build the RFC 8030 POST request: aes128gcm body + RFC 8030\/8292 headers.
buildRequest ::
  EndpointUrl ->
  ByteString ->
  VapidHeaders ->
  -- | TTL (seconds). 0 = transient.
  Word32 ->
  -- | Urgency header value.
  Text ->
  IO Request
buildRequest endpoint body vapidHeaders ttl urgency = do
  -- 'EndpointUrl' is validated HTTPS by its smart constructor, so
  -- 'parseRequest' failing here is unexpected and propagates as a push
  -- failure (caught by the per-recipient catch-all).
  req0 <- parseRequest (Text.unpack (endpointUrlText endpoint))
  pure $
    req0
      { method = "POST",
        requestBody = RequestBodyBS body,
        requestHeaders =
          [ (hContentEncoding, "aes128gcm"),
            ("TTL", toByteString' ttl),
            ("Urgency", encodeUtf8 urgency),
            (hAuthorization, encodeUtf8 (vhAuthorization vapidHeaders)),
            ("Crypto-Key", encodeUtf8 (vhCryptoKey vapidHeaders))
          ]
      }

--------------------------------------------------------------------------------
-- Pure helpers (exported for testing)

-- | Outcome of classifying an RFC 8030 push-service response status. Drives
-- the dispatch decision in 'push1' (record \/ delete \/ retry \/ error).
data ResponseOutcome
  = -- | 201 Created or 202 Accepted. The push service accepted the message.
    ResponseSuccess
  | -- | 404 Not Found or 410 Gone. The subscription no longer exists; delete it.
    ResponseGone
  | -- | 413 Payload Too Large. The encrypted body exceeded the push service limit.
    ResponseTooLarge
  | -- | 429 Too Many Requests or any 5xx. Retry with backoff.
    ResponseRetryable
  | -- | Any other 4xx. A permanent client-side error; do not retry.
    ResponseError
  deriving stock (Eq, Show)

-- | Map a push-service HTTP status to a dispatch decision (RFC 8030 §5).
--
-- 201 and 202 indicate the message was accepted (RFC 8030 §5.1). 404 and 410
-- indicate the subscription has expired or been unsubscribed (RFC 8030 §5.1).
-- 413 indicates the payload is too large (RFC 8030 §5.1). 429 and 5xx are
-- transient (RFC 8030 §5.1) and should be retried with backoff. All other
-- 4xx status codes are permanent client errors.
classifyResponse :: Status -> ResponseOutcome
classifyResponse s =
  case statusCode s of
    c
      | c == 201 || c == 202 -> ResponseSuccess
      | c == 404 || c == 410 -> ResponseGone
      | c == 413 -> ResponseTooLarge
      | c == 429 -> ResponseRetryable
      | c >= 500 -> ResponseRetryable
      | otherwise -> ResponseError

-- | Map a Wire push 'Priority' to an RFC 8030 §5.3 @Urgency@ header value.
-- Wire has two levels; RFC 8030 defines four. The mapping is conservative:
-- @LowPriority@ maps to @"low"@ (not @"very-low"@) so the push service still
-- attempts timely delivery, and @HighPriority@ maps to @"high"@ so the user
-- agent wakes immediately.
urgencyFrom :: Priority -> Text
urgencyFrom LowPriority = "low"
urgencyFrom HighPriority = "high"

-- | Resolve the RFC 8030 §5.2 @TTL@ header value (seconds) from the
-- configured default. 'Nothing' means transient (TTL=0): the message is
-- delivered only if the user agent is immediately reachable, and discarded
-- otherwise. Wire's notification stream (Cassandra) is the durable store, so
-- a transient push is the correct default — the client fetches missed
-- notifications on reconnect.
ttlFrom :: Maybe Word32 -> Word32
ttlFrom = fromMaybe 0

-- | Extract the push-service endpoint origin (scheme + authority, i.e.
-- @https:\/\/host[:port]@) for use as the RFC 8292 @aud@ JWT claim.
-- 'EndpointUrl' is guaranteed HTTPS by its smart constructor, so the
-- @https:\/\/@ prefix is always present; the origin is everything up to the
-- first path separator.
endpointOrigin :: EndpointUrl -> Text
endpointOrigin (EndpointUrl raw) =
  case Text.stripPrefix "https://" raw of
    Nothing -> raw
    Just rest -> "https://" <> Text.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') rest

--------------------------------------------------------------------------------
-- Retry policy and handlers

-- | Bounded exponential backoff for transient web push failures: at most
-- 'webPushMaxRetries' attempts, starting at 50ms, capped at 5s. Mirrors the
-- shape of @x3@ used elsewhere in the codebase ('Wire.Rpc') but with shorter
-- delays appropriate for push service rate-limit recovery (RFC 8030 §5.1).
webPushRetryPolicy :: RetryPolicy
webPushRetryPolicy =
  capDelay 5000000 $
    limitRetries webPushMaxRetries <> exponentialBackoff 50000

-- | Maximum retry attempts for a transient web push failure (429 \/ 5xx \/
-- transport error). Three retries gives the push service four chances total,
-- which is generous for a best-effort notification signal.
webPushMaxRetries :: Int
webPushMaxRetries = 3

-- | Retry handlers for 'recovering'. Retries only on:
--
-- * 'WebPushRetrySignal' — our own signal for 429 \/ 5xx response statuses.
-- * Transient 'HttpException' content (timeout, connection failure).
--   Stricter than 'Bilge.Retry.canRetry': omits 'InternalException'
--   (wraps the SSRF egress filter rejection and must NOT be retried),
--   omits 'StatusCodeException' (we inspect status ourselves via
--   'httpNoBody'), and omits 'ProxyConnectException' (cannot arise with
--   'noProxy'); adds 'ConnectionTimeout'.
webPushRetryHandlers :: [RetryStatus -> Handler Gundeck Bool]
webPushRetryHandlers =
  [ const $ Handler $ \(_ :: WebPushRetrySignal) -> pure True,
    const $ Handler $ \(e :: HttpException) -> pure (retryableHttp e)
  ]

-- | Classify an 'HttpException' as retryable. Conservative: only clearly
-- transient transport failures warrant a retry.
retryableHttp :: HttpException -> Bool
retryableHttp (HttpExceptionRequest _ content) = case content of
  ResponseTimeout -> True
  ConnectionTimeout -> True
  ConnectionFailure {} -> True
  ConnectionClosed -> True
  _ -> False
retryableHttp _ = False

--------------------------------------------------------------------------------
-- Internal exceptions

-- | Signal thrown inside the 'recovering' action to trigger a retry on a
-- retryable response status (429 \/ 5xx). Not exported; only the retry
-- handler matches it.
data WebPushRetrySignal = WebPushRetrySignal
  deriving stock (Show)

instance Exception WebPushRetrySignal

-- | Permanent preparation failures, caught by 'onPrepareError' before the
-- retry loop is entered.
data WebPushPrepareError
  = WebPushPrepareTooLarge
  | WebPushPrepareCryptoFailure !CryptoError
  | WebPushConfigMissing
  deriving stock (Show)

instance Exception WebPushPrepareError

-- | Unwrap a 'Maybe' from the 'Env', throwing 'WebPushConfigMissing' if
-- absent. All three web-push env fields are constructed atomically by
-- 'createEnv', so a 'Nothing' is a wiring bug, not a runtime condition.
require :: Lens' Env (Maybe a) -> Gundeck a
require l = do
  mVal <- view l
  case mVal of
    Just v -> pure v
    Nothing -> throwM WebPushConfigMissing

--------------------------------------------------------------------------------
-- Prometheus counters

{-# NOINLINE webPushSuccessCounter #-}
webPushSuccessCounter :: Prom.Counter
webPushSuccessCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "web_push_success",
          Prom.metricHelp = "Number of times web pushes were successfully pushed"
        }

{-# NOINLINE webPushGoneCounter #-}
webPushGoneCounter :: Prom.Counter
webPushGoneCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "web_push_gone",
          Prom.metricHelp = "Number of times web pushes were rejected with 404/410 (subscription gone)"
        }

{-# NOINLINE webPushTooLargeCounter #-}
webPushTooLargeCounter :: Prom.Counter
webPushTooLargeCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "web_push_too_large",
          Prom.metricHelp =
            "Number of times web pushes were not pushed due to payload being too large"
        }

{-# NOINLINE webPushErrorCounter #-}
webPushErrorCounter :: Prom.Counter
webPushErrorCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "web_push_errors",
          Prom.metricHelp =
            "Number of times web pushes were not pushed due to an unexpected error"
        }
