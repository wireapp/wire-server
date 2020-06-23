{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Network.Wai.Utilities.Server
  ( -- * Server Setup
    Server (..),
    defaultServer,
    newSettings,
    runSettingsWithShutdown,
    compile,
    route,

    -- * Middlewares
    catchErrors,
    OnErrorMetrics,
    heavyDebugLogging,
    rethrow5xx,
    lazyResponseBody,

    -- * Utilities
    onError,
    logError,
    logIO,
    runHandlers,
    restrict,
    flushRequestBody,
  )
where

import Control.Concurrent.Async
import Control.Exception (throw, throwIO)
import Control.Monad.Catch hiding (onError, onException)
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Metrics.GC (spawnGCMetricsCollector)
import Data.Metrics.Middleware
import Data.Streaming.Zlib (ZlibException (..))
import Data.String.Conversions (cs)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy.Encoding as LT
import Imports
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal (TimeoutThread)
import qualified Network.Wai.Internal as WaiInt
import Network.Wai.Predicate hiding (Error, err, status)
import qualified Network.Wai.Predicate as P
import Network.Wai.Predicate.Request (HasRequest)
import Network.Wai.Routing.Route (App, Continue, Routes, Tree)
import qualified Network.Wai.Routing.Route as Route
import qualified Network.Wai.Utilities.Error as Error
import qualified Network.Wai.Utilities.Error as Wai
import Network.Wai.Utilities.Request (lookupRequestId)
import Network.Wai.Utilities.Response
import qualified Prometheus as Prm
import qualified System.Logger as Log
import System.Logger.Class hiding (Error, Settings, format)
import System.Posix.Signals (installHandler, sigINT, sigTERM)
import qualified System.Posix.Signals as Sig

--------------------------------------------------------------------------------
-- Server Setup

data Server = Server
  { serverHost :: String,
    serverPort :: Word16,
    serverLogger :: Logger,
    serverMetrics :: Metrics,
    serverTimeout :: Maybe Int
  }

defaultServer :: String -> Word16 -> Logger -> Metrics -> Server
defaultServer h p l m = Server h p l m Nothing

newSettings :: MonadIO m => Server -> m Settings
newSettings (Server h p l m t) = do
  -- (Atomically) initialise the standard metrics, to avoid races.
  void $ gaugeGet (path "net.connections") m
  void $ counterGet (path "net.errors") m
  return $
    setHost (fromString h)
      . setPort (fromIntegral p)
      . setBeforeMainLoop logStart
      . setOnOpen (const $ connStart >> return True)
      . setOnClose (const connEnd)
      . setTimeout (fromMaybe 300 t)
      $ defaultSettings
  where
    connStart = gaugeIncr (path "net.connections") m
    connEnd = gaugeDecr (path "net.connections") m
    logStart =
      Log.info l . msg $
        val "Listening on " +++ h +++ ':' +++ p

-- Run a WAI 'Application', initiating Warp's graceful shutdown
-- on receiving either the INT or TERM signals. After closing
-- the listen socket, Warp will be allowed to drain existing
-- connections up to the given number of seconds.
runSettingsWithShutdown :: Settings -> Application -> Word16 -> IO ()
runSettingsWithShutdown s app secs = do
  initialization
  latch <- newEmptyMVar
  let s' = setInstallShutdownHandler (catchSignals latch) s
  srv <- async $ runSettings s' app `finally` void (tryPutMVar latch ())
  takeMVar latch
  await srv secs
  where
    initialization :: IO ()
    initialization = do
      spawnGCMetricsCollector
    catchSignals latch closeSocket = do
      let shutdown = closeSocket >> putMVar latch ()
      void $ installHandler sigINT (Sig.CatchOnce shutdown) Nothing
      void $ installHandler sigTERM (Sig.CatchOnce shutdown) Nothing
    await srv t = do
      status <- poll srv
      case status of
        Nothing | t > 0 -> threadDelay 1000000 >> await srv (t - 1)
        Just (Left ex) -> throwIO ex
        _ -> cancel srv

compile :: Monad m => Routes a m b -> Tree (App m)
compile routes = Route.prepare (Route.renderer predicateError >> routes)
  where
    predicateError e = return (encode $ Wai.Error (P.status e) "client-error" (format e), [jsonContent])
    -- [label] 'source' reason: message
    format e =
      let l = labelStr $ labels e
          s = sourceStr <$> source e
          r = reasonStr <$> reason e
          t = message e
       in case catMaybes [l, s, r] of
            [] -> maybe "N/A" (LT.decodeUtf8With lenientDecode . LBS.fromStrict) t
            bs -> LT.decodeUtf8With lenientDecode . toLazyByteString $ mconcat bs <> messageStr t
    labelStr [] = Nothing
    labelStr ls =
      Just $
        char7 '['
          <> byteString (C.intercalate "," ls)
          <> char7 ']'
          <> char7 ' '
    sourceStr s = char7 '\'' <> byteString s <> char7 '\'' <> char7 ' '
    reasonStr NotAvailable = "required"
    reasonStr TypeError = "invalid"
    messageStr (Just t) = char7 ':' <> char7 ' ' <> byteString t
    messageStr Nothing = mempty

route :: (MonadCatch m, MonadIO m) => Tree (App m) -> Request -> Continue IO -> m ResponseReceived
route rt rq k = Route.routeWith (Route.Config $ errorRs' noEndpoint) rt rq (liftIO . k)
  where
    noEndpoint = Wai.Error status404 "no-endpoint" "The requested endpoint does not exist"
{-# INLINEABLE route #-}

--------------------------------------------------------------------------------
-- Middlewares

-- | Create a middleware that catches exceptions and turns
-- them into appropriate 'Error' responses, thereby logging
-- as well as counting server errors (i.e. exceptions that
-- yield 5xx responses).
--
-- This does not log any 'Response' values with error status.
-- See 'catchErrors'.
catchErrors :: Logger -> OnErrorMetrics -> Middleware
catchErrors l m app req k =
  rethrow5xx l app req k `catch` errorResponse
  where
    errorResponse :: SomeException -> IO ResponseReceived
    errorResponse ex = do
      er <- runHandlers ex errorHandlers
      when (statusCode (Error.code er) >= 500) $
        logIO l Log.Error (Just req) (show ex)
      onError l m req k er
{-# INLINEABLE catchErrors #-}

-- | Standard handlers for turning exceptions into appropriate
-- 'Error' responses.
errorHandlers :: Applicative m => [Handler m Wai.Error]
errorHandlers =
  [ Handler $ \(x :: Wai.Error) -> pure x,
    Handler $ \(_ :: InvalidRequest) -> pure $ Wai.Error status400 "client-error" "Invalid Request",
    Handler $ \(_ :: TimeoutThread) -> pure $ Wai.Error status408 "client-error" "Request Timeout",
    Handler $ \case
      ZlibException (-3) -> pure $ Wai.Error status400 "client-error" "Invalid request body compression"
      ZlibException _ -> pure $ Wai.Error status500 "server-error" "Server Error",
    Handler $ \(_ :: SomeException) -> pure $ Wai.Error status500 "server-error" "Server Error"
  ]
{-# INLINE errorHandlers #-}

-- | If the log level is less sensitive than 'Debug' just call the underlying app unchanged.
-- Otherwise, pull a copy of the request body before running it, and if response status is @>=
-- 400@, log the entire request, including the body.
--
-- The request sanitizer is called on the 'Request' and its body before it is being logged,
-- giving you a chance to erase any confidential information.
--
-- WARNINGS:
--
--  * This may log confidential information if contained in the request.  Use the sanitizer to
--    avoid that.
--  * This does not catch any exceptions in the underlying app, so consider calling
--    'catchErrors' before this.
--  * Be careful with trying this in production: this puts a performance penalty on every
--    request (unless level is less sensitive than 'Debug').
heavyDebugLogging ::
  ((Request, LByteString) -> Maybe (Request, LByteString)) ->
  Level ->
  Logger ->
  Middleware
heavyDebugLogging sanitizeReq lvl lgr app = \req cont -> do
  (bdy, req') <-
    if lvl `elem` [Trace, Debug]
      then cloneBody req
      else pure ("body omitted because log level was less sensitive than Debug", req)
  app req' $ \resp -> do
    forM_ (sanitizeReq (req', bdy)) $ \(req'', bdy') ->
      when (statusCode (responseStatus resp) >= 400) $ logMostlyEverything req'' bdy' resp
    cont resp
  where
    cloneBody :: Request -> IO (LByteString, Request)
    cloneBody req = do
      bdy <- lazyRequestBody req
      requestBody' <- emitLByteString bdy
      pure (bdy, req {requestBody = requestBody'})
    logMostlyEverything :: Request -> LByteString -> Response -> IO ()
    logMostlyEverything req bdy resp = Log.debug lgr logMsg
      where
        logMsg =
          field "request" (fromMaybe "N/A" $ lookupRequestId req)
            . field "request_details" (show req)
            . field "request_body" bdy
            . field "response_status" (show $ responseStatus resp)
            . field "response_headers" (show $ responseHeaders resp)
            . msg (val "full request details")

-- | Compute a stream from a lazy bytestring suitable for putting into the 'Response'.  This
-- can be used if we want to take a look at the body in a 'Middleware' *after* the request has
-- been processed and the stream flushed.
--
-- This implementation returns the entire body in the first stream chunk.  An alternative,
-- possibly faster implementation would be this:
--
-- >>> emitLByteString lbs = do
-- >>>     chunks <- TVar.newTVarIO (LBS.toChunks lbs)
-- >>>     pure $ do
-- >>>         nextChunk <- atomically $ do
-- >>>             xs <- TVar.readTVar chunks
-- >>>             case xs of
-- >>>                 [] -> pure Nothing
-- >>>                 (x:xs') -> TVar.writeTVar chunks xs' >> pure (Just x)
-- >>>         pure $ fromMaybe "" nextChunk
emitLByteString :: LByteString -> IO (IO ByteString)
emitLByteString lbs = do
  tvar <- newTVarIO (cs lbs)
  -- Emit the bytestring on the first read, then always return "" on subsequent reads
  return . atomically $ swapTVar tvar mempty

-- | Run the 'Application'; check the response status; if >=500, throw a 'Wai.Error' with
-- label @"server-error"@ and the body as the error message.
rethrow5xx :: Logger -> Middleware
rethrow5xx logger app req k = app req k'
  where
    k' resp@(WaiInt.ResponseRaw {}) = do
      -- See Note [Raw Response]
      let logMsg =
            field "canoncalpath" (show $ pathInfo req)
              . field "rawpath" (rawPathInfo req)
              . field "request" (fromMaybe "N/A" $ lookupRequestId req)
              . msg (val "ResponseRaw - cannot collect metrics or log info on errors")
      Log.log logger Log.Debug logMsg
      k resp
    k' resp = do
      let st = responseStatus resp
      if statusCode st < 500
        then k resp
        else do
          rsbody :: LText <- liftIO $ cs <$> lazyResponseBody resp
          throwM $ Wai.Error st "server-error" rsbody

-- | This flushes the response!  If you want to keep using the response, you need to construct
-- a new one with a fresh body stream.
lazyResponseBody :: Response -> IO LByteString
lazyResponseBody rs = case responseToStream rs of
  (_, _, cont :: (StreamingBody -> IO ()) -> IO ()) -> do
    tvar <- atomically $ newTVar mempty
    let pushstream builder = atomically $ modifyTVar tvar (<> builder)
    cont $ \streamingBody ->
      streamingBody pushstream (pure ())
    atomically $ toLazyByteString <$> readTVar tvar

--------------------------------------------------------------------------------
-- Utilities

-- | 'onError' and 'catchErrors' support both the metrics-core ('Right') and the prometheus
-- package introduced for spar ('Left').
type OnErrorMetrics = [Either Prm.Counter Metrics]

-- | Send an 'Error' response.
onError ::
  MonadIO m =>
  Logger ->
  OnErrorMetrics ->
  Request ->
  Continue IO ->
  Wai.Error ->
  m ResponseReceived
onError g m r k e = liftIO $ do
  logError g (Just r) e
  when (statusCode (Error.code e) >= 500) $
    either Prm.incCounter (counterIncr (path "net.errors")) `mapM_` m
  flushRequestBody r
  k (errorRs' e)

-- | Log an 'Error' response for debugging purposes.
--
-- It would be nice to have access to the request body here, but that's already streamed away
-- by the handler in all likelyhood.  See 'heavyDebugLogging'.
logError :: (MonadIO m, HasRequest r) => Logger -> Maybe r -> Wai.Error -> m ()
logError g mr (Wai.Error c l m) = liftIO $ Log.debug g logMsg
  where
    logMsg =
      field "code" (statusCode c)
        . field "label" l
        . field "request" (fromMaybe "N/A" (lookupRequestId =<< mr))
        . msg (val "\"" +++ m +++ val "\"")

logIO :: (ToBytes msg, HasRequest r) => Logger -> Level -> Maybe r -> msg -> IO ()
logIO lg lv r a =
  let reqId = field "request" . fromMaybe "N/A" . lookupRequestId <$> r
      mesg = fromMaybe id reqId . msg a
   in Log.log lg lv mesg

runHandlers :: SomeException -> [Handler m a] -> m a
runHandlers e [] = throw e
runHandlers e (Handler h : hs) = maybe (runHandlers e hs) h (fromException e)

restrict :: Int -> Int -> Predicate r P.Error Int -> Predicate r P.Error Int
restrict l u = fmap $ \x ->
  x >>= \v ->
    if v >= l && v <= u
      then x
      else Fail (setMessage (emsg v) . setReason TypeError $ e400)
  where
    emsg v =
      LBS.toStrict . toLazyByteString $
        byteString "outside range ["
          <> intDec l
          <> byteString ", "
          <> intDec u
          <> byteString "]: "
          <> intDec v

flushRequestBody :: Request -> IO ()
flushRequestBody req = do
  bs <- getRequestBodyChunk req
  unless (BS.null bs) $
    flushRequestBody req
