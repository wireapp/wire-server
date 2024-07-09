{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

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

module Network.Wai.Utilities.Server
  ( -- * Server Setup
    Server (..),
    defaultServer,
    newSettings,
    runSettingsWithShutdown,
    runSettingsWithCleanup,
    compile,
    route,

    -- * Middlewares
    requestIdMiddleware,
    catchErrors,
    catchErrorsWithRequestId,
    heavyDebugLogging,
    rethrow5xx,
    lazyResponseBody,

    -- * Utilities
    onError,
    logError,
    logError',
    logErrorMsg,
    restrict,
    flushRequestBody,

    -- * Constants
    defaultRequestIdHeaderName,
    federationRequestIdHeaderName,
  )
where

import Control.Error.Util ((?:))
import Control.Exception (AsyncException (..), throwIO)
import Control.Monad.Catch hiding (onError, onException)
import Data.Aeson (decode, encode)
import Data.ByteString (toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as LBS
import Data.Domain (domainText)
import Data.Metrics.GC (spawnGCMetricsCollector)
import Data.Streaming.Zlib (ZlibException (..))
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal (TimeoutThread)
import Network.Wai.Internal qualified as WaiInt
import Network.Wai.Predicate hiding (Error, err, status)
import Network.Wai.Predicate qualified as P
import Network.Wai.Routing.Route (App, Continue, Routes, Tree)
import Network.Wai.Routing.Route qualified as Route
import Network.Wai.Utilities.Error qualified as Error
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.JSONResponse
import Network.Wai.Utilities.Request (lookupRequestId)
import Network.Wai.Utilities.Response
import Prometheus qualified as Prom
import System.Logger qualified as Log
import System.Logger.Class hiding (Error, Settings, format)
import System.Posix.Signals (installHandler, sigINT, sigTERM)
import System.Posix.Signals qualified as Sig

--------------------------------------------------------------------------------
-- Server Setup

data Server = Server
  { serverHost :: String,
    serverPort :: Word16,
    serverLogger :: Logger,
    serverTimeout :: Maybe Int
  }

defaultServer :: String -> Word16 -> Logger -> Server
defaultServer h p l = Server h p l Nothing

newSettings :: (MonadIO m) => Server -> m Settings
newSettings (Server h p l t) = do
  pure
    $ setHost (fromString h)
      . setPort (fromIntegral p)
      . setBeforeMainLoop logStart
      . setOnOpen (const $ connStart >> pure True)
      . setOnClose (const connEnd)
      . setTimeout (fromMaybe 300 t)
    $ defaultSettings
  where
    connStart = Prom.incGauge netConnections
    connEnd = Prom.decGauge netConnections
    logStart =
      Log.info l . msg $
        val "Listening on " +++ h +++ ':' +++ p

{-# NOINLINE netConnections #-}
netConnections :: Prom.Gauge
netConnections =
  Prom.unsafeRegister $
    Prom.gauge
      Prom.Info
        { Prom.metricName = "net_connections",
          Prom.metricHelp = "Number of active connections"
        }

-- Run a WAI 'Application', initiating Warp's graceful shutdown
-- on receiving either the INT or TERM signals. After closing
-- the listen socket, Warp will be allowed to drain existing
-- connections up to the given number of seconds.
--
-- See also: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7681
runSettingsWithShutdown :: Settings -> Application -> Maybe Int -> IO ()
runSettingsWithShutdown = runSettingsWithCleanup $ pure ()

-- As above, but with an additional cleanup action that is called before the server shuts down.
runSettingsWithCleanup :: IO () -> Settings -> Application -> Maybe Int -> IO ()
runSettingsWithCleanup cleanup s app (fromMaybe defaultShutdownTime -> secs) = do
  initialization
  let s' =
        setInstallShutdownHandler catchSignals
          . setGracefulShutdownTimeout (Just secs)
          $ s
  runSettings s' app
  where
    initialization :: IO ()
    initialization = do
      spawnGCMetricsCollector
    catchSignals closeSocket = do
      void $ installHandler sigINT (Sig.CatchOnce $ finally cleanup closeSocket) Nothing
      void $ installHandler sigTERM (Sig.CatchOnce $ finally cleanup closeSocket) Nothing

defaultShutdownTime :: Int
defaultShutdownTime = 30

compile :: (Monad m) => Routes a m b -> Tree (App m)
compile routes = Route.prepare (Route.renderer predicateError >> routes)
  where
    predicateError e = pure (encode $ Wai.mkError (P.status e) "client-error" (format e), [jsonContent])
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

route :: (MonadIO m) => Tree (App m) -> Request -> Continue IO -> m ResponseReceived
route rt rq k = Route.routeWith (Route.Config $ errorRs' noEndpoint) rt rq (liftIO . k)
  where
    noEndpoint = Wai.mkError status404 "no-endpoint" "The requested endpoint does not exist"
{-# INLINEABLE route #-}

--------------------------------------------------------------------------------
-- Middlewares

requestIdMiddleware :: Logger -> HeaderName -> Middleware
requestIdMiddleware logger reqIdHeaderName origApp req responder =
  case lookup reqIdHeaderName req.requestHeaders of
    Just _ -> origApp req responder
    Nothing -> do
      reqId <- Text.encodeUtf8 . UUID.toText <$> UUID.nextRandom
      unless (req.rawPathInfo `elem` ["/i/status", "/i/metrics", "/api-version"]) $
        Log.info logger $
          msg ("generated a new request id for local request" :: ByteString)
            . field "request" reqId
            . field "method" (requestMethod req)
            . field "path" (rawPathInfo req)
      let reqWithId = req {requestHeaders = (reqIdHeaderName, reqId) : req.requestHeaders}
      origApp reqWithId responder

catchErrors :: Logger -> HeaderName -> Middleware
catchErrors l reqIdHeaderName = catchErrorsWithRequestId (lookupRequestId reqIdHeaderName) l

-- | Create a middleware that catches exceptions and turns
-- them into appropriate 'Error' responses, thereby logging
-- as well as counting server errors (i.e. exceptions that
-- yield 5xx responses).
--
-- This does not log any 'Response' values with error status.
-- See 'catchErrors'.
catchErrorsWithRequestId ::
  (Request -> Maybe ByteString) ->
  Logger ->
  Middleware
catchErrorsWithRequestId getRequestId l app req k =
  rethrow5xx getRequestId l app req k `catch` errorResponse
  where
    mReqId = getRequestId req

    errorResponse :: SomeException -> IO ResponseReceived
    errorResponse ex = do
      er <- runHandlers ex errorHandlers
      onError l mReqId req k er

{-# INLINEABLE catchErrors #-}

-- | Standard handlers for turning exceptions into appropriate
-- 'Error' responses.
errorHandlers :: [Handler IO (Either Wai.Error JSONResponse)]
errorHandlers =
  -- a Wai.Error can be converted to a JSONResponse, but doing so here would
  -- prevent us from logging the error cleanly later
  [ Handler $ \(x :: JSONResponse) -> pure (Right x),
    Handler $ \(x :: Wai.Error) -> pure (Left x),
    -- warp throws 'ThreadKilled' when the client is gone or when it thinks it's
    -- time to reap the worker thread. Here, there is no point trying to respond
    -- nicely and there is no point logging this as it happens regularly when a
    -- client just closes a long running connection without consuming the whole
    -- body.
    Handler $ \(x :: AsyncException) ->
      case x of
        ThreadKilled -> throwIO x
        _ ->
          pure . Left $
            Wai.mkError status500 "server-error" ("Server Error. " <> LT.pack (displayException x)),
    Handler $ \(_ :: InvalidRequest) ->
      pure . Left $
        Wai.mkError status400 "client-error" "Invalid Request",
    Handler $ \(_ :: TimeoutThread) ->
      pure . Left $
        Wai.mkError status408 "client-error" "Request Timeout",
    Handler $ \case
      ZlibException (-3) ->
        pure . Left $
          Wai.mkError status400 "client-error" "Invalid request body compression"
      ZlibException _ ->
        pure . Left $
          Wai.mkError status500 "server-error" "Server Error",
    Handler $ \(e :: SomeException) ->
      pure . Left $
        Wai.mkError status500 "server-error" ("Server Error. " <> LT.pack (displayException e))
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
  HeaderName ->
  Middleware
heavyDebugLogging sanitizeReq lvl lgr reqIdHeaderName app = \req cont -> do
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
          field "request" (fromMaybe "N/A" $ lookupRequestId reqIdHeaderName req)
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
  tvar <- newTVarIO (toStrict lbs)
  -- Emit the bytestring on the first read, then always return "" on subsequent reads
  pure . atomically $ swapTVar tvar mempty

-- | Run the 'Application'; check the response status; if >=500, throw a 'Wai.Error' with
-- label @"server-error"@ and the body as the error message.
rethrow5xx :: (Request -> Maybe ByteString) -> Logger -> Middleware
rethrow5xx getRequestId logger app req k = app req k'
  where
    k' resp@WaiInt.ResponseRaw {} = do
      -- See Note [Raw Response]
      let logMsg =
            field "canoncalpath" (show $ pathInfo req)
              . field "rawpath" (rawPathInfo req)
              . field "request" (fromMaybe "N/A" $ getRequestId req)
              . msg (val "ResponseRaw - cannot collect metrics or log info on errors")
      Log.log logger Log.Debug logMsg
      k resp
    k' resp = do
      let st = responseStatus resp
      if statusCode st < 500
        then k resp
        else do
          rsbody <- liftIO (lazyResponseBody resp)
          throwM $ wrapError st rsbody

-- | Wrap the body of an HTTP error into a Wai.Error structure.
--
-- If the error is already a JSON serialisation of a Wai.Error, avoid creating
-- an unnecessary wrapper.
wrapError :: Status -> LByteString -> Wai.Error
wrapError st body =
  decode body ?:
    Wai.mkError st "server-error" (LT.decodeUtf8With lenientDecode body)

-- | This flushes the response!  If you want to keep using the response, you need to construct
-- a new one with a fresh body stream.
lazyResponseBody :: Response -> IO LByteString
lazyResponseBody rs = case responseToStream rs of
  (_, _, cont :: (StreamingBody -> IO ()) -> IO ()) -> do
    bref <- newIORef mempty
    let pushstream builder = modifyIORef bref (<> builder)
    cont $ \streamingBody ->
      streamingBody pushstream (pure ())
    toLazyByteString <$> readIORef bref

--------------------------------------------------------------------------------
-- Utilities

-- | Send an 'Error' response.
onError ::
  (MonadIO m) =>
  Logger ->
  Maybe ByteString ->
  Request ->
  Continue IO ->
  Either Wai.Error JSONResponse ->
  m ResponseReceived
onError g mReqId r k e = liftIO $ do
  case e of
    Left we -> logError' g mReqId we
    Right jr -> logJSONResponse g mReqId jr
  let resp = either waiErrorToJSONResponse id e
  let code = statusCode (resp.status)
  when (code >= 500) $ Prom.incCounter netErrors
  flushRequestBody r
  k (jsonResponseToWai resp)

{-# NOINLINE netErrors #-}
netErrors :: Prom.Counter
netErrors =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "net_errors",
          Prom.metricHelp = "Number of exceptions caught by catchErrors middleware"
        }

defaultRequestIdHeaderName :: HeaderName
defaultRequestIdHeaderName = "Request-Id"

federationRequestIdHeaderName :: HeaderName
federationRequestIdHeaderName = "Wire-Origin-Request-Id"

-- | Log an 'Error' response for debugging purposes.
--
-- It would be nice to have access to the request body here, but that's already streamed away
-- by the handler in all likelyhood.  See 'heavyDebugLogging'.
logError :: (MonadIO m) => Logger -> Maybe Request -> Wai.Error -> m ()
logError g mr = logError' g (lookupRequestId defaultRequestIdHeaderName =<< mr)

logError' :: (MonadIO m) => Logger -> Maybe ByteString -> Wai.Error -> m ()
logError' g mr e = liftIO $ doLog g (logErrorMsgWithRequest mr e)
  where
    doLog
      | statusCode (Error.code e) >= 500 = Log.err
      | otherwise = Log.debug

logJSONResponse :: (MonadIO m) => Logger -> Maybe ByteString -> JSONResponse -> m ()
logJSONResponse g mReqId e = do
  let r = fromMaybe "N/A" mReqId
  liftIO $
    doLog g $
      field "request" r
        . field "code" status
        . field "value" (encode e.value)
  where
    status = statusCode e.status
    doLog
      | status >= 500 = Log.err
      | otherwise = Log.debug

logErrorMsg :: Wai.Error -> Msg -> Msg
logErrorMsg (Wai.Error c l m md inner) =
  field "code" (statusCode c)
    . field "label" l
    . maybe id logErrorData md
    . msg (val "\"" +++ m +++ val "\"")
    . maybe id logErrorMsg inner
  where
    logErrorData (Wai.FederationErrorData d p) =
      field "domain" (domainText d)
        . field "path" p

logErrorMsgWithRequest :: Maybe ByteString -> Wai.Error -> Msg -> Msg
logErrorMsgWithRequest mr e =
  field "request" (fromMaybe "N/A" mr) . logErrorMsg e

runHandlers :: SomeException -> [Handler IO a] -> IO a
runHandlers e [] = throwIO e
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
