{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Utilities.Server
    ( -- * Server Setup
      Server (..)
    , defaultServer
    , newSettings
    , runSettingsWithShutdown
    , compile
    , route

      -- * Middlewares
    , measureRequests
    , catchErrors

      -- * Utilities
    , onError
    , logError
    , logIO
    , runHandlers
    , restrict
    , flushRequestBody
    ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (throw, throwIO)
import Control.Monad (when, unless, void)
import Control.Monad.Catch hiding (onException)
import Control.Monad.IO.Class
import Data.Aeson (encode)
import Data.ByteString.Builder
import Data.Foldable (for_)
import Data.Functor.Identity
import Data.Maybe (fromMaybe, catMaybes)
import Data.Metrics.Middleware
import Data.Monoid
import Data.Streaming.Zlib (ZlibException (..))
import Data.String (fromString)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word16)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal (TimeoutThread)
import Network.Wai.Predicate hiding (Error, err, status)
import Network.Wai.Routing.Route (Routes, Tree, App, Continue)
import Network.Wai.Utilities.Error (Error (Error))
import Network.Wai.Utilities.Request (lookupRequestId)
import Network.Wai.Utilities.Response
import Prelude
import System.Logger.Class hiding (Settings, Error, format)
import System.Posix.Signals (installHandler, sigINT, sigTERM)

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text.Lazy.Encoding     as LT
import qualified Network.Wai.Predicate       as P
import qualified Network.Wai.Routing.Route   as Route
import qualified Network.Wai.Utilities.Error as Error
import qualified System.Logger               as Log
import qualified System.Posix.Signals        as Sig

--------------------------------------------------------------------------------
-- Server Setup

data Server = Server
    { serverHost                :: String
    , serverPort                :: Word16
    , serverLogger              :: Logger
    , serverMetrics             :: Metrics
    , serverTimeout             :: Maybe Int
    , serverOnException         :: [Maybe Request -> Handler IO ()]
    , serverOnExceptionResponse :: [Handler Identity Response]
    }

defaultServer :: String -> Word16 -> Logger -> Metrics -> Server
defaultServer h p l m = Server h p l m Nothing [] []

newSettings :: MonadIO m => Server -> m Settings
newSettings (Server h p l m t el er) = do
    -- (Atomically) initialise the standard metrics, to avoid races.
    void $ gaugeGet' (path "net.connections") m
    void $ counterGet' (path "net.errors") m
    return $ setHost (fromString h)
           . setPort (fromIntegral p)
           . setBeforeMainLoop logStart
           . setOnOpen (const $ connStart >> return True)
           . setOnClose (const connEnd)
           . setOnException onException
           . setOnExceptionResponse onExceptionResponse
           . setTimeout (fromMaybe 300 t)
           $ defaultSettings
  where
    connStart = gaugeIncr (path "net.connections") m
    connEnd   = gaugeDecr (path "net.connections") m

    logStart = Log.info l . msg $
        val "Listening on " +++ h +++ ':' +++ p

    onException r e = for_ r flushRequestBody >> (runHandlers e $
        map ($ r) el ++
        [ Handler $ \(x :: Error) -> do
            logError l r x
            when (statusCode (Error.code x) >= 500) $
                counterIncr (path "net.errors") m
        , Handler $ \(ZlibException (-3)) -> logIO l Info r $
            val "Invalid zlib compression in request body."
        , Handler $ \(x :: SomeException) ->
            if defaultShouldDisplayException x
                then do
                    logIO l Log.Error r (show e)
                    counterIncr (path "net.errors") m
                else logIO l Log.Trace r (show e)
        ])

    onExceptionResponse e = runIdentity . runHandlers e $
        er ++ map (fmap errorRs') errorHandlers

-- Run a WAI 'Application', initiating Warp's graceful shutdown
-- on receiving either the INT or TERM signals. After closing
-- the listen socket, Warp will be allowed to drain existing
-- connections up to the given number of seconds.
runSettingsWithShutdown :: Settings -> Application -> Word16 -> IO ()
runSettingsWithShutdown s app secs = do
    latch <- newEmptyMVar
    let s' = setInstallShutdownHandler (catchSignals latch) s
    srv <- async $ runSettings s' app `finally` void (tryPutMVar latch ())
    takeMVar latch
    await srv secs
  where
    catchSignals latch closeSocket = do
        let shutdown = closeSocket >> putMVar latch ()
        void $ installHandler sigINT  (Sig.CatchOnce shutdown) Nothing
        void $ installHandler sigTERM (Sig.CatchOnce shutdown) Nothing

    await srv t = do
        status <- poll srv
        case status of
            Nothing | t > 0 -> threadDelay 1000000 >> await srv (t - 1)
            Just (Left  ex) -> throwIO ex
            _               -> cancel srv

compile :: Monad m => Routes a m b -> Tree (App m)
compile routes = Route.prepare (Route.renderer predicateError >> routes)
  where
    predicateError e = return (encode $ Error (P.status e) "client-error" (format e), [jsonContent])

    -- [label] 'source' reason: message
    format e =
        let l = labelStr   $  labels e
            s = sourceStr <$> source e
            r = reasonStr <$> reason e
            t = message e
        in case catMaybes [l, s, r] of
            [] -> maybe "N/A" (LT.decodeUtf8With lenientDecode . LBS.fromStrict) t
            bs -> LT.decodeUtf8With lenientDecode . toLazyByteString $ mconcat bs <> messageStr t

    labelStr [] = Nothing
    labelStr ls = Just
                $  char7 '['
                <> byteString (C.intercalate "," ls)
                <> char7 ']'
                <> char7 ' '

    sourceStr s = char7 '\'' <> byteString s <> char7 '\'' <> char7 ' '

    reasonStr NotAvailable = "required"
    reasonStr TypeError    = "invalid"

    messageStr (Just t) = char7 ':' <> char7 ' ' <> byteString t
    messageStr Nothing  = mempty

route :: (MonadCatch m, MonadIO m) => Tree (App m) -> Request -> Continue IO -> m ResponseReceived
route rt rq k = Route.routeWithCustomNotFoundResponse (errorRs' notFound) rt rq (liftIO . k)
  where
    notFound :: Error
    notFound = Error status404 "no-such-endpoint" "The requested endpoint does not exist"
{-# INLINEABLE route #-}

--------------------------------------------------------------------------------
-- Middlewares

-- | Create a middleware that tracks detailed request / response
-- statistics, including timing information, for every path in the
-- given routing tree.
--
-- Note: For accurate metrics on error responses, this middleware
-- should be combined with the 'catchErrors' middleware.
measureRequests :: Monad m => Metrics -> Tree (App m) -> Middleware
measureRequests m rtree = withPathTemplate rtree $ \p ->
      requestCounter m p . duration 30 12 m p
{-# INLINEABLE measureRequests #-}

-- | Create a middleware that catches exceptions and turns
-- them into appropriate 'Error' responses, thereby logging
-- as well as counting server errors (i.e. exceptions that
-- yield 5xx responses).
catchErrors :: Logger -> Metrics -> Middleware
catchErrors l m app req k =
    app req k `catch` errorResponse
  where
    errorResponse ex = do
        er <- runHandlers ex errorHandlers
        when (statusCode (Error.code er) >= 500) $
            logIO l Log.Error (Just req) (show ex)
        onError l m req k er
{-# INLINEABLE catchErrors #-}

-- | Standard handlers for turning exceptions into appropriate
-- 'Error' responses.
errorHandlers :: Applicative m => [Handler m Error]
errorHandlers =
    [ Handler $ \(x :: Error)          -> pure x
    , Handler $ \(_ :: InvalidRequest) -> pure $ Error status400 "client-error" "Invalid Request"
    , Handler $ \(_ :: TimeoutThread)  -> pure $ Error status408 "client-error" "Request Timeout"
    , Handler $ \(ZlibException (-3))  -> pure $ Error status400 "client-error" "Invalid request body compression"
    , Handler $ \(_ :: SomeException)  -> pure $ Error status500 "server-error" "Server Error"
    ]
{-# INLINE errorHandlers #-}

--------------------------------------------------------------------------------
-- Utilities

-- | Send an 'Error' response.
onError :: MonadIO m => Logger -> Metrics -> Request -> Continue IO -> Error -> m ResponseReceived
onError g m r k e = liftIO $ do
    logError g (Just r) e
    when (statusCode (Error.code e) >= 500) $
        counterIncr (path "net.errors") m
    flushRequestBody r
    k (errorRs' e)

-- | Log an 'Error' response for debugging purposes.
logError :: MonadIO m => Logger -> Maybe Request -> Error -> m ()
logError g r (Error c l m) = liftIO $ Log.debug g logMsg
  where
    logMsg = field "code" (statusCode c)
           . field "label" l
           . field "request" (fromMaybe "N/A" (lookupRequestId =<< r))
           . msg (val "\"" +++ m +++ val "\"")

logIO :: ToBytes a => Logger -> Level -> Maybe Request -> a -> IO ()
logIO lg lv r a =
    let reqId = field "request" . fromMaybe "N/A" . lookupRequestId <$> r
        mesg  = fromMaybe id reqId . msg a
    in Log.log lg lv mesg

runHandlers :: SomeException -> [Handler m a] -> m a
runHandlers e []             = throw e
runHandlers e (Handler h:hs) = maybe (runHandlers e hs) h (fromException e)

restrict :: Int -> Int -> Predicate r P.Error Int -> Predicate r P.Error Int
restrict l u = fmap $ \x -> x >>= \v ->
    if v >= l && v <= u
        then x
        else Fail (setMessage (emsg v) . setReason TypeError $ e400)
  where
    emsg v = LBS.toStrict . toLazyByteString
        $ byteString "outside range ["
       <> intDec l
       <> byteString ", "
       <> intDec u
       <> byteString "]: "
       <> intDec v

flushRequestBody :: Request -> IO ()
flushRequestBody req = do
    bs <- requestBody req
    unless (BS.null bs) $
        flushRequestBody req
