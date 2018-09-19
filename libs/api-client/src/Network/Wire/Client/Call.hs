{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE GADTs               #-}

-- | Machinery for constructing and performing API calls.
module Network.Wire.Client.Call
    (
    -- * Constructing API calls
      Call (..)
    , Server (..)
    , mkCall

    -- * Performing API calls
    -- ** Environment
    , MonadClient (..)
    , ClientEnv (..)
    , Auth (..)
    -- ** Calling in clients and RPC routines
    , call
    -- ** Calling in tests
    , callTest
    , (<!!)
    , (!!!)

    -- * Assertions
    , expect2xx, expect3xx, expect4xx, expect5xx
    , expectStatus, expectStatusIn
    -- , expectError, expectErrorIn
    -- , expectBody

    -- * Exceptions
    , CallException (..)
    , AssertionFailure (..)
    ) where

import Control.Error
import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import GHC.Stack (HasCallStack)
import Data.ByteString (ByteString)
import Data.Word
import Data.Typeable
import Data.Aeson hiding (Error)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Network.HTTP.Types.Status hiding (statusCode)
import Network.HTTP.Types.Header (hUserAgent)
import Network.HTTP.Client (Request, Response, Manager)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Client  as Rq
import qualified System.Logger        as Log
import qualified Bilge.Assert         as Bilge

data Error = Error
    { code    :: Int
    , label   :: Text
    , message :: Text
    } deriving (Show)

instance FromJSON Error where
    parseJSON = withObject "error" $ \o ->
        Error <$> o .: "code"
              <*> o .: "label"
              <*> o .: "message"

type ResponseLBS = Response (Maybe Lazy.ByteString)

data Service = Brig | Galley | Gundeck | Cargohold | Cannon | Proxy | Spar
    deriving (Eq, Ord, Show)

data Env = Env
    { clientServer  :: Service -> Server
    , clientManager :: Manager
    }

data Server = Server
    { serverHost    :: ByteString
    , serverPort    :: Word16
    , serverWsHost  :: Maybe ByteString
    , serverWsPort  :: Maybe Word16
    , serverSSL     :: Bool
    }

class MonadClient m where
    getServer :: Service -> m Server
    getManager :: m Manager

-- TODO: allow picking whether to log requests or not

setServer :: Server -> Request -> Request
setServer (Server h p _ _ s) = host h . port p . (if s then secure else id)

----------------------------------------------------------------------------
-- Calling machinery

-- | An API call. Can be performed in several different ways (e.g. you might
-- want to retry, or to override the assertions, etc).
data Call a = Call
    {
    -- | Service that the call should be made against.
      service :: Service
    -- | Request that should be sent to perform the call.
    , request :: Request
    -- | Default checks for the response. Can be overridden.
    , assertions :: Assertions
    }

-- | Perform an API call, execute the default assertions (most calls will
-- simply contain something like a check for status code 200), and parse the
-- response as JSON.
--
-- Retry policy: will retry 3 times, but only if the request is idempotent
-- and only for selected error codes (408, 420, 500, 502, 503, 504).
--
-- Authentication policy: will /always/ attempt to reauthenticate
-- ('refreshAuth') upon getting a 401, even if 401 is the expected status
-- code.
--
-- /Note:/ if the result type is '()', 'call' will expect an empty body, not
-- the JSON encoding of '()'.
call :: ( HasCallStack, MonadClient m
        , Typeable a, FromJSON a )
     => Call a -> m a
call (Call service_ request_ assertions_) = do
    server <- getServer service_
    manager <- getManager
    let request' = request_
            & setServer server
            & header hUserAgent "api-client"
    let makeRequest = withResponse request' manager $ \response -> do
            when (statusCode (responseStatus response) == 401) $ do
                refreshAuth
                throwIO (Unauthenticated request' response)
            case execWriter (assertions_ response) of
                [] -> decodeResponse (request', response)
                failures -> throwIO (AssertionFailures request' response failures)
    recovering retry3x handlers (const makeRequest)
  where
    idempotent = (`elem` ["GET", "PUT", "DELETE", "HEAD", "OPTIONS"])
    canRetry   = (`elem` [401, 408, 420, 500, 502, 503, 504])
    retry3x    = limitRetries 3 <> exponentialBackoff 1000000
    handlers   =
        skipAsyncExceptions ++
        -- TODO log retries
        [ const $ Handler $ \(e :: CallException) -> do
              let method_ = method (exRequest e)
                  code_   = statusCode (responseStatus (exResponse e))
              pure (idempotent method_ && canRetry code_)
        , const $ Handler $ \(_ :: HttpException) ->
              pure True
        , const $ Handler $ \(_ :: SomeException) ->
              pure False
        ]

-- | Perform an API call, execute the default assertions (most calls will
-- simply contain something like a check for status code 200), and parse the
-- response as JSON.
--
-- Does not attempt to retry or reauthenticate.
--
-- /Note:/ if the result type is '()', 'call' will expect an empty body, not
-- the JSON encoding of '()'.
callTest :: ( HasCallStack, MonadClient m
            , Typeable a, FromJSON a )
         => Call a -> m a
callTest (Call service_ request_ assertions_) = do
    server <- getServer service_
    manager <- getManager
    let request' = request_
            & setServer server
            & header hUserAgent "api-client"
    withResponse request' manager $ \response -> do
        case execWriter (assertions_ response) of
            [] -> decodeResponse (request', response)
            failures -> throwIO (AssertionFailures request' response failures)

-- | Perform an API call and override the default assertions.
(<!!) :: ( HasCallStack, MonadClient m
         , Typeable a, FromJSON a )
      => Call m a -> Assertions -> m a
(<!!) call_ assertions_ = callTest (call_ { assertions = assertions_ })

-- | Perform an API call and override the default assertions, like ('<!!'),
-- but discard the response body.
--
-- Does not check that the response can be parsed, and therefore shouldn't
-- be used for requests that are supposed to succeed. If you want to check
-- that the response is parseable, use ('<!!').
(!!!) :: (HasCallStack, MonadClient m)
      => Call m a -> Assertions -> m ()
(!!!) (Call service_ request_ _) assertions_ = do
    server <- getServer service_
    manager <- getManager
    let request' = request_
            & setServer server
            & header hUserAgent "api-client"
    withResponse request' manager $ \response -> do
        case execWriter (assertions_ response) of
            [] -> pure ()
            failures -> throwIO (AssertionFailures request' response failures)

-- | An exception that can happen during a call.
data CallException
    = MissingBody
          { exRequest :: Request
          , exResponse :: ResponseLBS }
    | UnexpectedBody
          { exRequest :: Request
          , exResponse :: ResponseLBS }
    | UnparseableBody
          { exRequest :: Request
          , exResponse :: ResponseLBS
          , exResponseType :: TypeRep
          , exParseError :: String }
    | AssertionFailures
          { exRequest :: Request
          , exResponse :: Response
          , exFailures :: [AssertionFailure] }
    | Unauthenticated
          { exRequest :: Request
          , exResponse :: Response }
    deriving Show

instance Exception CallException

-- | A response assertion failure. Outside of this module you should always
-- expect it to be wrapped into 'CallException'.
data AssertionFailure
    = WrongStatus
          { afExpected :: Text
          , afStatus :: Status }
    | WrongError
          { afExpected :: Text
          , afError :: Error }
    | NoError
          { afExpected :: Text }

instance Exception AssertionFailure

----------------------------------------------------------------------------
-- Utilities

-- | Decode response body as JSON and throw a 'CallException' if it can't be
-- done.
--
-- /Note:/ if the result type is '()', 'decodeResponse' will expect an empty
-- body, not the JSON encoding of '()'.
decodeResponse
    :: forall m a. (HasCallStack, MonadThrow m, Typeable a, FromJSON a)
    => (Request, ResponseLBS)
    -> m a
decodeResponse (request, response) =
    case responseBody response of
        Nothing -> case eqT @a @() of
            Nothing   -> throwM (MissingResponseBody request response)
            Just Refl -> pure ()
        Just body_ -> case eqT @a @() of
            Just Refl -> throwM (UnexpectedResponseBody request response)
            Nothing   -> case eitherDecode' body_ of
                Right result    -> pure result
                Left parseError ->
                    let responseType_ = typeRep (Proxy @a)
                    in throwM $ UnparseableResponseBody
                                    request response responseType_ parseError

----------------------------------------------------------------------------
-- Assertions

type Assertions = ResponseLBS -> Writer [AssertionFailure] ()

expect2xx :: Assertions
expect2xx = expectStatus "code 2xx" (\code -> code `div` 100 == 2)

expect3xx :: Assertions
expect3xx = expectStatus "code 3xx" (\code -> code `div` 100 == 3)

expect4xx :: Assertions
expect4xx = expectStatus "code 4xx" (\code -> code `div` 100 == 4)

expect5xx :: Assertions
expect5xx = expectStatus "code 5xx" (\code -> code `div` 100 == 5)

expectStatus
    :: Text           -- ^ Description of expected status
    -> (Int -> Bool)  -- ^ Predicate on the status code
    -> Assertions
expectStatus expected predicate resp =
    unless (predicate (statusCode (responseStatus resp))) $
        tell [WrongStatus expected (responseStatus resp)]

expectStatusIn :: [Int] -> Assertions
expectStatusIn codes = expectStatus ("one of " ++ show codes) (`elem` codes)

-- TODO: expectBody
-- TODO: expectError

-------------------------------------------------------------------------------
-- Performing Requests

clientRequest
    :: MonadClient m
    => Service                       -- ^ Service to send the request to.
    -> (Request -> Request)          -- ^ The request to send. This function
                                     --   will be applied to 'empty' to get
                                     --   the request.
    -> Bilge.Assertions ()           -- ^ Checks for the response.
    -> (Response BodyReader -> IO a) -- ^ Handler function.
    -> Call m a
clientRequest service rqBuilder assertions f = do
    s <- getServer service
    l <- getLogger
    m <- getManager
    liftIO $ recovering retry3x handlers (const (exec l s m))
  where
    rq = rqBuilder empty
    idempotent = Rq.method rq `elem` ["GET", "PUT", "DELETE", "HEAD", "OPTIONS"]
    canRetry c = idempotent && c `elem` [408, 420, 500, 502, 503, 504]
    retry3x    = limitRetries 3 <> exponentialBackoff 1000000
    handlers   = [ const $ Handler (\(e :: ClientException) -> case e of
                        ErrorResponse c _ _ -> return (canRetry c)
                        x                   -> throwIO x)
                 , const $ Handler (\(e :: SomeException)   -> throwIO e)
                 ]
    exec l s m = do
        let rq' = rq & setServer s
                     & header hUserAgent "api-client"
        Log.debug l $ Log.msg (show rq')
        withResponse rq' m $ \rs -> do
            Log.debug l $ Log.msg $ show (rs { responseBody = "" :: String })
            if responseStatus rs `elem` toList expected
                then f rs
                else if (statusCode rs `div` 100) `elem` [4,5]
                    then mkErrorResponse rs >>= throwIO
                    else unexpected rs ""

-------------------------------------------------------------------------------
-- Utilities

acceptJson :: Request -> Request
acceptJson = accept "application/json"

readBody :: FromJSON a => Response BodyReader -> IO a
readBody = consumeBody >=> fromBody

fromBody :: (MonadIO m, FromJSON a) => ResponseLBS -> m a
fromBody = either (liftIO . throwIO . ParseError . ("fromBody: "<>)) return . parse
  where
    parse = maybe (Left "missing response body")
                  (fmapL pack . eitherDecode)
                  .
                  responseBody

unexpected :: MonadIO m => Response a -> Text -> m b
unexpected r = liftIO . throwIO . UnexpectedResponse (responseStatus r) (responseHeaders r)

mkErrorResponse :: Response BodyReader -> IO ClientException
mkErrorResponse rs = do
    r <- consumeBody rs
    let re = maybe (Left "N/A")
                   (\bdy -> fmapL (const . T.decodeLatin1 $ Lazy.toStrict bdy)
                                  (eitherDecode bdy))
                   (responseBody r)
    return $ case re of
        Left  m -> ErrorResponse (statusCode rs) "N/A" m
        Right e -> ErrorResponse (code e) (label e) (message e)
