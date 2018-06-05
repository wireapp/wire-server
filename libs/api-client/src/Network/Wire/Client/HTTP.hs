{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wire.Client.HTTP
    ( clientRequest
    , acceptJson
    , readBody
    , fromBody
    , unexpected
    , mkErrorResponse
    ) where

import Bilge
import Control.Error
import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import Data.Aeson hiding (Error)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Network.HTTP.Types.Status hiding (statusCode)
import Network.HTTP.Types.Header (hUserAgent)
import Network.Wire.Client.Monad

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Client  as Rq
import qualified System.Logger        as Log

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

-------------------------------------------------------------------------------
-- Performing Requests

clientRequest :: MonadClient m
              => Request                       -- ^ The request to send.
              -> NonEmpty Status               -- ^ Expected response codes.
              -> (Response BodyReader -> IO a) -- ^ Handler function.
              -> m a
clientRequest rq expected f = do
    s <- getServer
    l <- getLogger
    m <- getManager
    liftIO $ recovering retry3x handlers (const (exec l s m))
  where
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

fromBody :: (MonadIO m, FromJSON a) => Response (Maybe Lazy.ByteString) -> m a
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
