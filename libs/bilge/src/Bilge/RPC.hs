{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Bilge.RPC
  ( HasRequestId (..),
    RPCException (..),
    rpc,
    rpc',
    statusCheck,
    parseResponse,
    rpcExceptionMsg,
  )
where

import Bilge.IO
import Bilge.Request
import Bilge.Response
import Control.Error hiding (err)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Except
import Data.Aeson (FromJSON, eitherDecode')
import Data.CaseInsensitive (original)
import Data.Text.Lazy (pack)
import Imports hiding (log)
import qualified Network.HTTP.Client as HTTP
import System.Logger.Class
import UnliftIO.Exception (try)

class HasRequestId m where
  getRequestId :: m RequestId

data RPCException
  = RPCException
      { rpceRemote :: !LText,
        rpceRequest :: !Request,
        rpceCause :: !SomeException
      }
  deriving (Typeable)

instance Exception RPCException

instance Show RPCException where
  showsPrec _ (RPCException r rq (SomeException c)) =
    showString "RPCException {"
      . showString "remote = "
      . shows r
      . showString ", path = "
      . shows (HTTP.path rq)
      . showString ", headers = "
      . shows (HTTP.requestHeaders rq)
      . showString ", cause = "
      . shows c
      . showString "}"

rpc ::
  (MonadUnliftIO m, MonadHttp m, HasRequestId m, MonadLogger m, MonadThrow m) =>
  LText ->
  (Request -> Request) ->
  m (Response (Maybe LByteString))
rpc sys = rpc' sys empty

-- | Perform an HTTP request and return the response, thereby
-- forwarding the @Request-Id@ header from the current monadic
-- context.
-- Note: 'syncIO' is wrapped around the IO action performing the request
--       and any exceptions caught are re-thrown in an 'RPCException'.
rpc' ::
  (MonadUnliftIO m, MonadHttp m, HasRequestId m, MonadThrow m) =>
  -- | A label for the remote system in case of 'RPCException's.
  LText ->
  Request ->
  (Request -> Request) ->
  m (Response (Maybe LByteString))
rpc' sys r f = do
  rId <- getRequestId
  let rq = f . requestId rId $ r
  res <- try $ httpLbs rq id
  case res of
    Left x -> throwM $ RPCException sys rq x
    Right x -> return x

rpcExceptionMsg :: RPCException -> Msg -> Msg
rpcExceptionMsg (RPCException sys req ex) =
  "remote" .= sys ~~ "path" .= HTTP.path req ~~ headers ~~ msg (show ex)
  where
    headers = foldr hdr id (HTTP.requestHeaders req)
    hdr (k, v) x = x ~~ original k .= v

statusCheck ::
  (MonadError e m, MonadIO m) =>
  Int ->
  (LText -> e) ->
  Response (Maybe LByteString) ->
  m ()
statusCheck c f r =
  unless (statusCode r == c)
    $ throwError
    $ f ("unexpected status code: " <> pack (show $ statusCode r))

parseResponse ::
  (Exception e, MonadThrow m, Monad m, FromJSON a) =>
  (LText -> e) ->
  Response (Maybe LByteString) ->
  m a
parseResponse f r = either throwM return $ do
  b <- note (f "no response body") (responseBody r)
  fmapL (f . pack) (eitherDecode' b)
