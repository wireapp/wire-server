{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad.Catch (MonadCatch, MonadThrow (..), try)
import Control.Monad.Except
import Data.Aeson (FromJSON, eitherDecode')
import Data.CaseInsensitive (original)
import Data.Text.Lazy (pack)
import Imports hiding (log)
import Network.HTTP.Client qualified as HTTP
import System.Logger.Class

class HasRequestId m where
  getRequestId :: m RequestId

instance (Monad m) => HasRequestId (ReaderT RequestId m) where
  getRequestId = ask

data RPCException = RPCException
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
  (MonadIO m, MonadCatch m, MonadHttp m, HasRequestId m) =>
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
  (MonadIO m, MonadCatch m, MonadHttp m, HasRequestId m) =>
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
    Right x -> pure x

rpcExceptionMsg :: RPCException -> Msg -> Msg
rpcExceptionMsg (RPCException sys req ex) =
  "remote" .= sys ~~ "path" .= HTTP.path req ~~ headers ~~ msg (show ex)
  where
    headers = foldr hdr id (HTTP.requestHeaders req)
    hdr (k, v) x = x ~~ original k .= v

statusCheck ::
  (MonadError e m) =>
  Int ->
  (LText -> e) ->
  Response (Maybe LByteString) ->
  m ()
statusCheck c f r =
  unless (statusCode r == c) $
    throwError $
      f ("unexpected status code: " <> pack (show $ statusCode r))

parseResponse ::
  (Exception e, MonadThrow m, FromJSON a) =>
  (LText -> e) ->
  Response (Maybe LByteString) ->
  m a
parseResponse f r = either throwM pure $ do
  b <- note (f "no response body") (responseBody r)
  fmapL (f . pack) (eitherDecode' b)
