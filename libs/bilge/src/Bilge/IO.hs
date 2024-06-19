{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

module Bilge.IO
  ( -- * Convenience API
    HttpT (..),
    Http,
    MonadHttp (..),
    handleRequest,
    Debug (..),
    runHttpT,
    http,
    httpLbs,
    httpDebug,
    get,
    get',
    put,
    put',
    post,
    post',
    head,
    head',
    delete,
    delete',
    options,
    options',
    trace,
    trace',
    patch,
    patch',
    consumeBody,

    -- * Re-exports
    ManagerSettings (..),
    withResponse,
    Manager,
    newManager,
    withManager,
    defaultManagerSettings,
    BodyReader,
    brRead,
    brConsume,
    HttpException (..),
  )
where

-- It's impossible to create a Response body without using internals :'(

import Bilge.Request
import Bilge.Response
import Bilge.TestSession
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (CI)
import Imports hiding (head)
import Network.HTTP.Client as Client hiding (httpLbs, method)
import Network.HTTP.Client qualified as Client (method)
import Network.HTTP.Client.Internal qualified as Client (Response (..), ResponseClose (..))
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest

-- | Debug settings may cause debug information to be printed to stdout.
data Debug
  = -- | Print HTTP request/response header.
    Head
  | -- | Like 'Head' but also print the response body.
    Full
  deriving (Eq, Ord, Show, Enum)

type Http = HttpT IO

newtype HttpT m a = HttpT
  { unwrap :: ReaderT Manager m a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadTrans,
      MonadReader Manager,
      MonadFail
    )

class MonadHttp m where
  handleRequestWithCont :: Request -> (Response BodyReader -> IO a) -> m a
  {-# MINIMAL handleRequestWithCont #-}

handleRequest :: (MonadHttp m) => Request -> m (Response (Maybe LByteString))
handleRequest req = handleRequestWithCont req consumeBody

instance (MonadIO m) => MonadHttp (HttpT m) where
  handleRequestWithCont :: Request -> (Response BodyReader -> IO a) -> HttpT m a
  handleRequestWithCont req h = do
    m <- ask
    liftIO $ withResponse req m h

-- | Returns the entire ByteString immediately on first read
-- then empty ByteString on all subsequent reads.
-- This is used for back-compatability on MonadHttp so that we can write an instance for
-- MonadHttp of Wai.Session while maintaining compatability with the previous interface.
trivialBodyReader :: ByteString -> IO BodyReader
trivialBodyReader bodyBytes = do
  bodyVar <- newTVarIO bodyBytes
  pure $ mkBodyReader bodyVar
  where
    mkBodyReader :: TVar ByteString -> BodyReader
    mkBodyReader bodyVar = do
      atomically $ swapTVar bodyVar ""

instance MonadHttp WaiTest.Session where
  handleRequestWithCont req cont = unSessionT $ handleRequestWithCont req cont

instance (MonadIO m) => MonadHttp (SessionT m) where
  handleRequestWithCont req cont = do
    reqBody <- liftIO $ getHttpClientRequestBody (Client.requestBody req)
    -- `srequest` sets the requestBody for us
    wResponse :: WaiTest.SResponse <- liftSession $ WaiTest.srequest (WaiTest.SRequest wRequest reqBody)
    bodyReader <- liftIO $ trivialBodyReader $ LBS.toStrict $ WaiTest.simpleBody wResponse
    let bilgeResponse :: Response BodyReader
        bilgeResponse = toBilgeResponse bodyReader wResponse req

    liftIO $ cont bilgeResponse
    where
      wRequest :: Wai.Request
      wRequest =
        flip WaiTest.setPath (Client.path req <> Client.queryString req) $
          Wai.defaultRequest
            { Wai.requestMethod = Client.method req,
              Wai.httpVersion = Client.requestVersion req,
              Wai.requestHeaders = Client.requestHeaders req,
              Wai.isSecure = Client.secure req,
              Wai.requestHeaderHost = lookupHeader "HOST" req,
              Wai.requestHeaderRange = lookupHeader "RANGE" req,
              Wai.requestHeaderReferer = lookupHeader "REFERER" req,
              Wai.requestHeaderUserAgent = lookupHeader "USER-AGENT" req
            }
      toBilgeResponse :: BodyReader -> WaiTest.SResponse -> Client.Request -> Response BodyReader
      toBilgeResponse bodyReader WaiTest.SResponse {WaiTest.simpleStatus, WaiTest.simpleHeaders} originalReq =
        Client.Response
          { responseStatus = simpleStatus,
            -- I just picked an arbitrary version; shouldn't matter.
            responseVersion = http11,
            responseHeaders = simpleHeaders,
            responseBody = bodyReader,
            responseOriginalRequest = originalReq,
            responseEarlyHints = [],
            Client.responseCookieJar = mempty,
            Client.responseClose' = Client.ResponseClose $ pure ()
          }
      lookupHeader :: CI ByteString -> Client.Request -> Maybe ByteString
      lookupHeader headerName r = lookup headerName (Client.requestHeaders r)

-- | Does not support all constructors, but so far we only use 'RequestBodyLBS'.
-- The other ones are slightly less straight-forward, so we can implement them later if needed.
getHttpClientRequestBody :: (HasCallStack) => Client.RequestBody -> IO LByteString
getHttpClientRequestBody = \case
  Client.RequestBodyLBS lbs -> pure lbs
  Client.RequestBodyBS bs -> pure (LBS.fromStrict bs)
  Client.RequestBodyBuilder _ _ -> notImplemented "RequestBodyBuilder"
  Client.RequestBodyStream _ _ -> notImplemented "RequestBodyStream"
  Client.RequestBodyStreamChunked _ -> notImplemented "RequestBodyStreamChunked"
  Client.RequestBodyIO _ -> notImplemented "RequestBodyIO"
  where
    notImplemented x = error ("getHttpClientRequestBody: not implemented: " <> x)

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadHttp m, Monad m) => MonadHttp (t m) where
  handleRequestWithCont req cont = lift $ handleRequestWithCont req cont

instance MonadBase IO (HttpT IO) where
  liftBase = liftIO

instance MonadTransControl HttpT where
  type StT HttpT a = StT (ReaderT Manager) a
  liftWith = defaultLiftWith HttpT unwrap
  restoreT = defaultRestoreT HttpT

instance MonadBaseControl IO (HttpT IO) where
  type StM (HttpT IO) a = ComposeSt HttpT IO a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance (MonadUnliftIO m) => MonadUnliftIO (HttpT m) where
  withRunInIO inner =
    HttpT . ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runHttpT r)

runHttpT :: Manager -> HttpT m a -> m a
runHttpT m h = runReaderT (unwrap h) m

-- | Given a 'Request' builder function, perform an actual HTTP request using the
-- respective method and return the response, fully consuming the response body
-- as a lazy 'ByteString'.
get,
  post,
  put,
  head,
  delete,
  options,
  trace,
  patch ::
    (MonadHttp m) =>
    (Request -> Request) ->
    m (Response (Maybe LByteString))
get f = httpLbs empty (method GET . f)
post f = httpLbs empty (method POST . f)
put f = httpLbs empty (method PUT . f)
head f = httpLbs empty (method HEAD . f)
delete f = httpLbs empty (method DELETE . f)
options f = httpLbs empty (method OPTIONS . f)
trace f = httpLbs empty (method TRACE . f)
patch f = httpLbs empty (method PATCH . f)

get',
  post',
  put',
  head',
  delete',
  options',
  trace',
  patch' ::
    (MonadHttp m) =>
    Request ->
    (Request -> Request) ->
    m (Response (Maybe LByteString))
get' r f = httpLbs r (method GET . f)
post' r f = httpLbs r (method POST . f)
put' r f = httpLbs r (method PUT . f)
head' r f = httpLbs r (method HEAD . f)
delete' r f = httpLbs r (method DELETE . f)
options' r f = httpLbs r (method OPTIONS . f)
trace' r f = httpLbs r (method TRACE . f)
patch' r f = httpLbs r (method PATCH . f)

httpLbs ::
  (MonadHttp m) =>
  Request ->
  (Request -> Request) ->
  m (Response (Maybe LByteString))
httpLbs r f = http r f consumeBody

http ::
  (MonadHttp m) =>
  Request ->
  (Request -> Request) ->
  (Response BodyReader -> IO a) ->
  m a
http r f = handleRequestWithCont (f r)

httpDebug ::
  (MonadIO m, MonadHttp m) =>
  Debug ->
  Request ->
  (Request -> Request) ->
  (Response (Maybe LByteString) -> IO a) ->
  m a
httpDebug debug r f h = do
  let rq = f r
  if debug > Head
    then putStrLn (showRequest rq)
    else putStrLn (showRequest (rq {requestBody = RequestBodyLBS ""}))
  putStrLn "-"
  handleRequestWithCont rq $
    consumeBody >=> \rsp -> do
      if debug > Head
        then putStrLn (showResponse rsp)
        else putStrLn (showResponse $ rsp {responseBody = "" :: String})
      putStrLn "--"
      h rsp

consumeBody :: Response BodyReader -> IO (Response (Maybe LBS.ByteString))
consumeBody r = do
  chunks <- brConsume (responseBody r)
  let bdy =
        if null chunks
          then Nothing
          else Just (LBS.fromChunks chunks)
  pure $ r {responseBody = bdy}
