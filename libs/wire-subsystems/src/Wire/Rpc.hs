{-# LANGUAGE TemplateHaskell #-}

module Wire.Rpc
  ( Rpc,
    rpc,
    rpcWithRetries,
    runRpcWithHttp,
    x3,
    zUser,
    expect,
  )
where

import Bilge
import Bilge.RPC hiding (rpc)
import Bilge.Retry
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Conversion
import Data.Id
import Data.Text.Encoding (encodeUtf8)
import Imports
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Polysemy
import Util.Options

-- * Effect

type ServiceName = LText

data Rpc m a where
  Rpc :: ServiceName -> Endpoint -> (Request -> Request) -> Rpc m (Response (Maybe LByteString))
  RpcWithRetries :: ServiceName -> Endpoint -> (Request -> Request) -> Rpc m (Response (Maybe LByteString))

makeSem ''Rpc

runRpcWithHttp :: (Member (Embed IO) r) => Manager -> RequestId -> Sem (Rpc : r) a -> Sem r a
runRpcWithHttp mgr reqId = interpret $ \case
  Rpc serviceName ep req ->
    embed $ runHttpRpc mgr reqId $ rpcImpl serviceName ep req
  RpcWithRetries serviceName ep req ->
    embed $ runHttpRpc mgr reqId $ rpcWithRetriesImpl serviceName ep req

rpcImpl :: ServiceName -> Endpoint -> (Request -> Request) -> HttpRpc (Response (Maybe LByteString))
rpcImpl serviceName ep req = do
  rpc' serviceName empty $
    req
      . Bilge.host (encodeUtf8 ep.host)
      . Bilge.port ep.port

rpcWithRetriesImpl :: ServiceName -> Endpoint -> (Request -> Request) -> HttpRpc (Response (Maybe LByteString))
rpcWithRetriesImpl serviceName ep req =
  recovering x3 rpcHandlers $
    const $
      rpcImpl serviceName ep req

-- * Helpers

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

zUser :: UserId -> Request -> Request
zUser uid = header "Z-User" (toByteString' uid)

expect :: [Status] -> Request -> Request
expect ss rq = rq {HTTP.checkResponse = check}
  where
    check rq' rs = do
      let s = responseStatus rs
          rs' = rs {responseBody = ()}
      when (statusIsServerError s || s `notElem` ss) $
        throwM $
          HttpExceptionRequest rq' (HTTP.StatusCodeException rs' mempty)

-- * Internals

newtype HttpRpc a = HttpRpc {unHttpRpc :: ReaderT (Manager, RequestId) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader (Manager, RequestId)
    )

instance MonadHttp HttpRpc where
  handleRequestWithCont :: Request -> (Response BodyReader -> IO a) -> HttpRpc a
  handleRequestWithCont req responseConsumer = do
    mgr <- asks fst
    runHttpT mgr $ handleRequestWithCont req responseConsumer

instance HasRequestId HttpRpc where
  getRequestId = asks snd

runHttpRpc :: Manager -> RequestId -> HttpRpc a -> IO a
runHttpRpc mgr reqId =
  flip runReaderT (mgr, reqId) . unHttpRpc
