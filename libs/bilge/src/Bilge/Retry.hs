{-# LANGUAGE ScopedTypeVariables #-}

module Bilge.Retry where

import Bilge.RPC (RPCException (..))
import Control.Monad.Catch
import Imports
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Types

httpHandlers :: Monad m => [a -> Handler m Bool]
httpHandlers = [const . Handler $ return . canRetry]

rpcHandlers :: Monad m => [a -> Handler m Bool]
rpcHandlers =
  [ const . Handler $ \(RPCException _ _ cause) ->
      return $ maybe False canRetry (fromException cause)
  ]

canRetry :: HttpException -> Bool
canRetry (HttpExceptionRequest _ ResponseTimeout) = True
canRetry (HttpExceptionRequest _ ConnectionClosed) = True
canRetry (HttpExceptionRequest _ ConnectionFailure {}) = True
canRetry (HttpExceptionRequest _ InternalException {}) = True
canRetry (HttpExceptionRequest _ ProxyConnectException {}) = True
canRetry (HttpExceptionRequest _ (StatusCodeException rs _)) = statusIsServerError (responseStatus rs)
canRetry _ = False
