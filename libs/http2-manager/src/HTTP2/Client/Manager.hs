module HTTP2.Client.Manager
  ( Http2Manager,
    setCacheLimit,
    setSSLContext,
    TLSEnabled,
    HostName,
    Port,
    Target,
    defaultHttp2Manager,
    http2ManagerWithSSLCtx,
    withHTTP2Request,
    connectIfNotAlreadyConnected,
    ConnectionAlreadyClosed (..),
    disconnectServer,
    disconnectServerWithTimeout,
  )
where

import HTTP2.Client.Manager.Internal
