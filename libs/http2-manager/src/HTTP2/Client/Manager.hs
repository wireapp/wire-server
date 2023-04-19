module HTTP2.Client.Manager
  ( Http2Manager,
    setCacheLimit,
    setSSLContext,
    setSSLRemoveTrailingDot,
    TLSEnabled,
    HostName,
    Port,
    Target,
    defaultHttp2Manager,
    http2ManagerWithSSLCtx,
    withHTTP2Request,
    connectIfNotAlreadyConnected,
    ConnectionAlreadyClosed (..),
    disconnectTarget,
    disconnectTargetWithTimeout,
  )
where

import HTTP2.Client.Manager.Internal
