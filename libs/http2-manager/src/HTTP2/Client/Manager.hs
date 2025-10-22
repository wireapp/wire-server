module HTTP2.Client.Manager
  ( Http2Manager,
    setCacheLimit,
    setSSLContext,
    setSSLRemoveTrailingDot,
    setTCPConnectionTimeout,
    TLSEnabled,
    HostName,
    Port,
    Target,
    defaultHttp2Manager,
    http2ManagerWithSSLCtx,
    withHTTP2Request,
    withHTTP2RequestOnSingleUseConn,
    withHTTP2RequestOnSingleUseConnWithHook,
    connectIfNotAlreadyConnected,
    ConnectionAlreadyClosed (..),
    disconnectTarget,
    disconnectTargetWithTimeout,
    startPersistentHTTP2Connection,
    sendRequestWithConnection,
    HTTP2Conn (..),
    ConnectionAction (..),
  )
where

import HTTP2.Client.Manager.Internal
