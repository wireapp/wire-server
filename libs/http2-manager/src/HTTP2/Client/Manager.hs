module HTTP2.Client.Manager
  ( HTTP2Manager,
    setCacheLimit,
    setSSLContext,
    TLSEnabled,
    HostName,
    Port,
    Target,
    defaultHTTP2Manager,
    http2ManagerWithSSLCtx,
    withHTTP2Request,
    ConnectionAlreadyClosed (..),
    disconnectServer,
    disconnectServerWithTimeout,
  )
where

import HTTP2.Client.Manager.Internal
