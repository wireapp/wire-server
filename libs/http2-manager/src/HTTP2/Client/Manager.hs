module HTTP2.Client.Manager
  ( HTTP2Manager,
    TLSEnabled,
    defaultHTTP2Manager,
    http2ManagerWithSSLCtx,
    withHTTP2Request,
    ConnectionAlreadyClosed (..),
  )
where

import HTTP2.Client.Manager.Internal
