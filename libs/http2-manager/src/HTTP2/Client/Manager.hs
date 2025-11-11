-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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
