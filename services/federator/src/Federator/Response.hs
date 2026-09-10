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

module Federator.Response where

import Data.ByteString.Builder
import Imports
import Network.HTTP.Types.Header (hContentLength, hTransferEncoding)
import Network.Wai qualified as Wai
import Servant.Client.Core
import Servant.Types.SourceT

-- | Turn a streaming upstream response (from the outward federation call) into a
-- WAI response that Warp serves back to the caller.
--
-- We re-frame the body: Warp streams it with chunked transfer-encoding (it does
-- not know the length up front). We must therefore DROP the upstream's own
-- framing headers ('Content-Length', 'Transfer-Encoding'); forwarding them is a
-- keep-alive desync waiting to happen.
--
-- In particular, if we forward a 'Content-Length', Warp honours it verbatim and
-- sends the streamed body raw under that declared length instead of chunking it.
-- The moment the declared length disagrees with the number of bytes we actually
-- stream — a truncated or reset cold-start upstream, a stale @Content-Length@ —
-- the client reads exactly the declared number of bytes and runs straight past
-- the response boundary into the next response on the reused connection. On the
-- integration suite's shared, pooled HTTP/1.1 connection that surfaces as a
-- @POST /rpc/…@ coming back with an unrelated @/i/metrics@ body. Stripping the
-- framing headers lets Warp frame exactly what we stream, so the length on the
-- wire can never disagree with the body and the connection stays in sync.
streamingResponseToWai :: StreamingResponse -> Wai.Response
streamingResponseToWai resp =
  let headers = filter (not . isFramingHeader . fst) (toList (responseHeaders resp))
      status = responseStatusCode resp
      streamingBody output flush =
        foreach
          (const (pure ()))
          (\chunk -> output (byteString chunk) *> flush)
          (responseBody resp)
   in Wai.responseStream status headers streamingBody
  where
    isFramingHeader h = h == hContentLength || h == hTransferEncoding
