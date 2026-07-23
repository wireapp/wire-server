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
import Network.Wai qualified as Wai
import Servant.Client.Core
import Servant.Types.SourceT

streamingResponseToWai :: StreamingResponse -> Wai.Response
streamingResponseToWai resp =
  let -- We re-frame the body ourselves via 'Wai.responseStream' (Warp emits it
      -- chunked), so any framing header from the upstream response must be
      -- dropped.  Passing a stale 'Content-Length' or 'Transfer-Encoding'
      -- through would double-frame the body: the peer reads the advertised
      -- length, treats the response as complete, and leaves the remaining bytes
      -- in the socket buffer.  On a reused keep-alive connection the next
      -- request then reads those leftover bytes as its own response, which
      -- surfaces as an unrelated 200 with a non-JSON body (e.g. a buffered
      -- /i/metrics scrape) and manifests as flaky federation calls.
      isFramingHeader (name, _) = name == "Content-Length" || name == "Transfer-Encoding"
      headers = filter (not . isFramingHeader) (toList (responseHeaders resp))
      status = responseStatusCode resp
      streamingBody output flush =
        foreach
          (const (pure ()))
          (\chunk -> output (byteString chunk) *> flush)
          (responseBody resp)
   in Wai.responseStream status headers streamingBody
