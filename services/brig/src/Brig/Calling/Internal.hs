-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Calling.Internal where

import Control.Lens ((?~))
import Imports
import qualified Data.ByteString.Char8 as BS
import Data.Misc (ensureHttpsUrl)
import qualified URI.ByteString as URI
import qualified URI.ByteString.QQ as URI
import qualified Wire.API.Call.Config as Public
import Wire.Network.DNS.SRV (SrvTarget (..))

-- FUTUREWORK: Extract function to translate SrvTarget to HttpsUrl and use it
-- wherever we use DNS for service discovery
sftServerFromSrvTarget :: SrvTarget -> Public.SFTServer
sftServerFromSrvTarget (SrvTarget host port) =
  let uriPort = URI.Port (fromIntegral port)
      uriHost = URI.Host (dropTrailingDot host)
      uri = [URI.uri|https://|] & URI.authorityL ?~ URI.Authority Nothing uriHost (Just uriPort)
   in Public.sftServer (ensureHttpsUrl uri)
  where
    dropTrailingDot :: ByteString -> ByteString
    dropTrailingDot bs =
      if BS.last bs == '.'
        then BS.init bs
        else bs
