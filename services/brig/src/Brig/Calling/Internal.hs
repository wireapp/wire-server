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
import qualified Data.ByteString.Char8 as BS
import Data.Misc (ensureHttpsUrl)
import Imports
import Network.DNS.Types (Domain)
import Network.DNS.Utils (normalize)
import qualified URI.ByteString as URI
import qualified URI.ByteString.QQ as URI
import qualified Wire.API.Call.Config as Public

-- FUTUREWORK: Extract function to translate SrvTarget to HttpsUrl and use it
-- wherever we use DNS for service discovery
sftServerFromSrvTarget :: (Word16, Word16, Word16, Domain) -> Public.SFTServer
sftServerFromSrvTarget (_priority, _weight, port, target) =
  let uriPort = URI.Port (fromIntegral port)
      uriHost = URI.Host . BS.init . normalize $ target
      uri = [URI.uri|https://|] & URI.authorityL ?~ URI.Authority Nothing uriHost (Just uriPort)
   in Public.sftServer (ensureHttpsUrl uri)
