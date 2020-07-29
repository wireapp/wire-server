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
import Data.Misc (mkHttpsUrl)
import Imports
import qualified URI.ByteString as URI
import qualified URI.ByteString.QQ as URI
import qualified Wire.API.Call.Config as Public
import Wire.Network.DNS.SRV (SrvTarget (..))

sftServerFromSrvTarget :: SrvTarget -> Public.SFTServer
sftServerFromSrvTarget (SrvTarget host port) =
  let uriPort = URI.Port (fromIntegral port)
      uri =
        [URI.uri|https://|]
          & URI.authorityL ?~ URI.Authority Nothing (URI.Host (dropTrailingDot host)) (Just uriPort)
   in either (error "sftServerFromSrvTarget: invalid https URI") Public.sftServer (mkHttpsUrl uri)
  where
    dropTrailingDot :: ByteString -> ByteString
    dropTrailingDot bs =
      if BS.last bs == '.'
        then BS.init bs
        else bs
