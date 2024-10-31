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

module Brig.Calling.Internal where

import Control.Lens ((?~))
import Data.ByteString.Char8 qualified as BS
import Data.Misc (ensureHttpsUrl)
import Data.Text qualified as T
import Imports
import URI.ByteString qualified as URI
import URI.ByteString.QQ qualified as URI
import Wire.API.Call.Config qualified as Public
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

base26 :: Integer -> Text
base26 0 = "a"
base26 num = T.pack $ go [] num
  where
    go :: String -> Integer -> String
    go acc 0 = acc
    go acc n =
      let (q, r) = divMod n 26
       in go (chr (fromIntegral r + ord 'a') : acc) q
