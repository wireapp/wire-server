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

module Galley.Intra.Team where

import Bilge
import Bilge.RPC
import Brig.Types.Team
import Data.ByteString.Conversion
import Data.Id
import Galley.Intra.Util
import Galley.Monad
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

getSize :: TeamId -> App TeamSize
getSize tid = do
  r <-
    call Brig $
      method GET
        . paths ["/i/teams", toByteString' tid, "size"]
        . expect2xx
  parseResponse (mkError status502 "server-error") r
