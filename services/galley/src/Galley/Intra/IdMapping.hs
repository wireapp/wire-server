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

module Galley.Intra.IdMapping
  ( createIdMappingInBrig,
  )
where

import Bilge (expect2xx, host, json, method, path, port)
import Galley.App (Galley)
import Galley.Intra.Util (brigReq, call)
import Galley.Types.IdMapping (PostIdMappingRequest)
import Imports
import Network.HTTP.Types.Method (StdMethod (POST))

-- | Calls 'Brig.API.IdMapping.postIdMappingH'.
createIdMappingInBrig :: PostIdMappingRequest -> Galley ()
createIdMappingInBrig req = do
  (brigHost, brigPort) <- brigReq
  void . call "brig" $
    method POST
      . host brigHost
      . port brigPort
      . path "/i/id-mapping"
      . json req
      . expect2xx
