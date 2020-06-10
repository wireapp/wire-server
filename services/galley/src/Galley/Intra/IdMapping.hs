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

import Bilge (expectStatus, host, json, method, path, port, responseStatus)
import Data.Qualified (renderQualifiedId)
import Galley.App (Galley)
import Galley.Intra.Util (brigReq, call)
import Galley.Types.IdMapping (PostIdMappingRequest (reqQualifiedId))
import Imports
import Network.HTTP.Types (StdMethod (POST), statusCode)
import qualified System.Logger.Class as Log

-- | Calls 'Brig.API.IdMapping.postIdMappingH'.
createIdMappingInBrig :: PostIdMappingRequest -> Galley ()
createIdMappingInBrig req = do
  (brigHost, brigPort) <- brigReq
  st <-
    fmap (statusCode . responseStatus) . call "brig" $
      method POST
        . host brigHost
        . port brigPort
        . path "/i/id-mapping"
        . json req
        . expectStatus (`elem` [200, 403])
  when (st == 403) $ do
    -- don't fail, but make some noise
    Log.err $
      Log.field "qualified_id" (renderQualifiedId (reqQualifiedId req))
        . Log.msg @Text "Creating ID mapping in Brig, but federation seems disabled."
