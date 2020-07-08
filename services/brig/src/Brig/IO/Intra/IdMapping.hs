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

module Brig.IO.Intra.IdMapping
  ( createIdMappingInGalley,
  )
where

import Bilge (json, paths, statusCode)
import Brig.App (AppIO)
import Brig.RPC (expect, galleyRequest, remote)
import Data.Qualified (renderQualifiedId)
import Galley.Types.IdMapping (PostIdMappingRequest (reqQualifiedId))
import Imports
import Network.HTTP.Types (StdMethod (POST), status200, status403)
import qualified System.Logger.Class as Log

-- | Calls 'Brig.API.IdMapping.postIdMappingH'.
createIdMappingInGalley :: PostIdMappingRequest -> AppIO ()
createIdMappingInGalley body = do
  Log.debug $ remote "galley" . Log.msg (Log.val "Create ID mapping")
  st <- statusCode <$> galleyRequest POST req
  when (st /= 403) $ do
    -- don't fail, but make some noise
    Log.err $
      Log.field "qualified_id" (renderQualifiedId (reqQualifiedId body))
        . Log.msg ("Creating ID mapping in Galley, but got " <> show st)
  where
    req =
      paths ["i", "id-mapping"]
        . expect [status200, status403]
        . json body
