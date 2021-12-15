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

module CargoHold.API.Public
  ( sitemap,
    apiDocs,
  )
where

import CargoHold.App
import Data.Predicate
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Encoding (decodeLatin1)
import Imports hiding (head)
import Network.Wai.Predicate hiding (Error, setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities hiding (message)
import Network.Wai.Utilities.Swagger (mkSwaggerApi)

-- FUTUREWORK: restore (and servantify) resumable upload functionality, removed
-- in https://github.com/wireapp/wire-server/pull/1998

--------------------------------------------------------------------------------
-- Wai routes

sitemap :: Routes Doc.ApiBuilder Handler ()
sitemap = pure ()

apiDocs :: Routes Doc.ApiBuilder Handler ()
apiDocs = do
  get
    "/assets/api-docs"
    ( \(_ ::: url) k ->
        let doc = mkSwaggerApi (decodeLatin1 url) [] sitemap
         in k $ json doc
    )
    $ accept "application" "json"
      .&. query "base_url"
