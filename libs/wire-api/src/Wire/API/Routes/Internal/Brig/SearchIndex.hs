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

module Wire.API.Routes.Internal.Brig.SearchIndex where

import Servant (JSON)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Routes.Named (Named (..))

type ISearchIndexAPI =
  Named
    "index-refresh"
    ( Summary "make index updates visible (e.g. for integration testing)"
        :> "index"
        :> "refresh"
        :> Post '[JSON] NoContent
    )
    :<|> Named
           "index-reindex"
           ( Summary
               "reindex from Cassandra (NB: e.g. integration testing prefer the `brig-index` \
               \executable for actual operations!)"
               :> "index"
               :> "reindex"
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "index-reindex-if-same-or-newer"
           ( Summary
               "forcefully reindex from Cassandra, even if nothing has changed (NB: e.g. \
               \integration testing prefer the `brig-index` executable for actual operations!)"
               :> "index"
               :> "reindex-if-same-or-newer"
               :> Post '[JSON] NoContent
           )
