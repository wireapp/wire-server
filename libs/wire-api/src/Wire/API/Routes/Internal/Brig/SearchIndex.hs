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

import Data.Id (UserId)
import Servant (JSON)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Routes.Named (Named)

type ISearchIndexAPI =
  Named
    "indexRefresh"
    ( Summary "make index updates visible (e.g. for integration testing)"
        :> "index"
        :> "refresh"
        :> Post '[JSON] NoContent
    )
    :<|> Named
           "update-search-index"
           ( Summary "updates the search index for a single user"
               :> "index"
               :> "update"
               :> Capture "userId" UserId
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "bump-write-time-and-update-search-index"
           ( Summary "updates the search index for a single user, forcing the document version to advance"
               :> Description
                    "Use this instead of `update-search-index` when the change that needs to be \
                    \indexed does not live in the user record itself (currently: team \
                    \collaborations).  The index version is derived from the user record, so \
                    \without bumping it the updated document would be rejected as a version \
                    \conflict."
               :> "index"
               :> "update"
               :> Capture "userId" UserId
               :> "bump-write-time"
               :> Post '[JSON] NoContent
           )
