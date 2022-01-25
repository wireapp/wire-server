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

module Spar.Sem.ScimUserTimesStore
  ( ScimUserTimesStore (..),
    write,
    read,
    delete,
  )
where

import Data.Id (UserId)
import Data.Json.Util (UTCTimeMillis)
import Imports (Maybe)
import Polysemy
import Web.Scim.Schema.Common (WithId)
import Web.Scim.Schema.Meta (WithMeta)

data ScimUserTimesStore m a where
  Write :: WithMeta (WithId UserId t) -> ScimUserTimesStore m ()
  Read :: UserId -> ScimUserTimesStore m (Maybe (UTCTimeMillis, UTCTimeMillis))
  Delete :: UserId -> ScimUserTimesStore m ()

makeSem ''ScimUserTimesStore
