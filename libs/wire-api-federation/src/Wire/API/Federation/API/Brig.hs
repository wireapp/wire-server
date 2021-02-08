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

module Wire.API.Federation.API.Brig where

import Data.Handle (Handle)
import Data.Id (UserId)
import Data.Qualified (Qualified)
import Imports
import Servant.API
import Servant.API.Generic

-- Maybe this module should be called Brig
newtype Api routes = Api
  { getUserByHandle ::
      routes
        :- "federation"
        :> "users"
        :> "by-handle"
        :> QueryParam' '[Required, Strict] "handle" Handle
        -- FUTUREWORK: Make this return UserProfile, at that point there would
        -- be interesting questions like whether to expose email or not and how
        -- we code that part. I want to avoid solving this until federator works
        :> Get '[JSON] (Qualified UserId)
  }
  deriving (Generic)
