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

module Brig.API.Connection.Util
  ( ConnectionM,
    checkLimit,
  )
where

import Brig.API.Types
import Brig.App
import Brig.Data.Connection qualified as Data
import Brig.Options (Settings (setUserMaxConnections))
import Control.Error (noteT)
import Control.Lens (view)
import Control.Monad.Trans.Except
import Data.Id (UserId)
import Data.Qualified (Local, tUnqualified)
import Imports
import Wire.API.Connection (Relation (..))

type ConnectionM r = ExceptT ConnectionError (AppT r)

-- Helpers

checkLimit :: Local UserId -> ExceptT ConnectionError (AppT r) ()
checkLimit u = noteT (TooManyConnections (tUnqualified u)) $ do
  n <- lift . wrapClient $ Data.countConnections u [Accepted, Sent]
  l <- setUserMaxConnections <$> view settings
  guard (n < l)
