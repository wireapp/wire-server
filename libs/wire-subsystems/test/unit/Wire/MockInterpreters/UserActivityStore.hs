-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.UserActivityStore where

import Data.Id
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Imports
import Polysemy
import Polysemy.State
import Wire.UserActivityStore

inMemoryUserActivityStoreInterpreter ::
  (Member (State (Map UserId UTCTime)) r) =>
  InterpreterFor UserActivityStore r
inMemoryUserActivityStoreInterpreter = interpret $ \case
  GetLastActivity uid -> gets (Map.lookup uid)
  UpdateLastActivity uid t -> modify (Map.insert uid t)
  DeleteLastActivity uid -> modify (Map.delete uid)

noOpUserActivityStoreInterpreter :: InterpreterFor UserActivityStore r
noOpUserActivityStoreInterpreter = interpret $ \case
  GetLastActivity _ -> pure Nothing
  UpdateLastActivity _ _ -> pure ()
  DeleteLastActivity _ -> pure ()
