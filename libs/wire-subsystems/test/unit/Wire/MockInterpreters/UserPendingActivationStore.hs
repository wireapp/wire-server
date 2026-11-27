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

module Wire.MockInterpreters.UserPendingActivationStore where

import Data.Id
import Data.Map qualified as Map
import Data.Time.Clock
import Imports
import Polysemy
import Polysemy.State
import Wire.UserPendingActivationStore

-- | Pages in 'UserId' order, like the Postgres interpreter.
inMemoryUserPendingActivationStoreInterpreter ::
  (Member (State (Map UserId UTCTime)) r) =>
  InterpreterFor UserPendingActivationStore r
inMemoryUserPendingActivationStoreInterpreter = interpret $ \case
  Add upa -> modify $ Map.insert upa.upaUserId upa.upaDay
  ListPage after limit -> do
    rows <- gets $ Map.toAscList . maybe id (\u -> snd . Map.split u) after
    pure
      ( uncurry UserPendingActivation <$> take (fromIntegral limit) rows,
        length rows > fromIntegral limit
      )
  RemoveMultiple uids -> modify $ \m -> foldl' (flip Map.delete) m uids
