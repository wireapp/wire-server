-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH
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

module Wire.TeamEventQueueAccess.Aws
  ( interpretTeamEventQueueAccess,
  )
where

import Imports
import Polysemy
import Wire.AWS qualified as WA
import Wire.TeamEventQueueAccess (TeamEventQueueAccess (..))

interpretTeamEventQueueAccess ::
  (Member (Embed IO) r) =>
  Maybe WA.Env ->
  Sem (TeamEventQueueAccess ': r) a ->
  Sem r a
interpretTeamEventQueueAccess mEnv = interpret $ \case
  EnqueueTeamEvent ev -> case mEnv of
    Nothing -> pure ()
    Just e -> embed $ WA.execute e (WA.enqueue ev)
