-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

-- | Interprets 'BrigAPIAccess' from within brig itself, by calling into the local
-- subsystems directly instead of round-tripping over HTTP to itself (as
-- 'Wire.BrigAPIAccess.Rpc.interpretBrigAccess' does for every other service).
--
-- Only the operations actually needed by code shared with other services (e.g.
-- 'Wire.TeamCollaboratorsSubsystem') are implemented; everything else is
-- unimplemented until brig itself needs it.
module Wire.BrigAPIAccess.Local where

import Imports
import Polysemy
import Wire.BrigAPIAccess
import Wire.UserSubsystem (UserSubsystem)
import Wire.UserSubsystem qualified as UserSubsystem

interpretBrigAPIAccessLocally ::
  InterpreterFor UserSubsystem r ->
  InterpreterFor BrigAPIAccess r
interpretBrigAPIAccessLocally runUser = interpret $ \case
  UpdateSearchIndex uid -> runUser (UserSubsystem.internalUpdateSearchIndex uid)
  _ -> error "BrigAPIAccess.Local: operation not implemented" -- TODO: shouldn't we make an effort and at least fall back to the Rpc interpreter somehow?
