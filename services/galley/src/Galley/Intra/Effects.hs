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

module Galley.Intra.Effects where

import Galley.Cassandra.Util
import Galley.Effects.SparAccess (SparAccess (..))
import Galley.Env
import Galley.Intra.Spar
import Galley.Monad
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog

interpretSparAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r,
    Member TinyLog r
  ) =>
  Sem (SparAccess ': r) a ->
  Sem r a
interpretSparAccess = interpret $ \case
  DeleteTeam tid -> do
    logEffect "SparAccess.DeleteTeam"
    embedApp $ deleteTeam tid
  LookupScimUserInfo uid -> do
    logEffect "SparAccess.LookupScimUserInfo"
    embedApp $ lookupScimUserInfo uid
