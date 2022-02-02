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

module Spar.Sem.GalleyAccess.Http where

import Bilge
import Imports hiding (log)
import Polysemy
import Polysemy.Error
import Spar.Error (SparError)
import qualified Spar.Intra.Galley as Intra
import Spar.Sem.GalleyAccess
import Spar.Sem.Logger (Logger)
import Spar.Sem.Utils
import qualified System.Logger as TinyLog

galleyAccessToHttp ::
  Members '[Logger (TinyLog.Msg -> TinyLog.Msg), Error SparError, Embed IO] r =>
  Bilge.Manager ->
  Bilge.Request ->
  Sem (GalleyAccess ': r) a ->
  Sem r a
galleyAccessToHttp mgr req =
  interpret $
    viaRunHttp (RunHttpEnv mgr req) . \case
      GetTeamMembers itlt -> Intra.getTeamMembers itlt
      AssertHasPermission itlt perm itlu -> Intra.assertHasPermission itlt perm itlu
      AssertSSOEnabled itlt -> Intra.assertSSOEnabled itlt
      IsEmailValidationEnabledTeam itlt -> Intra.isEmailValidationEnabledTeam itlt
