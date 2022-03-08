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

module Spar.Sem.BrigAccess.Http
  ( brigAccessToHttp,
  )
where

import Bilge
import Imports
import Polysemy
import Polysemy.Error (Error)
import Spar.Error (SparError)
import qualified Spar.Intra.Brig as Intra
import Spar.Sem.BrigAccess
import Spar.Sem.Logger (Logger)
import Spar.Sem.Utils (RunHttpEnv (..), viaRunHttp)
import qualified System.Logger as TinyLog

brigAccessToHttp ::
  Members '[Logger (TinyLog.Msg -> TinyLog.Msg), Error SparError, Embed IO] r =>
  Bilge.Manager ->
  Bilge.Request ->
  Sem (BrigAccess ': r) a ->
  Sem r a
brigAccessToHttp mgr req =
  interpret $
    viaRunHttp (RunHttpEnv mgr req) . \case
      CreateSAML u itlu itlt n m -> Intra.createBrigUserSAML u itlu itlt n m
      CreateNoSAML e itlt n -> Intra.createBrigUserNoSAML e itlt n
      UpdateEmail itlu e -> Intra.updateEmail itlu e
      GetAccount h itlu -> Intra.getBrigUserAccount h itlu
      GetByHandle h -> Intra.getBrigUserByHandle h
      GetByEmail e -> Intra.getBrigUserByEmail e
      SetName itlu n -> Intra.setBrigUserName itlu n
      SetHandle itlu h -> Intra.setBrigUserHandle itlu h
      SetManagedBy itlu m -> Intra.setBrigUserManagedBy itlu m
      SetVeid itlu v -> Intra.setBrigUserVeid itlu v
      SetRichInfo itlu r -> Intra.setBrigUserRichInfo itlu r
      GetRichInfo itlu -> Intra.getBrigUserRichInfo itlu
      CheckHandleAvailable h -> Intra.checkHandleAvailable h
      Delete itlu -> Intra.deleteBrigUser itlu
      EnsureReAuthorised mitlu mp mc ma -> Intra.ensureReAuthorised mitlu mp mc ma
      SsoLogin itlu -> Intra.ssoLogin itlu
      GetStatus itlu -> Intra.getStatus itlu
      GetStatusMaybe itlu -> Intra.getStatusMaybe itlu
      SetStatus itlu a -> Intra.setStatus itlu a
