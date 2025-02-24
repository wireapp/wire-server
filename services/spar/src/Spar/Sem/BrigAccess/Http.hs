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
import Spar.Sem.Utils (RunHttpEnv (..), viaRunHttp)
import qualified System.Logger as TinyLog
import Wire.Sem.Logger (Logger)

brigAccessToHttp ::
  ( Member (Logger (TinyLog.Msg -> TinyLog.Msg)) r,
    Member (Error SparError) r,
    Member (Embed IO) r
  ) =>
  Bilge.Manager ->
  Bilge.Request ->
  Sem (BrigAccess ': r) a ->
  Sem r a
brigAccessToHttp mgr req =
  interpret $
    viaRunHttp (RunHttpEnv mgr req) . \case
      CreateSAML u itlu itlt n m h ri ml r -> Intra.createBrigUserSAML u itlu itlt n m h ri ml r
      CreateNoSAML eid e uid itlt n ml r -> Intra.createBrigUserNoSAML eid e uid itlt n ml r
      UpdateEmail itlu e -> Intra.updateEmail itlu e
      GetAccount h itlu -> Intra.getBrigUserAccount h itlu
      GetByHandle h -> Intra.getBrigUserByHandle h
      GetByEmail e -> Intra.getBrigUserByEmail e
      SetName itlu n -> Intra.setBrigUserName itlu n
      SetHandle itlu h -> Intra.setBrigUserHandle itlu h
      SetManagedBy itlu m -> Intra.setBrigUserManagedBy itlu m
      SetSSOId itlu v -> Intra.setBrigUserSSOId itlu v
      SetRichInfo itlu r -> Intra.setBrigUserRichInfo itlu r
      SetLocale itlu l -> Intra.setBrigUserLocale itlu l
      GetRichInfo itlu -> Intra.getBrigUserRichInfo itlu
      CheckHandleAvailable h -> Intra.checkHandleAvailable h
      DeleteUser itlu -> Intra.deleteBrigUserInternal itlu
      EnsureReAuthorised mitlu mp mc ma -> Intra.ensureReAuthorised mitlu mp mc ma
      SsoLogin itlu -> Intra.ssoLogin itlu
      GetStatus itlu -> Intra.getStatus itlu
      GetStatusMaybe itlu -> Intra.getStatusMaybe itlu
      SetStatus itlu a -> Intra.setStatus itlu a
      GetDefaultUserLocale -> Intra.getDefaultUserLocale
