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

-- | Interprets 'BrigAPIAccess' from within brig itself, by calling into the local
-- subsystems directly instead of round-tripping over HTTP to itself (as
-- 'Wire.BrigAPIAccess.Rpc.interpretBrigAccess' does for every other service).
--
-- Only the operations needed by code shared with other services (e.g.
-- 'Wire.TeamCollaboratorsSubsystem') are implemented locally.  Everything else
-- falls back to the RPC handler, pointed at brig itself: correct, but a wasteful
-- round-trip through our own listen socket, so it logs a warning and should be
-- given a local implementation once something actually relies on it.
module Wire.BrigAPIAccess.Local where

import Imports
import Polysemy
import Polysemy.Error (Error)
import Polysemy.Input (runInputConst)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Util.Options (Endpoint)
import Wire.BrigAPIAccess
import Wire.BrigAPIAccess.Rpc (brigAccessRpcHandler)
import Wire.ParseException (ParseException)
import Wire.Rpc (Rpc)
import Wire.RpcException (RpcException)
import Wire.UserSubsystem (UserSubsystem)
import Wire.UserSubsystem qualified as UserSubsystem

-- | The 'Endpoint' is brig's own; it is only used for the operations that have
-- no local implementation yet.
interpretBrigAPIAccessLocally ::
  forall r.
  ( Member TinyLog r,
    Member Rpc r,
    Member (Error ParseException) r,
    Member (Error RpcException) r
  ) =>
  Endpoint ->
  InterpreterFor UserSubsystem r ->
  InterpreterFor BrigAPIAccess r
interpretBrigAPIAccessLocally selfEndpoint runUser = interpret $ \case
  UpdateSearchIndex uid -> runUser (UserSubsystem.internalUpdateSearchIndex uid)
  other -> selfRpc other
  where
    selfRpc :: forall m x. BrigAPIAccess m x -> Sem r x
    selfRpc action = do
      Log.warn $
        Log.msg (Log.val "BrigAPIAccess.Local: no local implementation, calling brig over HTTP")
      runInputConst selfEndpoint (brigAccessRpcHandler action)
