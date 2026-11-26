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

module Wire.CargoholdAPIAccess.Rpc (interpretCargoholdAPIAccessToRpc) where

import Bilge hiding (head, options, requestId)
import Imports
import Network.HTTP.Types.Method
import Polysemy
import Polysemy.Error
import Util.Options
import Wire.CargoholdAPIAccess
import Wire.ParseException
import Wire.Rpc

interpretCargoholdAPIAccessToRpc ::
  ( Member (Error ParseException) r,
    Member Rpc r
  ) =>
  Endpoint ->
  InterpreterFor CargoholdAPIAccess r
interpretCargoholdAPIAccessToRpc endpoint = interpret $ \case
  CleanupBackups ->
    rpcWithRetries "cargohold" endpoint req >>= decodeBodyOrThrow "cargohold"
    where
      req =
        method POST
          . paths ["i", "backups", "cleanup"]
          . expect2xx
