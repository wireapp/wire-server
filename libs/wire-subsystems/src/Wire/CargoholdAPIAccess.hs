{-# LANGUAGE TemplateHaskell #-}

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

module Wire.CargoholdAPIAccess where

import Bilge
import Data.ByteString.Conversion
import Data.Id
import Imports
import Network.HTTP.Types
import Polysemy
import Util.Options
import Wire.Rpc

data CargoholdAPIAccess m a where
  DeleteBackupsOfClient :: UserId -> ClientId -> CargoholdAPIAccess m ()
  DeleteBackupsOfUser :: UserId -> CargoholdAPIAccess m ()

makeSem ''CargoholdAPIAccess

runCargoholdAPIAccess :: (Member Rpc r) => Endpoint -> InterpreterFor CargoholdAPIAccess r
runCargoholdAPIAccess ep = interpret $ \case
  DeleteBackupsOfClient uid cid ->
    void . rpcWithRetries "cargohold" ep $
      method DELETE
        . paths ["i", "backups", "users", toByteString' uid, "clients", toByteString' cid]
        . expect2xx
  DeleteBackupsOfUser uid ->
    void . rpcWithRetries "cargohold" ep $
      method DELETE
        . paths ["i", "backups", "users", toByteString' uid]
        . expect2xx
