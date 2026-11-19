{-# LANGUAGE StrictData #-}


-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Buffered JSON bodies preserve producer-supplied ids across HTTP retries.
module Wire.FanInGundeckAPIAccess.Rpc (runFanInGundeckAPIAccess) where

import Bilge
import Imports
import Network.HTTP.Types (StdMethod (POST, PUT))
import Polysemy
import Util.Options (Endpoint)
import Wire.FanInGundeckAPIAccess
import Wire.Rpc

runFanInGundeckAPIAccess :: (Member Rpc r) => Endpoint -> InterpreterFor FanInGundeckAPIAccess r
runFanInGundeckAPIAccess endpoint = interpret $ \case
  SendFanInNotifications pushes ->
    void $
      rpcWithRetries "gundeck" endpoint $
        method POST . path "/i/fan-in/push" . json pushes . expect2xx
  SendFanInMembershipUpdate update ->
    void $
      rpcWithRetries "gundeck" endpoint $
        method PUT . path "/i/fan-in/membership" . json update . expect2xx
