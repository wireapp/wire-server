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

module Wire.FanInDeliveryAccess.Rpc
  ( runFanInDeliveryAccess,
    mutuallyAcceptedConnections,
  )
where

import Bilge
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain)
import Data.Id
import Data.Map.Strict qualified as Map
import Data.Qualified (Qualified (..))
import Data.Set qualified as Set
import Imports
import Network.HTTP.Types (StdMethod (GET))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Util.Options (Endpoint)
import Wire.API.Connection (Relation (Accepted))
import Wire.API.Notification.FanIn (FanInCannonRequest)
import Wire.API.Routes.Internal.Brig.Connection (ConnectionStatusV2 (..))
import Wire.API.User.Client (UserClients (..))
import Wire.BrigAPIAccess.Rpc qualified as Brig
import Wire.FanInDeliveryAccess
import Wire.ParseException
import Wire.Rpc

runFanInDeliveryAccess ::
  (Member Rpc r, Member (Error ParseException) r, Member (Embed IO) r) =>
  Endpoint ->
  Endpoint ->
  Domain ->
  (FanInCannonRequest -> IO ()) ->
  InterpreterFor FanInDeliveryAccess r
runFanInDeliveryAccess brig galley localDomain notifySockets = interpret $ \case
  GetFanInClientIds uid -> do
    UserClients clients <- runInputConst brig (Brig.lookupClients [uid])
    pure (Set.toList (Map.findWithDefault Set.empty uid clients))
  GetFanInTeamIds uid -> do
    response <- rpcWithRetries "galley" galley $ method GET . paths ["i", "users", toByteString' uid, "teams"] . expect2xx
    Brig.decodeBodyOrThrow "galley" response
  GetFanInAcceptedConnections uid -> do
    connections <- runInputConst brig (Brig.getConnections [uid] Nothing (Just Accepted))
    let localAuthors = [author | c <- connections, csv2From c == uid, csv2Status c == Accepted, Qualified author domain <- [csv2To c], domain == localDomain]
    reverseConnections <- runInputConst brig (Brig.getConnections localAuthors (Just [Qualified uid localDomain]) (Just Accepted))
    pure (mutuallyAcceptedConnections localDomain uid connections reverseConnections)
  NotifyFanInSockets request -> embed (notifySockets request)

-- Blocking does not update the other user's relation. Both directions are
-- necessary to prevent a blocked user reading old events from the blocker.
-- Remote connection-profile streams remain closed until reverse authorization
-- is available through federation; a local Accepted row cannot prove it.
mutuallyAcceptedConnections :: Domain -> UserId -> [ConnectionStatusV2] -> [ConnectionStatusV2] -> [Qualified UserId]
mutuallyAcceptedConnections localDomain uid forward reverseConnections =
  Set.toList (Set.intersection candidates authorized)
  where
    candidates = Set.fromList [Qualified author domain | c <- forward, csv2From c == uid, csv2Status c == Accepted, Qualified author domain <- [csv2To c], domain == localDomain]
    authorized = Set.fromList [Qualified (csv2From c) localDomain | c <- reverseConnections, csv2To c == Qualified uid localDomain, csv2Status c == Accepted]
