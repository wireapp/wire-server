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

module Wire.MockInterpreters.WebPushStore where

import Control.Lens ((^.))
import Data.Id
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Push.V2.WebSubscription
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.WebPushStore

-- | Key identifying a single subscription row. Mirrors the Postgres primary
-- key @(user_id, client_id, endpoint)@ so upsert semantics match.
type WebPushKey = (UserId, ClientId, EndpointUrl)

-- | In-memory store value: the connection id alongside the full subscription
-- (which carries expiration, needed by 'purgeExpired').
data StoredWebPushSubscription = StoredWebPushSubscription
  { swpsConn :: !ConnId,
    swpsSubscription :: !WebPushSubscription
  }

type WebPushStoreState = Map WebPushKey StoredWebPushSubscription

inMemoryWebPushStoreInterpreter ::
  forall r.
  (Member (State WebPushStoreState) r, Member Now r) =>
  InterpreterFor WebPushStore r
inMemoryWebPushStoreInterpreter = interpret $ \case
  InsertSubscription uid sub conn ->
    modify $
      Map.insert
        (uid, sub ^. wpsClient, sub ^. wpsEndpoint)
        StoredWebPushSubscription {swpsConn = conn, swpsSubscription = sub}
  LookupSubscriptions uid ->
    gets $
      map (toAddress uid)
        . Map.elems
        . Map.filterWithKey (\(u, _, _) _ -> u == uid)
  DeleteSubscription uid endpoint ->
    modify $ Map.filterWithKey \(u, _c, ep) _ -> not (u == uid && ep == endpoint)
  DeleteAllForUser uid ->
    modify $ Map.filterWithKey \(u, _, _) _ -> u /= uid
  PurgeExpired uid -> do
    now <- Now.get
    let nowMs = round (utcTimeToPOSIXSeconds now * 1000) :: Word64
    modify $
      Map.filterWithKey
        ( \(u, _, _) swps ->
            u /= uid
              || case swps.swpsSubscription ^. wpsExpirationTime of
                Nothing -> True
                Just expMs -> expMs >= nowMs
        )

toAddress :: UserId -> StoredWebPushSubscription -> WebPushAddress
toAddress uid swps =
  WebPushAddress
    { wpaUser = uid,
      wpaConn = swps.swpsConn,
      wpaClient = swps.swpsSubscription ^. wpsClient,
      wpaEndpoint = swps.swpsSubscription ^. wpsEndpoint,
      wpaKeys = swps.swpsSubscription ^. wpsKeys
    }
