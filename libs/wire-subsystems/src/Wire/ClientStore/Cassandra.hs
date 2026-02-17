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

module Wire.ClientStore.Cassandra
  ( interpretClientStoreToCassandra,
    toClient,
  )
where

import Cassandra as C hiding (Client)
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time.Clock
import Imports
import Polysemy
import Polysemy.Embed (runEmbedded)
import Wire.API.MLS.CipherSuite
import Wire.API.User.Auth
import Wire.API.User.Client hiding (UpdateClient (..))
import Wire.ClientStore (ClientStore (..))

interpretClientStoreToCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor ClientStore r
interpretClientStoreToCassandra casClient =
  interpret $
    runEmbedded (runClient @IO casClient) . embed . \case
      Lookup uid cid -> lookupImpl uid cid

lookupImpl :: (MonadClient m) => UserId -> ClientId -> m (Maybe Client)
lookupImpl u c = do
  keys <- retry x1 (query selectMLSPublicKeys (params LocalQuorum (u, c)))
  fmap (toClient keys)
    <$> retry x1 (query1 selectClient (params LocalQuorum (u, c)))

selectClient :: PrepQuery R (UserId, ClientId) (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Text, Maybe (C.Set ClientCapability), Maybe UTCTime)
selectClient = "SELECT client, type, tstamp, label, class, cookie, model, capabilities, last_active from clients where user = ? and client = ?"

selectMLSPublicKeys :: PrepQuery R (UserId, ClientId) (SignatureSchemeTag, Blob)
selectMLSPublicKeys = "SELECT sig_scheme, key from mls_public_keys where user = ? and client = ?"

toClient ::
  [(SignatureSchemeTag, Blob)] ->
  ( ClientId,
    ClientType,
    UTCTimeMillis,
    Maybe Text,
    Maybe ClientClass,
    Maybe CookieLabel,
    Maybe Text,
    Maybe (C.Set ClientCapability),
    Maybe UTCTime
  ) ->
  Client
toClient keys (cid, cty, tme, lbl, cls, cok, mdl, cps, lastActive) =
  Client
    { clientId = cid,
      clientType = cty,
      clientTime = tme,
      clientClass = cls,
      clientLabel = lbl,
      clientCookie = cok,
      clientModel = mdl,
      clientCapabilities = ClientCapabilityList $ maybe Set.empty (Set.fromList . C.fromSet) cps,
      clientMLSPublicKeys = fmap (LBS.toStrict . fromBlob) (Map.fromList keys),
      clientLastActive = lastActive
    }
