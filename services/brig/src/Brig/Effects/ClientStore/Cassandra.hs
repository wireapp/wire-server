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

module Brig.Effects.ClientStore.Cassandra
  ( clientStoreToCassandra,
    -- FUTUREWORK: Do not export the names below once the module Brig.Data.Client has been
    -- polysemised.
    lookupClients,
    rmClient,
    toClient,
    key,
  )
where

import qualified Amazonka.DynamoDB as AWS
import qualified Amazonka.DynamoDB.DeleteItem as AWS
import Brig.AWS hiding (Env)
import Brig.App
import Brig.Data.Instances ()
import qualified Brig.Effects.ClientStore as E
import Brig.User.Auth.DB.Instances ()
import Cassandra hiding (Client)
import qualified Cassandra as C
import Control.Lens (view, (.~))
import Control.Monad.Catch
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import Imports
import Polysemy
import Wire.API.MLS.Credential
import Wire.API.User.Auth
import Wire.API.User.Client

clientStoreToCassandra ::
  forall m r a.
  ( MonadCatch m,
    MonadClient m,
    MonadReader Env m,
    Member (Embed m) r
  ) =>
  Sem (E.ClientStore ': r) a ->
  Sem r a
clientStoreToCassandra =
  interpret $
    embed @m . \case
      E.LookupClients uid -> lookupClients uid
      E.DeleteClient uid cid -> rmClient uid cid

lookupClients :: MonadClient m => UserId -> m [Client]
lookupClients u = do
  keys <-
    (\(cid, ss, Blob b) -> (cid, [(ss, LBS.toStrict b)]))
      <$$> retry x1 (query selectMLSPublicKeysByUser (params LocalQuorum (Identity u)))
  let keyMap = Map.fromListWith (<>) keys
      updateKeys c =
        c
          { clientMLSPublicKeys =
              Map.fromList $ Map.findWithDefault [] (clientId c) keyMap
          }
  updateKeys . toClient []
    <$$> retry x1 (query selectClients (params LocalQuorum (Identity u)))

selectMLSPublicKeysByUser :: PrepQuery R (Identity UserId) (ClientId, SignatureSchemeTag, Blob)
selectMLSPublicKeysByUser = "SELECT client, sig_scheme, key from mls_public_keys where user = ?"

selectClients :: PrepQuery R (Identity UserId) (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text, Maybe (C.Set ClientCapability))
selectClients = "SELECT client, type, tstamp, label, class, cookie, lat, lon, model, capabilities from clients where user = ?"

rmClient ::
  ( MonadClient m,
    MonadReader Env m,
    MonadCatch m
  ) =>
  UserId ->
  ClientId ->
  m ()
rmClient u c = do
  retry x5 $ write removeClient (params LocalQuorum (u, c))
  retry x5 $ write removeClientKeys (params LocalQuorum (u, c))
  unlessM (isJust <$> view randomPrekeyLocalLock) $ deleteOptLock u c

removeClient :: PrepQuery W (UserId, ClientId) ()
removeClient = "DELETE FROM clients where user = ? and client = ?"

removeClientKeys :: PrepQuery W (UserId, ClientId) ()
removeClientKeys = "DELETE FROM prekeys where user = ? and client = ?"

deleteOptLock ::
  ( MonadReader Env m,
    MonadCatch m,
    MonadIO m
  ) =>
  UserId ->
  ClientId ->
  m ()
deleteOptLock u c = do
  t <- view (awsEnv . prekeyTable)
  e <- view (awsEnv . amazonkaEnv)
  void $ exec e (AWS.newDeleteItem t & AWS.deleteItem_key .~ key u c)

--------------------------------------------------------------------------------
-- Conversions

toClient ::
  [(SignatureSchemeTag, Blob)] ->
  ( ClientId,
    ClientType,
    UTCTimeMillis,
    Maybe Text,
    Maybe ClientClass,
    Maybe CookieLabel,
    Maybe Latitude,
    Maybe Longitude,
    Maybe Text,
    Maybe (C.Set ClientCapability)
  ) ->
  Client
toClient keys (cid, cty, tme, lbl, cls, cok, lat, lon, mdl, cps) =
  Client
    { clientId = cid,
      clientType = cty,
      clientTime = tme,
      clientClass = cls,
      clientLabel = lbl,
      clientCookie = cok,
      clientLocation = location <$> lat <*> lon,
      clientModel = mdl,
      clientCapabilities = ClientCapabilityList $ maybe Set.empty (Set.fromList . C.fromSet) cps,
      clientMLSPublicKeys = fmap (LBS.toStrict . fromBlob) (Map.fromList keys)
    }

key :: UserId -> ClientId -> HashMap Text AWS.AttributeValue
key uid cid = HashMap.singleton ddbClient (ddbKey uid cid)

-------------------------------------------------------------------------------
-- Best-effort optimistic locking for prekeys via DynamoDB

ddbClient :: Text
ddbClient = "client"

ddbKey :: UserId -> ClientId -> AWS.AttributeValue
ddbKey u c = AWS.S (UUID.toText (toUUID u) <> "." <> client c)
