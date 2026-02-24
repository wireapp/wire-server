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

module Wire.UserClientIndexStore.Cassandra (interpretUserClientIndexStoreToCassandra) where

import Cassandra as C
import Control.Arrow
import Control.Lens
import Data.Id
import Data.List.Extra (chunksOf)
import Data.Text.Lazy qualified as LT
import Galley.Types.Clients (Clients)
import Galley.Types.Clients qualified as Clients
import Imports
import Polysemy
import UnliftIO qualified
import Wire.UserClientIndexStore (UserClientIndexStore (..))
import Wire.Util

interpretUserClientIndexStoreToCassandra ::
  (Member (Embed IO) r) =>
  ClientState ->
  Sem (UserClientIndexStore ': r) a ->
  Sem r a
interpretUserClientIndexStoreToCassandra cas = interpret $ \case
  GetClients uids -> do
    embedClient cas $ lookupClients uids
  CreateClient uid cid -> do
    embedClient cas $ updateClient True uid cid
  DeleteClient uid cid -> do
    embedClient cas $ updateClient False uid cid
  DeleteClients uid -> do
    embedClient cas $ eraseClients uid

updateClient :: Bool -> UserId -> ClientId -> Client ()
updateClient add usr cls = do
  let q = if add then upsertMemberAddClient else upsertMemberRmClient
  retry x5 $ write (q cls) (params LocalQuorum (Identity usr))
  where
    upsertMemberAddClient :: ClientId -> QueryString W (Identity UserId) ()
    upsertMemberAddClient c =
      let t = LT.fromStrict (clientToText c)
       in QueryString $ "update clients set clients = clients + {'" <> t <> "'} where user = ?"

    upsertMemberRmClient :: ClientId -> QueryString W (Identity UserId) ()
    upsertMemberRmClient c =
      let t = LT.fromStrict (clientToText c)
       in QueryString $ "update clients set clients = clients - {'" <> t <> "'} where user = ?"

-- Do, at most, 16 parallel lookups of up to 128 users each
lookupClients :: [UserId] -> Client Clients
lookupClients users =
  Clients.fromList . concat . concat
    <$> forM (chunksOf 2048 users) (UnliftIO.mapConcurrently getClients . chunksOf 128)
  where
    getClients us =
      map (second fromSet)
        <$> retry x1 (query selectClients (params LocalQuorum (Identity us)))

    selectClients :: PrepQuery R (Identity [UserId]) (UserId, C.Set ClientId)
    selectClients = "select user, clients from clients where user in ?"

eraseClients :: UserId -> Client ()
eraseClients user = retry x5 (write rmClients (params LocalQuorum (Identity user)))
  where
    rmClients :: PrepQuery W (Identity UserId) ()
    rmClients = "delete from clients where user = ?"
