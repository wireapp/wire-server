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

module Galley.Cassandra.Client
  ( interpretClientStoreToCassandra,
    lookupClients,
  )
where

import Cassandra
import Control.Arrow
import Control.Lens
import Data.Id
import Data.List.Split (chunksOf)
import Galley.Cassandra.Queries qualified as Cql
import Galley.Cassandra.Store
import Galley.Effects.ClientStore (ClientStore (..))
import Galley.Env
import Galley.Monad
import Galley.Options
import Galley.Types.Clients (Clients)
import Galley.Types.Clients qualified as Clients
import Imports
import Polysemy
import Polysemy.Input
import UnliftIO qualified

updateClient :: Bool -> UserId -> ClientId -> Client ()
updateClient add usr cls = do
  let q = if add then Cql.addMemberClient else Cql.rmMemberClient
  retry x5 $ write (q cls) (params LocalQuorum (Identity usr))

-- Do, at most, 16 parallel lookups of up to 128 users each
lookupClients :: [UserId] -> Client Clients
lookupClients users =
  Clients.fromList . concat . concat
    <$> forM (chunksOf 2048 users) (UnliftIO.mapConcurrently getClients . chunksOf 128)
  where
    getClients us =
      map (second fromSet)
        <$> retry x1 (query Cql.selectClients (params LocalQuorum (Identity us)))

eraseClients :: UserId -> Client ()
eraseClients user = retry x5 (write Cql.rmClients (params LocalQuorum (Identity user)))

interpretClientStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member (Input Env) r
  ) =>
  Sem (ClientStore ': r) a ->
  Sem r a
interpretClientStoreToCassandra = interpret $ \case
  GetClients uids -> embedClient $ lookupClients uids
  CreateClient uid cid -> embedClient $ updateClient True uid cid
  DeleteClient uid cid -> embedClient $ updateClient False uid cid
  DeleteClients uid -> embedClient $ eraseClients uid
  UseIntraClientListing -> embedApp . view $ options . settings . intraListing
