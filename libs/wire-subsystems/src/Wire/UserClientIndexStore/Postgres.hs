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

module Wire.UserClientIndexStore.Postgres
  ( interpretUserClientIndexStoreToPostgres,
  )
where

import Data.Id
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Galley.Types.Clients qualified as Clients
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.User.Client (UserClients (..))
import Wire.Postgres
import Wire.UserClientIndexStore

interpretUserClientIndexStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor UserClientIndexStore r
interpretUserClientIndexStoreToPostgres = interpret $ \case
  GetClients uids
    | null uids -> pure (Clients.fromList [])
    | otherwise -> do
        rows <- V.toList <$> runStatement uids selectClientsStatement
        pure . Clients.fromUserClients . UserClients $
          Map.fromList
            [ (u, Set.fromList [c | (u', c) <- rows, u' == u])
            | u <- uids
            ]
  CreateClient uid cid -> runStatement (uid, cid) insertClientStatement
  DeleteClient uid cid -> runStatement (uid, cid) deleteClientStatement
  DeleteClients uid -> runStatement uid deleteClientsStatement

selectClientsStatement :: Hasql.Statement [UserId] (Vector (UserId, ClientId))
selectClientsStatement =
  dimapPG @(Vector _)
    [vectorStatement|SELECT user_id :: uuid, client_id :: text
                     FROM user_client_index
                     WHERE user_id = ANY ($1 :: uuid[])
                     ORDER BY user_id, client_id
                    |]

insertClientStatement :: Hasql.Statement (UserId, ClientId) ()
insertClientStatement =
  lmapPG
    [resultlessStatement|INSERT INTO user_client_index (user_id, client_id)
                         VALUES (($1 :: uuid), ($2 :: text))
                         ON CONFLICT DO NOTHING
                        |]

deleteClientStatement :: Hasql.Statement (UserId, ClientId) ()
deleteClientStatement =
  lmapPG
    [resultlessStatement|DELETE FROM user_client_index
                         WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text)
                        |]

deleteClientsStatement :: Hasql.Statement UserId ()
deleteClientsStatement =
  lmapPG
    [resultlessStatement|DELETE FROM user_client_index
                         WHERE user_id = ($1 :: uuid)
                        |]
