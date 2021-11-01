-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Data
  ( ResultSet,
    ResultSetType (..),
    PageWithState (..),
    mkResultSet,
    resultSetType,
    resultSetResult,
    schemaVersion,

    -- * Conversation Codes
    lookupCode,
    deleteCode,
    insertCode,

    -- * Clients
    eraseClients,
    lookupClients,
    lookupClients',
    updateClient,

    -- * Utilities
    localOne2OneConvId,

    -- * Defaults
    defRole,
    defRegularConvAccess,
  )
where

import Brig.Types.Code
import Cassandra
import Control.Arrow (second)
import Control.Lens hiding ((<|))
import Data.Id as Id
import Data.List.Split (chunksOf)
import Galley.App
import Galley.Data.Access
import Galley.Data.Conversation
import Galley.Data.Instances ()
import qualified Galley.Data.Queries as Cql
import Galley.Data.ResultSet
import Galley.Data.Types as Data
import Galley.Types.Clients (Clients)
import qualified Galley.Types.Clients as Clients
import Imports hiding (Set, max)
import qualified UnliftIO

schemaVersion :: Int32
schemaVersion = 54

-- | Insert a conversation code
insertCode :: Code -> Galley r ()
insertCode c = do
  let k = codeKey c
  let v = codeValue c
  let cnv = codeConversation c
  let t = round (codeTTL c)
  let s = codeScope c
  retry x5 (write Cql.insertCode (params Quorum (k, v, cnv, s, t)))

-- | Lookup a conversation by code.
lookupCode :: Key -> Scope -> Galley r (Maybe Code)
lookupCode k s = fmap (toCode k s) <$> retry x1 (query1 Cql.lookupCode (params Quorum (k, s)))

-- | Delete a code associated with the given conversation key
deleteCode :: Key -> Scope -> Galley r ()
deleteCode k s = retry x5 $ write Cql.deleteCode (params Quorum (k, s))

-- Clients ------------------------------------------------------------------

updateClient :: Bool -> UserId -> ClientId -> Galley r ()
updateClient add usr cls = do
  let q = if add then Cql.addMemberClient else Cql.rmMemberClient
  retry x5 $ write (q cls) (params Quorum (Identity usr))

-- Do, at most, 16 parallel lookups of up to 128 users each
lookupClients :: [UserId] -> Galley r Clients
lookupClients = liftClient . lookupClients'

-- This is only used by tests
lookupClients' :: [UserId] -> Client Clients
lookupClients' users =
  Clients.fromList . concat . concat
    <$> forM (chunksOf 2048 users) (UnliftIO.mapConcurrently getClients . chunksOf 128)
  where
    getClients us =
      map (second fromSet)
        <$> retry x1 (query Cql.selectClients (params Quorum (Identity us)))

eraseClients :: UserId -> Galley r ()
eraseClients user = retry x5 (write Cql.rmClients (params Quorum (Identity user)))
