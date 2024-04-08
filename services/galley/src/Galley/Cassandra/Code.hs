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

module Galley.Cassandra.Code
  ( interpretCodeStoreToCassandra,
  )
where

import Cassandra
import Control.Lens
import Data.Code
import Data.Map qualified as Map
import Galley.Cassandra.Queries qualified as Cql
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Data.Types
import Galley.Data.Types qualified as Code
import Galley.Effects.CodeStore (CodeStore (..))
import Galley.Env
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Password

interpretCodeStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member (Input Env) r,
    Member TinyLog r
  ) =>
  Sem (CodeStore ': r) a ->
  Sem r a
interpretCodeStoreToCassandra = interpret $ \case
  GetCode k s -> do
    logEffect "CodeStore.GetCode"
    embedClient $ lookupCode k s
  CreateCode code mPw -> do
    logEffect "CodeStore.CreateCode"
    embedClient $ insertCode code mPw
  DeleteCode k s -> do
    logEffect "CodeStore.DeleteCode"
    embedClient $ deleteCode k s
  MakeKey cid -> do
    logEffect "CodeStore.MakeKey"
    Code.mkKey cid
  GenerateCode cid s t -> do
    logEffect "CodeStore.GenerateCode"
    Code.generate cid s t
  GetConversationCodeURI mbHost -> do
    logEffect "CodeStore.GetConversationCodeURI"
    env <- input
    case env ^. convCodeURI of
      Left uri -> pure (Just uri)
      Right map' ->
        case mbHost of
          Just host -> pure (Map.lookup host map')
          Nothing -> pure Nothing

-- | Insert a conversation code
insertCode :: Code -> Maybe Password -> Client ()
insertCode c mPw = do
  let k = codeKey c
  let v = codeValue c
  let cnv = codeConversation c
  let t = round (codeTTL c)
  let s = codeScope c
  retry x5 (write Cql.insertCode (params LocalQuorum (k, v, cnv, s, mPw, t)))

-- | Lookup a conversation by code.
lookupCode :: Key -> Scope -> Client (Maybe (Code, Maybe Password))
lookupCode k s =
  fmap (toCode k s) <$> retry x1 (query1 Cql.lookupCode (params LocalQuorum (k, s)))

-- | Delete a code associated with the given conversation key
deleteCode :: Key -> Scope -> Client ()
deleteCode k s = retry x5 $ write Cql.deleteCode (params LocalQuorum (k, s))
