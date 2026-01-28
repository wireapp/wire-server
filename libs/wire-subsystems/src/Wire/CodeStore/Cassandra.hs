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

module Wire.CodeStore.Cassandra
  ( interpretCodeStoreToCassandra,
  )
where

import Cassandra
import Data.Code
import Data.Map qualified as Map
import Data.Misc (HttpsUrl)
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Password
import Wire.CodeStore (CodeStore (..))
import Wire.CodeStore.Cassandra.Queries qualified as Cql
import Wire.CodeStore.Code as Code
import Wire.Util (embedClientInput)

interpretCodeStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member (Input (Either HttpsUrl (Map Text HttpsUrl))) r
  ) =>
  Sem (CodeStore ': r) a ->
  Sem r a
interpretCodeStoreToCassandra = interpret $ \case
  GetCode k -> do
    embedClientInput $ lookupCode k
  CreateCode code mPw -> do
    embedClientInput $ insertCode code mPw
  DeleteCode k -> do
    embedClientInput $ deleteCode k
  MakeKey cid -> do
    Code.mkKey cid
  GenerateCode cid t -> do
    Code.generate cid t
  GetConversationCodeURI mbHost -> do
    convCodeURI <- input
    case convCodeURI of
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
  retry x5 (write Cql.insertCode (params LocalQuorum (k, v, cnv, mPw, t)))

-- | Lookup a conversation by code.
lookupCode :: Key -> Client (Maybe (Code, Maybe Password))
lookupCode k =
  fmap (toCode k) <$> retry x1 (query1 Cql.lookupCode (params LocalQuorum (Identity k)))

-- | Delete a code associated with the given conversation key
deleteCode :: Key -> Client ()
deleteCode k = retry x5 $ write Cql.deleteCode (params LocalQuorum (Identity k))
