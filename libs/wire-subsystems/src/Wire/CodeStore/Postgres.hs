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

module Wire.CodeStore.Postgres
  ( interpretCodeStoreToPostgres,
  )
where

import Data.Code
import Data.Map qualified as Map
import Data.Misc (HttpsUrl)
import Imports
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Polysemy
import Wire.API.PostgresMarshall
import Polysemy.Input
import Wire.API.Password
import Wire.CodeStore (CodeStore (..))
import Wire.CodeStore.Code as Code
import Wire.Postgres
import Data.Id

interpretCodeStoreToPostgres ::
  ( PGConstraints r,
    Member (Input (Either HttpsUrl (Map Text HttpsUrl))) r
  ) =>
  Sem (CodeStore ': r) a ->
  Sem r a
interpretCodeStoreToPostgres = interpret $ \case
  GetCode k s -> do
    lookupCode k s
  CreateCode code mPw -> do
    insertCode code mPw
  DeleteCode k s -> do
    deleteCode k s
  MakeKey cid -> do
    Code.mkKey cid
  GenerateCode cid s t -> do
    Code.generate cid s t
  GetConversationCodeURI mbHost -> do
    convCodeURI <- input
    case convCodeURI of
      Left uri -> pure (Just uri)
      Right map' ->
        case mbHost of
          Just host -> pure (Map.lookup host map')
          Nothing -> pure Nothing

insertCode :: PGConstraints r => Code -> Maybe Password -> Sem r ()
insertCode c mPw = do
  let k = codeKey c
  let v = codeValue c
  let cnv = codeConversation c
  let t = round (codeTTL c)
  let s = codeScope c
  runStatement (k, s, cnv, mPw, v, t) insert
  where
    insert :: Hasql.Statement (Key, Scope, ConvId, Maybe Password, Value, Int32) ()
    insert =
      lmapPG
        [resultlessStatement|INSERT INTO conversation_codes
                               (key, scope, conversation, password, value, expires_at)
                             VALUES
                               ($1 :: text, $2 :: int, $3 :: uuid, $4 :: bytea?, $5 :: text, now() + make_interval(secs => $6 :: int))
        |]
                             -- ON CONFLICT (key, scope) DO UPDATE
                             -- SET conversation = EXCLUDED.conversation,
                             --     password = EXCLUDED.password,
                             --     value = EXCLUDED.value,
                             --     expires_at = EXCLUDED.expires_at; 


lookupCode :: Key -> Scope -> Sem r (Maybe (Code, Maybe Password))
lookupCode k s =
  -- fmap (toCode k s) <$> retry x1 (query1 Cql.lookupCode (params LocalQuorum (k, s)))
  todo

deleteCode :: Key -> Scope -> Sem r ()
deleteCode k s = todo -- retry x5 $ write Cql.deleteCode (params LocalQuorum (k, s))
