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
import Data.Id
import Data.Map qualified as Map
import Data.Misc (HttpsUrl)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Password
import Wire.API.PostgresMarshall
import Wire.CodeStore (CodeStore (..))
import Wire.CodeStore.Code as Code
import Wire.Postgres

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
    pure $ case convCodeURI of
      Left uri -> Just uri
      Right map' -> mbHost >>= flip Map.lookup map'

insertCode :: (PGConstraints r) => Code -> Maybe Password -> Sem r ()
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
                             ON CONFLICT (key, scope) DO UPDATE
                             SET conversation = ($3 :: uuid),
                                 password = ($4 :: bytea?),
                                 value = ($5 :: text),
                                 expires_at = now() + make_interval(secs => $6 :: int)
         |]

lookupCode :: (PGConstraints r) => Key -> Scope -> Sem r (Maybe (Code, Maybe Password))
lookupCode k s = do
  mRow <- runStatement (k, s) selectCode
  pure $ fmap (toCode k s) mRow
  where
    selectCode :: Hasql.Statement (Key, Scope) (Maybe (Value, Int32, ConvId, Maybe Password))
    selectCode =
      dimapPG
        -- on the extraction of the remaining seconds of the TTL
        -- `expires_at - now()` produces an interval representing how much time is left
        -- `EXTRACT(EPOCH FROM interval)` converts that interval to seconds (a double precision)
        -- `FLOOR(...)` truncates fractional seconds
        -- `GREATEST(0, ...)` clamps negatives to 0 (expired rows)
        -- `::int4` casts to 32‑bit integer.
        [maybeStatement|SELECT 
                          value :: text,
                          GREATEST(0, FLOOR(EXTRACT(EPOCH FROM (expires_at - now()))))::int4 AS ttl_secs,
                          conversation :: uuid,
                          password :: bytea?
                        FROM conversation_codes
                        WHERE key = ($1 :: text) AND scope = ($2 :: int) AND expires_at > now ()
                        |]

deleteCode :: (PGConstraints r) => Key -> Scope -> Sem r ()
deleteCode k s =
  runStatement (k, s) delete
  where
    delete :: Hasql.Statement (Key, Scope) ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM conversation_codes
                             WHERE key = ($1 :: text) AND scope = ($2 :: int)
                            |]
