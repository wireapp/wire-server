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
  GetCode k -> do
    lookupCode k
  CreateCode code mPw -> do
    insertCode code mPw
  DeleteCode k -> do
    deleteCode k
  MakeKey cid -> do
    Code.mkKey cid
  GenerateCode cid t -> do
    Code.generate cid t
  GetConversationCodeURI mbHost -> do
    convCodeURI <- input
    pure $ case convCodeURI of
      Left uri -> Just uri
      Right map' -> mbHost >>= flip Map.lookup map'

insertCode :: (PGConstraints r) => Code -> Maybe Password -> Sem r ()
insertCode c password = do
  runStatement (codeKey c, codeConversation c, password, codeValue c, round (codeTTL c)) insert
  where
    insert :: Hasql.Statement (Key, ConvId, Maybe Password, Value, Int32) ()
    insert =
      lmapPG
        [resultlessStatement|INSERT INTO conversation_codes
                               (key, conversation, password, value, expires_at)
                             VALUES
                               ($1 :: text, $2 :: uuid, $3 :: bytea?, $4 :: text, now() + make_interval(secs => $5 :: int))
                             ON CONFLICT (key) DO UPDATE
                             SET conversation = ($2 :: uuid),
                                 password = ($3 :: bytea?),
                                 value = ($4 :: text),
                                 expires_at = now() + make_interval(secs => $5 :: int)
         |]

lookupCode :: (PGConstraints r) => Key -> Sem r (Maybe (Code, Maybe Password))
lookupCode k = do
  mRow <- runStatement k selectCode
  pure $ fmap (toCode k) mRow
  where
    selectCode :: Hasql.Statement Key (Maybe (Value, Int32, ConvId, Maybe Password))
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
                        WHERE key = ($1 :: text) AND expires_at > now ()
                        |]

deleteCode :: (PGConstraints r) => Key -> Sem r ()
deleteCode k =
  runStatement k delete
  where
    delete :: Hasql.Statement Key ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM conversation_codes
                             WHERE key = ($1 :: text) 
                            |]
