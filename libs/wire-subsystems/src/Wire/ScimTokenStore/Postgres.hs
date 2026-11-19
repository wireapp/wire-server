{-# LANGUAGE RecordWildCards #-}

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

module Wire.ScimTokenStore.Postgres
  ( interpretScimTokenStoreToPostgres,
    upsertScimTokenByKey,
    pgConvertPlaintextToken,
  )
where

import Data.Id
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.User.Scim
import Wire.Postgres
import Wire.ScimTokenStore (ScimTokenStore (..))

interpretScimTokenStoreToPostgres :: (PGConstraints r) => InterpreterFor ScimTokenStore r
interpretScimTokenStoreToPostgres = interpret $ \case
  Insert st sti ->
    upsertScimTokenByKey (ScimTokenLookupKeyHashed (hashScimToken st)) sti
  Lookup st -> lookupScimTokenPg st
  LookupByTeam tid -> getScimTokensPg tid
  UpdateName team tid name -> updateScimTokenNamePg team tid name
  Delete team tid -> deleteScimTokenPg team tid
  DeleteByTeam team -> deleteTeamScimTokensPg team

-- | The @scim_token@ row encoding, mirroring the C* @ScimTokenRow@ exactly.
type ScimTokenPgRow =
  ( Text,
    UUID,
    UUID,
    UTCTime,
    Maybe UUID,
    Text,
    Maybe Text
  )

fromScimTokenPgRow :: ScimTokenPgRow -> Maybe ScimTokenInfo
fromScimTokenPgRow (_, team, tid, createdAt, idp, descr, name) =
  case (postgresUnmarshall team, postgresUnmarshall tid, postgresUnmarshall createdAt, postgresUnmarshall idp) of
    (Right team', Right tid', Right createdAt', Right idp') ->
      Just
        ScimTokenInfo
          { stiTeam = team',
            stiId = tid',
            stiCreatedAt = createdAt',
            stiIdP = idp',
            stiDescr = descr,
            -- legacy rows may have a null name; consumers default to the token id
            stiName = fromMaybe (idToText tid') name
          }
    _ -> Nothing

insertTuple :: ScimTokenLookupKey -> ScimTokenInfo -> (Text, UUID, UUID, UTCTime, Maybe UUID, Text, Maybe Text)
insertTuple key ScimTokenInfo {..} =
  ( postgresMarshall key,
    postgresMarshall stiTeam,
    postgresMarshall stiId,
    stiCreatedAt,
    postgresMarshall stiIdP,
    stiDescr,
    Just stiName
  )

-- | Upsert with a caller-supplied lookup key. The migration worker uses this to
-- preserve plaintext-vs-hashed keys; the normal write path always uses the
-- hashed key.
upsertScimTokenByKey :: (PGConstraints r) => ScimTokenLookupKey -> ScimTokenInfo -> Sem r ()
upsertScimTokenByKey key info =
  runStatement (insertTuple key info) upsertScimTokenStatement

upsertScimTokenStatement :: Hasql.Statement (Text, UUID, UUID, UTCTime, Maybe UUID, Text, Maybe Text) ()
upsertScimTokenStatement =
  lmapPG
    [resultlessStatement|INSERT INTO scim_token
                           (token, team, id, created_at, idp, descr, name)
                         VALUES
                           ($1 :: text, $2 :: uuid, $3 :: uuid, $4 :: timestamptz, $5 :: uuid?, $6 :: text, $7 :: text?)
                         ON CONFLICT (token) DO UPDATE
                         SET team = ($2 :: uuid),
                             id = ($3 :: uuid),
                             created_at = ($4 :: timestamptz),
                             idp = ($5 :: uuid?),
                             descr = ($6 :: text),
                             name = ($7 :: text?)
     |]

-- | Look up a token by both its hashed and (legacy) plaintext key. If only the
-- plaintext-key row is present, convert it to the hashed key — on Postgres
-- directly, mirroring the Cassandra re-keying in 'Wire.ScimTokenStore.Cassandra.lookupScimToken'.
lookupScimTokenPg :: (PGConstraints r) => ScimToken -> Sem r (Maybe ScimTokenInfo)
lookupScimTokenPg token = do
  rows <- runStatement keys selectScimTokenByKeys
  let byKey =
        [ (key, info)
        | row@(tok, _, _, _, _, _, _) <- V.toList rows,
          Right key <- [postgresUnmarshall tok],
          Just info <- [fromScimTokenPgRow row]
        ]
  case byKey of
    -- A plaintext-key row means the token predates hashing. Prefer it over a
    -- hashed row (both can exist if a previous conversion crashed between the
    -- upsert and the delete, or if the migration worker copied the plaintext
    -- row after a concurrent conversion): convert it to the hashed key and
    -- delete the stale plaintext row, mirroring the C* re-keying. The
    -- conversion re-reads the row in-statement, so a revocation that raced the
    -- lookup cannot be resurrected here.
    _ | Just (ScimTokenLookupKeyPlaintext plain, info) <- find (isPlaintextKey . fst) byKey -> do
      pgConvertPlaintextToken plain
      pure (Just info)
    ((_, info) : _) -> pure (Just info)
    [] -> pure Nothing
  where
    isPlaintextKey = \case
      ScimTokenLookupKeyPlaintext _ -> True
      ScimTokenLookupKeyHashed _ -> False

    keys =
      V.fromList
        [ postgresMarshall (ScimTokenLookupKeyHashed (hashScimToken token)),
          postgresMarshall (ScimTokenLookupKeyPlaintext token)
        ]

selectScimTokenByKeys :: Hasql.Statement (Vector Text) (Vector (Text, UUID, UUID, UTCTime, Maybe UUID, Text, Maybe Text))
selectScimTokenByKeys =
  [vectorStatement|SELECT
                      token :: text,
                      team :: uuid,
                      id :: uuid,
                      created_at :: timestamptz,
                      idp :: uuid?,
                      descr :: text,
                      name :: text?
                    FROM scim_token
                    WHERE token = ANY ($1 :: text[])
                    |]

-- | Re-key a plaintext token row to its hashed key: copy the CURRENT row
-- contents under the hashed key, then delete the plaintext row. The
-- @INSERT ... SELECT@ reads the row in-statement (not the detached result of
-- an earlier lookup), so if the token was deleted between the lookup and this
-- conversion, there is nothing to insert and no credential is resurrected;
-- @DO NOTHING@ never clobbers a fresher hashed row written by a concurrent
-- insert or conversion. Used both by the Postgres read path and mirrored from
-- the DualWrite interpreter.
pgConvertPlaintextToken :: (PGConstraints r) => ScimToken -> Sem r ()
pgConvertPlaintextToken token = do
  runStatement (hashedKey, plainKey) convertPlaintextStatement
  runStatement plainKey deletePlaintextStatement
  where
    hashedKey = postgresMarshall (ScimTokenLookupKeyHashed (hashScimToken token))
    plainKey = postgresMarshall (ScimTokenLookupKeyPlaintext token)

convertPlaintextStatement :: Hasql.Statement (Text, Text) ()
convertPlaintextStatement =
  lmapPG
    [resultlessStatement|INSERT INTO scim_token
                           (token, team, id, created_at, idp, descr, name)
                         SELECT ($1 :: text), team, id, created_at, idp, descr, name
                         FROM scim_token
                         WHERE token = ($2 :: text)
                         ON CONFLICT (token) DO NOTHING
     |]

deletePlaintextStatement :: Hasql.Statement Text ()
deletePlaintextStatement =
  [resultlessStatement|DELETE FROM scim_token
                       WHERE token = ($1 :: text)
                      |]

getScimTokensPg :: (PGConstraints r) => TeamId -> Sem r [ScimTokenInfo]
getScimTokensPg team = do
  rows <- runStatement (postgresMarshall team) selectScimTokenByTeam
  pure $
    sortOn (.stiCreatedAt) $
      mapMaybe fromScimTokenPgRow $
        Map.elems $
          Map.fromListWith preferPlaintextKey [(tokenId r, r) | r <- V.toList rows]
  where
    -- Plaintext and hashed rows can coexist for the same token after an
    -- interrupted conversion, or after a read-path conversion raced the
    -- backfill. Deduplicate by token id, preferring the plaintext-key row
    -- (same precedence as 'lookupScimTokenPg'), so one credential is not
    -- listed — and counted toward maxScimTokens — twice.
    preferPlaintextKey a b
      | isPlaintextRow a = a
      | isPlaintextRow b = b
      | otherwise = a
    isPlaintextRow (tok, _, _, _, _, _, _) = case postgresUnmarshall tok of
      Right ScimTokenLookupKeyPlaintext {} -> True
      _ -> False
    tokenId (_, _, tid, _, _, _, _) = tid

selectScimTokenByTeam :: Hasql.Statement UUID (Vector (Text, UUID, UUID, UTCTime, Maybe UUID, Text, Maybe Text))
selectScimTokenByTeam =
  [vectorStatement|SELECT
                      token :: text,
                      team :: uuid,
                      id :: uuid,
                      created_at :: timestamptz,
                      idp :: uuid?,
                      descr :: text,
                      name :: text?
                    FROM scim_token
                    WHERE team = ($1 :: uuid)
                    |]

updateScimTokenNamePg :: (PGConstraints r) => TeamId -> ScimTokenId -> Text -> Sem r ()
updateScimTokenNamePg team tid name =
  runStatement (name, postgresMarshall team, postgresMarshall tid) updateNameStatement

updateNameStatement :: Hasql.Statement (Text, UUID, UUID) ()
updateNameStatement =
  lmapPG
    [resultlessStatement|UPDATE scim_token
                         SET name = ($1 :: text)
                         WHERE team = ($2 :: uuid) AND id = ($3 :: uuid)
                        |]

deleteScimTokenPg :: (PGConstraints r) => TeamId -> ScimTokenId -> Sem r ()
deleteScimTokenPg team tid =
  runStatement (postgresMarshall team, postgresMarshall tid) deleteScimTokenStatement

deleteScimTokenStatement :: Hasql.Statement (UUID, UUID) ()
deleteScimTokenStatement =
  lmapPG
    [resultlessStatement|DELETE FROM scim_token
                         WHERE team = ($1 :: uuid) AND id = ($2 :: uuid)
                        |]

deleteTeamScimTokensPg :: (PGConstraints r) => TeamId -> Sem r ()
deleteTeamScimTokensPg team =
  runStatement (postgresMarshall team) deleteTeamScimTokensStatement

deleteTeamScimTokensStatement :: Hasql.Statement UUID ()
deleteTeamScimTokensStatement =
  [resultlessStatement|DELETE FROM scim_token
                       WHERE team = ($1 :: uuid)
                      |]
