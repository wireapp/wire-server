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

-- | Postgres interpreter for 'ScimTokenStore'. Tokens are only ever stored and
-- looked up under their hashed key (see 'hashedKey').
--
-- FUTUREWORK: a Cassandra -> Postgres backfill must re-key legacy plaintext
-- rows to the hashed key (and drop duplicates by token id) when copying, as
-- this interpreter does not look up plaintext keys.
module Wire.ScimTokenStore.Postgres
  ( interpretScimTokenStoreToPostgres,
  )
where

import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import SAML2.WebSSO.Types qualified as SAML
import Wire.API.User.Scim
import Wire.Postgres
import Wire.ScimTokenStore (ScimTokenStore (..))

interpretScimTokenStoreToPostgres :: (PGConstraints r) => InterpreterFor ScimTokenStore r
interpretScimTokenStoreToPostgres = interpret $ \case
  Insert st sti -> upsertScimToken st sti
  Lookup st -> lookupScimToken st
  LookupByTeam tid -> getScimTokens tid
  UpdateName team tid name -> updateScimTokenName team tid name
  Delete team tid -> deleteScimToken team tid
  DeleteByTeam team -> deleteTeamScimTokens team

-- | The @scim_token@ columns (without the @token@ key): team, id, created_at,
-- idp, descr, name.
type ScimTokenPgRow =
  ( UUID,
    UUID,
    UTCTime,
    Maybe UUID,
    Text,
    Maybe Text
  )

fromScimTokenPgRow :: ScimTokenPgRow -> ScimTokenInfo
fromScimTokenPgRow (team, tid, createdAt, idp, descr, name) =
  ScimTokenInfo
    { stiTeam = Id team,
      stiId = Id tid,
      stiCreatedAt = createdAt,
      stiIdP = SAML.IdPId <$> idp,
      stiDescr = descr,
      -- the column is nullable (mirrors C*); default to the token id
      stiName = fromMaybe (UUID.toText tid) name
    }

-- | @sha512:<base64 digest>@, via 'ToByteString' like the C* key encoding.
hashedKey :: ScimToken -> Text
hashedKey = decodeUtf8 . toByteString' . hashScimToken

upsertScimToken :: (PGConstraints r) => ScimToken -> ScimTokenInfo -> Sem r ()
upsertScimToken token info =
  runStatement
    ( hashedKey token,
      toUUID info.stiTeam,
      toUUID info.stiId,
      info.stiCreatedAt,
      SAML.fromIdPId <$> info.stiIdP,
      info.stiDescr,
      Just info.stiName
    )
    upsertStatement
  where
    upsertStatement :: Hasql.Statement (Text, UUID, UUID, UTCTime, Maybe UUID, Text, Maybe Text) ()
    upsertStatement =
      [resultlessStatement|INSERT INTO scim_token
                             (token, team, id, created_at, idp, descr, name)
                           VALUES
                             ($1 :: text, $2 :: uuid, $3 :: uuid, $4 :: timestamptz, $5 :: uuid?, $6 :: text, $7 :: text?)
                           ON CONFLICT (token) DO UPDATE
                           SET team = excluded.team,
                               id = excluded.id,
                               created_at = excluded.created_at,
                               idp = excluded.idp,
                               descr = excluded.descr,
                               name = excluded.name
       |]

lookupScimToken :: (PGConstraints r) => ScimToken -> Sem r (Maybe ScimTokenInfo)
lookupScimToken token =
  fmap fromScimTokenPgRow <$> runStatement (hashedKey token) selectStatement
  where
    selectStatement :: Hasql.Statement Text (Maybe ScimTokenPgRow)
    selectStatement =
      [maybeStatement|SELECT
                        team :: uuid,
                        id :: uuid,
                        created_at :: timestamptz,
                        idp :: uuid?,
                        descr :: text,
                        name :: text?
                      FROM scim_token
                      WHERE token = ($1 :: text)
                      |]

getScimTokens :: (PGConstraints r) => TeamId -> Sem r [ScimTokenInfo]
getScimTokens team =
  sortOn (.stiCreatedAt) . map fromScimTokenPgRow . V.toList
    <$> runStatement (toUUID team) selectByTeamStatement
  where
    selectByTeamStatement :: Hasql.Statement UUID (Vector ScimTokenPgRow)
    selectByTeamStatement =
      [vectorStatement|SELECT
                         team :: uuid,
                         id :: uuid,
                         created_at :: timestamptz,
                         idp :: uuid?,
                         descr :: text,
                         name :: text?
                       FROM scim_token
                       WHERE team = ($1 :: uuid)
                       |]

updateScimTokenName :: (PGConstraints r) => TeamId -> ScimTokenId -> Text -> Sem r ()
updateScimTokenName team tid name =
  runStatement (name, toUUID team, toUUID tid) updateNameStatement
  where
    updateNameStatement :: Hasql.Statement (Text, UUID, UUID) ()
    updateNameStatement =
      [resultlessStatement|UPDATE scim_token
                           SET name = ($1 :: text)
                           WHERE team = ($2 :: uuid) AND id = ($3 :: uuid)
                          |]

deleteScimToken :: (PGConstraints r) => TeamId -> ScimTokenId -> Sem r ()
deleteScimToken team tid =
  runStatement (toUUID team, toUUID tid) deleteStatement
  where
    deleteStatement :: Hasql.Statement (UUID, UUID) ()
    deleteStatement =
      [resultlessStatement|DELETE FROM scim_token
                           WHERE team = ($1 :: uuid) AND id = ($2 :: uuid)
                          |]

deleteTeamScimTokens :: (PGConstraints r) => TeamId -> Sem r ()
deleteTeamScimTokens team =
  runStatement (toUUID team) deleteByTeamStatement
  where
    deleteByTeamStatement :: Hasql.Statement UUID ()
    deleteByTeamStatement =
      [resultlessStatement|DELETE FROM scim_token
                           WHERE team = ($1 :: uuid)
                          |]
