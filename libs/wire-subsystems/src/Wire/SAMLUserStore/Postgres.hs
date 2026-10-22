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

module Wire.SAMLUserStore.Postgres
  ( interpretSAMLUserStoreToPostgres,
    upsertSAMLUserRow,
    insertSAMLUserIfAbsent,
  )
where

import Control.Error (lastMay)
import Data.Id
import Data.Text.Lazy qualified as LT
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import SAML2.WebSSO qualified as SAML
import Wire.API.PostgresMarshall
import Wire.API.User.Saml (normalizeQualifiedNameId)
import Wire.Postgres
import Wire.SAMLUserStore

-- | Same page size as the Cassandra interpreter uses for its auto-paging.
pageSize :: Int32
pageSize = 200

interpretSAMLUserStoreToPostgres :: (PGConstraints r) => InterpreterFor SAMLUserStore r
interpretSAMLUserStoreToPostgres = interpret $ \case
  Insert ur uid -> upsertSAMLUserRow ur uid
  Get ur -> getSAMLUserPg ur
  DeleteByIssuer iss -> deleteByIssuerPg iss
  Delete uid ur -> deleteSAMLUserPg uid ur
  GetAllByIssuer iss -> getAllByIssuerPg iss

insertTuple :: SAML.UserRef -> UserId -> (Text, Text, Text, UUID)
insertTuple (SAML.UserRef tenant subject) uid =
  ( postgresMarshall tenant,
    postgresMarshall (normalizeQualifiedNameId subject),
    LT.toStrict (SAML.encodeElem subject),
    postgresMarshall uid
  )

-- | Upsert a full row. The migration worker uses this to copy rows verbatim;
-- the normal write path goes through the same statement (upsert = overwrite).
upsertSAMLUserRow :: (PGConstraints r) => SAML.UserRef -> UserId -> Sem r ()
upsertSAMLUserRow ur uid =
  runStatement (insertTuple ur uid) upsertSAMLUserStatement

-- | Insert only if no row exists for the (issuer, normalized_uname_id) key.
-- The legacy-table sweep uses this: @user_v2@ rows were copied first and are
-- authoritative, so a stale legacy row must never overwrite them.
insertSAMLUserIfAbsent :: (PGConstraints r) => SAML.UserRef -> UserId -> Sem r ()
insertSAMLUserIfAbsent ur uid =
  runStatement (insertTuple ur uid) insertIfAbsentStatement

insertIfAbsentStatement :: Hasql.Statement (Text, Text, Text, UUID) ()
insertIfAbsentStatement =
  lmapPG
    [resultlessStatement|INSERT INTO spar_user
                           (issuer, normalized_uname_id, sso_id, uid)
                         VALUES
                           ($1 :: text, $2 :: text, $3 :: text, $4 :: uuid)
                         ON CONFLICT (issuer, normalized_uname_id) DO NOTHING
      |]

upsertSAMLUserStatement :: Hasql.Statement (Text, Text, Text, UUID) ()
upsertSAMLUserStatement =
  lmapPG
    [resultlessStatement|INSERT INTO spar_user
                           (issuer, normalized_uname_id, sso_id, uid)
                         VALUES
                           ($1 :: text, $2 :: text, $3 :: text, $4 :: uuid)
                         ON CONFLICT (issuer, normalized_uname_id) DO UPDATE
                         SET sso_id = ($3 :: text),
                             uid = ($4 :: uuid)
      |]

lookupSAMLUserStatement :: Hasql.Statement (Text, Text) (Maybe UUID)
lookupSAMLUserStatement =
  lmapPG
    [maybeStatement|SELECT uid :: uuid
                    FROM spar_user
                    WHERE issuer = ($1 :: text) AND normalized_uname_id = ($2 :: text)
                   |]

getSAMLUserPg :: (PGConstraints r) => SAML.UserRef -> Sem r (Maybe UserId)
getSAMLUserPg (SAML.UserRef tenant subject) = do
  mUid <-
    runStatement
      (postgresMarshall tenant, postgresMarshall (normalizeQualifiedNameId subject))
      lookupSAMLUserStatement
  pure (mUid >>= hush . postgresUnmarshall)

deleteByIssuerStatement :: Hasql.Statement Text ()
deleteByIssuerStatement =
  lmapPG
    [resultlessStatement|DELETE FROM spar_user
                         WHERE issuer = ($1 :: text)
                        |]

deleteByIssuerPg :: (PGConstraints r) => SAML.Issuer -> Sem r ()
deleteByIssuerPg issuer =
  runStatement (postgresMarshall issuer) deleteByIssuerStatement

deleteSAMLUserStatement :: Hasql.Statement (Text, Text, UUID) ()
deleteSAMLUserStatement =
  lmapPG
    [resultlessStatement|DELETE FROM spar_user
                         WHERE issuer = ($1 :: text)
                           AND normalized_uname_id = ($2 :: text)
                           AND uid = ($3 :: uuid)
                        |]

-- | The uid guard mirrors the Cassandra read-then-delete (only delete when the
-- stored uid matches) but is atomic, closing the get/delete race window.
deleteSAMLUserPg :: (PGConstraints r) => UserId -> SAML.UserRef -> Sem r ()
deleteSAMLUserPg uid (SAML.UserRef tenant subject) =
  runStatement
    ( postgresMarshall tenant,
      postgresMarshall (normalizeQualifiedNameId subject),
      postgresMarshall uid
    )
    deleteSAMLUserStatement

selectPageStatement :: Hasql.Statement (Text, Maybe Text, Text, Int32) (Vector (Text, Text, UUID))
selectPageStatement =
  lmapPG
    [vectorStatement|SELECT normalized_uname_id :: text, sso_id :: text, uid :: uuid
                     FROM spar_user
                     WHERE issuer = ($1 :: text)
                       AND ($2 :: text? IS NULL OR normalized_uname_id > ($3 :: text))
                     ORDER BY normalized_uname_id ASC
                     LIMIT ($4 :: int)
                    |]

-- | Keyset pagination over @normalized_uname_id@ (the second PK column),
-- collecting all rows for the issuer.
getAllByIssuerPg :: (PGConstraints r) => SAML.Issuer -> Sem r [(SAML.UserRef, UserId)]
getAllByIssuerPg issuer =
  selectAllPages issuer (postgresMarshall issuer) Nothing

selectAllPages :: (PGConstraints r) => SAML.Issuer -> Text -> Maybe Text -> Sem r [(SAML.UserRef, UserId)]
selectAllPages issuer issuerTxt cursor = do
  rows <- V.toList <$> runStatement (issuerTxt, cursor, fromMaybe "" cursor, pageSize) selectPageStatement
  let items = mapMaybe (mkItem issuer) rows
  case lastMay rows of
    Nothing -> pure items
    Just (lastKey, _, _) -> (items ++) <$> selectAllPages issuer issuerTxt (Just lastKey)
  where
    -- Rows whose @sso_id@ text is not a decodable NameID cannot be represented
    -- as a UserRef; the Cassandra interpreter would have failed the whole page
    -- at the CQL layer, so dropping them here only affects corrupt rows.
    mkItem :: SAML.Issuer -> (Text, Text, UUID) -> Maybe (SAML.UserRef, UserId)
    mkItem iss (_, ssoId, uid) = do
      uid' <- hush (postgresUnmarshall uid)
      subject <- hush (SAML.decodeElem (LT.fromStrict ssoId))
      pure (SAML.UserRef iss subject, uid')

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just
