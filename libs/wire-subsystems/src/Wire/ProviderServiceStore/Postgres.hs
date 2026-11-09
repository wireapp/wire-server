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

module Wire.ProviderServiceStore.Postgres
  ( interpretProviderServiceStoreToPostgres,
    insertProviderService,
    upsertProviderServiceIfNewer,
    insertProviderServiceTagRow,
    insertProviderServicePrefixRow,
    insertProviderServiceWhitelist,
    insertProviderServiceWhitelistRev,
  )
where

import Cassandra (Page (..), emptyPage)
import Data.Aeson as A
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Misc
import Data.Range
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions qualified as Tx
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Profile (Asset, Name (..), fromName)
import Wire.Postgres
import Wire.ProviderServiceStore (IndexRow, ProviderServiceStore (..), RangedServiceTags, filterResults, mkPrefixIndex, nextRow, toLowerName, trim)

--------------------------------------------------------------------------------
-- Interpreter

interpretProviderServiceStoreToPostgres ::
  (PGConstraints r) =>
  Sem (ProviderServiceStore ': r) a ->
  Sem r a
interpretProviderServiceStoreToPostgres = interpret $ \case
  CreateProviderService pid name summary descr url token key fprint assets tags -> do
    sid <- liftIO randomId
    runStatement
      ( pid,
        sid,
        name,
        summary,
        descr,
        url,
        [token],
        [key],
        [fprint],
        assets,
        tags,
        False
      )
      insertProviderService
    pure sid
  LookupProviderService pid sid -> lookupProviderServiceRow pid sid
  ListProviderServices pid -> listProviderServiceRows pid
  UpdateProviderService pid sid svcName svcTags nameChange summary descr assets tagsChange enabled ->
    updateProviderServiceRow pid sid svcName svcTags nameChange summary descr assets tagsChange enabled
  DeleteProviderService pid sid name tags -> deleteProviderServiceRow pid sid name tags
  LookupProviderServiceProfile pid sid -> lookupProviderServiceProfileRow pid sid
  ListProviderServiceProfiles pid -> listProviderServiceProfileRows pid
  LookupProviderServiceConn pid sid -> lookupProviderServiceConnRow pid sid
  UpdateProviderServiceConn pid sid url tokens keys enabled ->
    updateProviderServiceConnRow pid sid url tokens keys enabled
  InsertProviderServiceIndexes pid sid name tags -> do
    runTransaction Tx.Serializable Tx.Write $ do
      for_ (nonEmptyTags tags) $ \t ->
        for_ (unfoldTagsInto t []) $ \tag ->
          Transaction.statement (defBucket, tag, toLowerName name, sid, pid) insertProviderServiceTagRow
      Transaction.statement (mkPrefixIndex name, toLowerName name, sid, pid) insertProviderServicePrefixRow
  DeleteProviderServiceIndexes _pid sid name tags -> do
    runTransaction Tx.Serializable Tx.Write $ do
      for_ (unfoldTags tags) $ \tag ->
        Transaction.statement (defBucket, tag, toLowerName name, sid) deleteProviderServiceTagRow
      Transaction.statement (mkPrefixIndex name, toLowerName name, sid) deleteProviderServicePrefixRow
  PaginateProviderServiceTags tags start size providerFilter ->
    paginateProviderServiceTags tags start size providerFilter
  PaginateProviderServiceNames mbPrefix size providerFilter ->
    paginateProviderServiceNames mbPrefix size providerFilter
  InsertProviderServiceWhitelist tid pid sid -> do
    runStatement (tid, pid, sid) insertProviderServiceWhitelist
    runStatement (tid, pid, sid) insertProviderServiceWhitelistRev
  DeleteProviderServiceWhitelist mbTid pid sid -> deleteProviderServiceWhitelistRows mbTid pid sid
  PaginateProviderServiceWhitelist tid mbPrefix filterDisabled size ->
    paginateProviderServiceWhitelist tid mbPrefix filterDisabled size
  GetProviderServiceWhitelistStatus tid pid sid ->
    runStatement (tid, pid, sid) selectProviderServiceWhitelistStatus

--------------------------------------------------------------------------------
-- provider_service

insertProviderService ::
  Hasql.Statement
    ( ProviderId,
      ServiceId,
      Name,
      Text,
      Text,
      HttpsUrl,
      [ServiceToken],
      [ServiceKey],
      [Fingerprint Rsa],
      [Asset],
      Set ServiceTag,
      Bool
    )
    ()
insertProviderService =
  lmapPG
    @(UUID, UUID, Text, Text, Text, ByteString, Vector Text, A.Value, Vector ByteString, Value, Vector Int64, Bool)
    [resultlessStatement|INSERT INTO provider_service
                           (provider, id, name, summary, descr, base_url, auth_tokens, pubkeys,
                            fingerprints, assets, tags, enabled, last_update)
                         VALUES
                           ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: text, $5 :: text,
                            $6 :: bytea, $7 :: text[], $8 :: jsonb, $9 :: bytea[],
                            $10 :: jsonb, $11 :: bigint[], $12 :: boolean, now())
        |]

-- | Backfill-only upsert used by Wire.ProviderServiceStore.Migration. On
-- conflict the row is only overwritten when the replayed Cassandra row is at
-- least as new as what is stored (ties go to Cassandra), cf.
-- @Wire.ServiceStore.Postgres.upsertServiceIfNewer@.
--
-- The Cassandra row timestamp is the /maximum/ of the per-column writetimes
-- (brig's service writes are field-granular, see the migration loop haddock).
upsertProviderServiceIfNewer ::
  Hasql.Statement
    ( ProviderId,
      ServiceId,
      Name,
      Text,
      Text,
      HttpsUrl,
      [ServiceToken],
      [ServiceKey],
      [Fingerprint Rsa],
      [Asset],
      Set ServiceTag,
      Bool,
      UTCTime
    )
    ()
upsertProviderServiceIfNewer =
  lmapPG
    @(UUID, UUID, Text, Text, Text, ByteString, Vector Text, A.Value, Vector ByteString, Value, Vector Int64, Bool, UTCTime)
    [resultlessStatement|INSERT INTO provider_service
                           (provider, id, name, summary, descr, base_url, auth_tokens, pubkeys,
                            fingerprints, assets, tags, enabled, last_update)
                         VALUES
                           ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: text, $5 :: text,
                            $6 :: bytea, $7 :: text[], $8 :: jsonb, $9 :: bytea[],
                            $10 :: jsonb, $11 :: bigint[], $12 :: boolean, ($13 :: timestamptz))
                         ON CONFLICT (provider, id) DO UPDATE
                         SET name = ($3 :: text),
                             summary = ($4 :: text),
                             descr = ($5 :: text),
                             base_url = ($6 :: bytea),
                             auth_tokens = ($7 :: text[]),
                             pubkeys = ($8 :: jsonb),
                             fingerprints = ($9 :: bytea[]),
                             assets = ($10 :: jsonb),
                             tags = ($11 :: bigint[]),
                             enabled = ($12 :: boolean),
                             last_update = ($13 :: timestamptz)
                         WHERE provider_service.last_update <= ($13 :: timestamptz)
        |]

-- | Decodes a list column into a 'NonEmpty', failing the statement when
-- empty (mirrors the strictness of the Cassandra @CqlNonEmpty@ decoder).
nonEmptyE :: Text -> [a] -> Either Text (NonEmpty a)
nonEmptyE what = maybe (Left (what <> ": expected at least one element")) Right . NonEmpty.nonEmpty

lookupProviderServiceRow :: (PGConstraints r) => ProviderId -> ServiceId -> Sem r (Maybe Service)
lookupProviderServiceRow pid sid =
  runStatement (pid, sid) selectProviderService
  where
    selectProviderService ::
      Hasql.Statement
        (ProviderId, ServiceId)
        (Maybe Service)
    selectProviderService =
      Hasql.refineResult
        (traverse (\(name, summary, descr, url, toks, keys, assets, tags, enabled) -> decodeServiceRow sid (name, summary, descr, url, toks, keys, assets, tags, enabled)))
        ( dimapPG
            @(UUID, UUID)
            @(ProviderId, ServiceId)
            @(Maybe (Text, Maybe Text, Text, ByteString, Vector Text, A.Value, A.Value, Vector Int64, Bool))
            [maybeStatement|SELECT (name :: text),
                               (summary :: text?),
                               (descr :: text),
                               (base_url :: bytea),
                               (auth_tokens :: text[]),
                               (pubkeys :: jsonb),
                               (assets :: jsonb),
                               (tags :: bigint[]),
                               (enabled :: boolean)
                        FROM provider_service
                        WHERE provider = ($1 :: uuid) AND id = ($2 :: uuid)
        |]
        )

-- | Builds a 'Service' row value, rejecting empty token/key lists (mirrors
-- the strictness of the Cassandra @CqlNonEmpty@ decoder).
decodeServiceRow ::
  ServiceId ->
  (Name, Maybe Text, Text, HttpsUrl, [ServiceToken], [ServiceKey], [Asset], Set ServiceTag, Bool) ->
  Either Text Service
decodeServiceRow sid (name, summary, descr, url, toks, keys, assets, tags, enabled) =
  Service sid name (fromMaybe mempty summary) descr url
    <$> nonEmptyE "provider_service.auth_tokens" toks
    <*> nonEmptyE "provider_service.pubkeys" keys
    <*> pure assets
    <*> pure tags
    <*> pure enabled

listProviderServiceRows :: (PGConstraints r) => ProviderId -> Sem r [Service]
listProviderServiceRows pid = do
  rows <- runStatement pid selectProviderServices
  pure (map snd (V.toList rows))
  where
    selectProviderServices ::
      Hasql.Statement
        ProviderId
        (Vector (ServiceId, Service))
    selectProviderServices =
      Hasql.refineResult
        (traverse (\(sid, name, summary, descr, url, toks, keys, assets, tags, enabled) -> (sid,) <$> decodeServiceRow sid (name, summary, descr, url, toks, keys, assets, tags, enabled)))
        ( dimapPG
            @UUID
            @ProviderId
            @(Vector (UUID, Text, Maybe Text, Text, ByteString, Vector Text, A.Value, A.Value, Vector Int64, Bool))
            [vectorStatement|SELECT (id :: uuid),
                                (name :: text),
                                (summary :: text?),
                                (descr :: text),
                                (base_url :: bytea),
                                (auth_tokens :: text[]),
                                (pubkeys :: jsonb),
                                (assets :: jsonb),
                                (tags :: bigint[]),
                                (enabled :: boolean)
                         FROM provider_service
                         WHERE provider = ($1 :: uuid)
        |]
        )

-- | The field-level UPDATE of @Brig.Provider.DB.updateService@ plus the
-- tag/prefix index maintenance it performed inside its batch when the service
-- is enabled. Cassandra updates individual columns; here a single COALESCE
-- UPDATE covers the optional fields and the index diff logic is replayed
-- statement-by-statement in one transaction.
updateProviderServiceRow ::
  (PGConstraints r) =>
  ProviderId ->
  ServiceId ->
  Name ->
  RangedServiceTags ->
  Maybe (Name, Name) ->
  Maybe Text ->
  Maybe Text ->
  Maybe [Asset] ->
  Maybe (RangedServiceTags, RangedServiceTags) ->
  Bool ->
  Sem r ()
updateProviderServiceRow pid sid svcName svcTags nameChange summary descr assets tagsChange enabled = do
  runTransaction Tx.Serializable Tx.Write $ do
    Transaction.statement (snd <$> nameChange, summary, descr, assets, pid, sid) updateRowStmt
    -- Mirror DB.updateService: a tag change always writes the tags column;
    -- index maintenance only happens while the service is enabled.
    for_ tagsChange $ \(oldTags, newTags) -> do
      Transaction.statement (fromRange newTags, pid, sid) updateTagsColumn
      when enabled $ case nameChange of
        Just (old, new) -> updateProviderServiceTagsTx pid sid (old, oldTags) (new, newTags)
        Nothing -> updateProviderServiceTagsTx pid sid (svcName, oldTags) (svcName, newTags)
    for_ nameChange $ \(oldName, newName) ->
      when enabled $ do
        updateProviderServicePrefixTx pid sid oldName newName
        updateProviderServiceTagsTx pid sid (oldName, svcTags) (newName, svcTags)
  where
    updateRowStmt ::
      Hasql.Statement
        (Maybe Name, Maybe Text, Maybe Text, Maybe [Asset], ProviderId, ServiceId)
        ()
    updateRowStmt =
      lmapPG
        [resultlessStatement|UPDATE provider_service
                             SET name = COALESCE(($1 :: text?), name),
                                 summary = COALESCE(($2 :: text?), summary),
                                 descr = COALESCE(($3 :: text?), descr),
                                 assets = COALESCE(($4 :: jsonb?), assets),
                                 last_update = now()
                             WHERE provider = ($5 :: uuid) AND id = ($6 :: uuid)
        |]
    updateTagsColumn :: Hasql.Statement (Set ServiceTag, ProviderId, ServiceId) ()
    updateTagsColumn =
      lmapPG
        @(Vector Int64, UUID, UUID)
        [resultlessStatement|UPDATE provider_service
                             SET tags = ($1 :: bigint[]),
                                 last_update = now()
                             WHERE provider = ($2 :: uuid) AND id = ($3 :: uuid)
        |]

-- | As 'updateProviderServiceTagsPG' but for statements inside a transaction.
updateProviderServiceTagsTx ::
  ProviderId ->
  ServiceId ->
  -- | Name and tags to remove.
  (Name, RangedServiceTags) ->
  -- | Name and tags to add.
  (Name, RangedServiceTags) ->
  Transaction.Transaction ()
updateProviderServiceTagsTx pid sid (oldName, oldTags) (newName, newTags)
  | eqTags && eqNames = pure ()
  | eqNames = do
      let name = oldNameLower
      let added = diffTags newTags oldTags
      let removed = diffTags oldTags newTags
      let retained = unfoldTags (diffTags oldTags removed)
      for_ (nonEmptyTags removed) $ \r ->
        for_ (r `unfoldTagsInto` retained) $ \tag ->
          Transaction.statement (defBucket, tag, name, sid) deleteProviderServiceTagRow
      for_ (nonEmptyTags added) $ \a ->
        for_ (a `unfoldTagsInto` retained) $ \tag ->
          Transaction.statement (defBucket, tag, name, sid, pid) insertProviderServiceTagRow
  | otherwise = do
      for_ (unfoldTags oldTags) $ \tag ->
        Transaction.statement (defBucket, tag, oldNameLower, sid) deleteProviderServiceTagRow
      for_ (unfoldTags newTags) $ \tag ->
        Transaction.statement (defBucket, tag, newNameLower, sid, pid) insertProviderServiceTagRow
  where
    oldNameLower = Name (Text.toLower (fromName oldName))
    newNameLower = Name (Text.toLower (fromName newName))
    eqTags = oldTags == newTags
    eqNames = oldNameLower == newNameLower

deleteProviderServiceRow :: (PGConstraints r) => ProviderId -> ServiceId -> Name -> RangedServiceTags -> Sem r ()
deleteProviderServiceRow pid sid name tags = do
  -- NB: mirrors DB.deleteService: whitelist cleanup first, then the row
  -- itself (so API consumers can retry a half-done delete).
  deleteProviderServiceWhitelistRows Nothing pid sid
  runTransaction Tx.Serializable Tx.Write $ do
    Transaction.statement (pid, sid) deleteRow
    Transaction.statement (mkPrefixIndex name, toLowerName name, sid) deleteProviderServicePrefixRow
    for_ (unfoldTags tags) $ \tag ->
      Transaction.statement (defBucket, tag, toLowerName name, sid) deleteProviderServiceTagRow
  where
    deleteRow :: Hasql.Statement (ProviderId, ServiceId) ()
    deleteRow =
      lmapPG
        [resultlessStatement|DELETE FROM provider_service
                             WHERE provider = ($1 :: uuid) AND id = ($2 :: uuid)
        |]

--------------------------------------------------------------------------------
-- Service profiles

serviceProfileFromRow ::
  ServiceId ->
  ProviderId ->
  (Name, Maybe Text, Text, [Asset], Set ServiceTag, Bool) ->
  ServiceProfile
serviceProfileFromRow sid pid (name, summary, descr, assets, tags, enabled) =
  ServiceProfile sid pid name (fromMaybe mempty summary) descr assets tags enabled

lookupProviderServiceProfileRow :: (PGConstraints r) => ProviderId -> ServiceId -> Sem r (Maybe ServiceProfile)
lookupProviderServiceProfileRow pid sid = do
  mrow <- runStatement (pid, sid) selectProfile
  pure (serviceProfileFromRow sid pid <$> mrow)
  where
    selectProfile ::
      Hasql.Statement
        (ProviderId, ServiceId)
        (Maybe (Name, Maybe Text, Text, [Asset], Set ServiceTag, Bool))
    selectProfile =
      dimapPG
        @(UUID, UUID)
        @(ProviderId, ServiceId)
        @(Maybe (Text, Maybe Text, Text, A.Value, Vector Int64, Bool))
        [maybeStatement|SELECT (name :: text),
                               (summary :: text?),
                               (descr :: text),
                               (assets :: jsonb),
                               (tags :: bigint[]),
                               (enabled :: boolean)
                        FROM provider_service
                        WHERE provider = ($1 :: uuid) AND id = ($2 :: uuid)
        |]

listProviderServiceProfileRows :: (PGConstraints r) => ProviderId -> Sem r [ServiceProfile]
listProviderServiceProfileRows pid = do
  rows <- runStatement pid selectProfiles
  pure
    ( map
        ( \(sid, name, summary, descr, assets, tags, enabled) ->
            serviceProfileFromRow sid pid (name, summary, descr, assets, tags, enabled)
        )
        (V.toList rows)
    )
  where
    selectProfiles ::
      Hasql.Statement
        ProviderId
        (Vector (ServiceId, Name, Maybe Text, Text, [Asset], Set ServiceTag, Bool))
    selectProfiles =
      dimapPG
        @UUID
        @ProviderId
        @(Vector (UUID, Text, Maybe Text, Text, A.Value, Vector Int64, Bool))
        [vectorStatement|SELECT (id :: uuid),
                                (name :: text),
                                (summary :: text?),
                                (descr :: text),
                                (assets :: jsonb),
                                (tags :: bigint[]),
                                (enabled :: boolean)
                         FROM provider_service
                         WHERE provider = ($1 :: uuid)
        |]

--------------------------------------------------------------------------------
-- Service connection data

lookupProviderServiceConnRow :: (PGConstraints r) => ProviderId -> ServiceId -> Sem r (Maybe ServiceConn)
lookupProviderServiceConnRow pid sid =
  runStatement (pid, sid) selectConn
  where
    selectConn ::
      Hasql.Statement
        (ProviderId, ServiceId)
        (Maybe ServiceConn)
    selectConn =
      Hasql.refineResult
        ( traverse
            ( \(url, toks, fps, ena) ->
                ServiceConn pid sid url
                  <$> nonEmptyE "provider_service.auth_tokens" toks
                  <*> nonEmptyE "provider_service.fingerprints" fps
                  <*> pure ena
            )
        )
        ( dimapPG
            @(UUID, UUID)
            @(ProviderId, ServiceId)
            @(Maybe (ByteString, Vector Text, Vector ByteString, Bool))
            [maybeStatement|SELECT (base_url :: bytea),
                               (auth_tokens :: text[]),
                               (fingerprints :: bytea[]),
                               (enabled :: boolean)
                        FROM provider_service
                        WHERE provider = ($1 :: uuid) AND id = ($2 :: uuid)
        |]
        )

updateProviderServiceConnRow ::
  (PGConstraints r) =>
  ProviderId ->
  ServiceId ->
  Maybe HttpsUrl ->
  Maybe (NonEmpty ServiceToken) ->
  Maybe (NonEmpty (ServiceKey, Fingerprint Rsa)) ->
  Maybe Bool ->
  Sem r ()
updateProviderServiceConnRow pid sid url tokens keys enabled =
  runStatement (url, toks, pks, fps, enabled, pid, sid) updateConn
  where
    (pks, fps) = case keys of
      Nothing -> (Nothing, Nothing)
      Just ks -> (Just (fmap fst . toList $ ks), Just (fmap snd . toList $ ks))
    toks = toList <$> tokens

    updateConn ::
      Hasql.Statement
        ( Maybe HttpsUrl,
          Maybe [ServiceToken],
          Maybe [ServiceKey],
          Maybe [Fingerprint Rsa],
          Maybe Bool,
          ProviderId,
          ServiceId
        )
        ()
    updateConn =
      lmapPG
        @(Maybe ByteString, Maybe (Vector Text), Maybe A.Value, Maybe (Vector ByteString), Maybe Bool, UUID, UUID)
        [resultlessStatement|UPDATE provider_service
                             SET base_url = COALESCE(($1 :: bytea?), base_url),
                                 auth_tokens = COALESCE(($2 :: text[]?), auth_tokens),
                                 pubkeys = COALESCE(($3 :: jsonb?), pubkeys),
                                 fingerprints = COALESCE(($4 :: bytea[]?), fingerprints),
                                 enabled = COALESCE(($5 :: boolean?), enabled),
                                 last_update = now()
                             WHERE provider = ($6 :: uuid) AND id = ($7 :: uuid)
        |]

--------------------------------------------------------------------------------
-- Index tables

insertProviderServiceTagRow ::
  Hasql.Statement (Bucket, Int64, Name, ServiceId, ProviderId) ()
insertProviderServiceTagRow =
  lmapPG
    [resultlessStatement|INSERT INTO provider_service_tag (bucket, tag, name, service, provider)
                         VALUES ($1 :: int, $2 :: bigint, $3 :: text, $4 :: uuid, $5 :: uuid)
                         ON CONFLICT (bucket, tag, name, service) DO NOTHING
        |]

deleteProviderServiceTagRow :: Hasql.Statement (Bucket, Int64, Name, ServiceId) ()
deleteProviderServiceTagRow =
  lmapPG
    [resultlessStatement|DELETE FROM provider_service_tag
                         WHERE bucket = ($1 :: int) AND tag = ($2 :: bigint)
                           AND name = ($3 :: text) AND service = ($4 :: uuid)
        |]

insertProviderServicePrefixRow :: Hasql.Statement (Text, Name, ServiceId, ProviderId) ()
insertProviderServicePrefixRow =
  lmapPG
    [resultlessStatement|INSERT INTO provider_service_prefix (prefix, name, service, provider)
                         VALUES ($1 :: text, $2 :: text, $3 :: uuid, $4 :: uuid)
                         ON CONFLICT (prefix, name, service) DO NOTHING
        |]

deleteProviderServicePrefixRow :: Hasql.Statement (Text, Name, ServiceId) ()
deleteProviderServicePrefixRow =
  lmapPG
    [resultlessStatement|DELETE FROM provider_service_prefix
                         WHERE prefix = ($1 :: text) AND name = ($2 :: text) AND service = ($3 :: uuid)
        |]

updateProviderServicePrefixTx ::
  ProviderId ->
  ServiceId ->
  -- | Name to remove.
  Name ->
  -- | Name to add.
  Name ->
  Transaction.Transaction ()
updateProviderServicePrefixTx pid sid oldName newName = do
  Transaction.statement (mkPrefixIndex oldName, toLowerName oldName, sid) deleteProviderServicePrefixRow
  Transaction.statement (mkPrefixIndex newName, toLowerName newName, sid, pid) insertProviderServicePrefixRow

--------------------------------------------------------------------------------
-- Whitelist

insertProviderServiceWhitelist :: Hasql.Statement (TeamId, ProviderId, ServiceId) ()
insertProviderServiceWhitelist =
  lmapPG
    [resultlessStatement|INSERT INTO provider_service_whitelist (team, provider, service)
                         VALUES ($1 :: uuid, $2 :: uuid, $3 :: uuid)
                         ON CONFLICT (team, provider, service) DO NOTHING
        |]

insertProviderServiceWhitelistRev :: Hasql.Statement (TeamId, ProviderId, ServiceId) ()
insertProviderServiceWhitelistRev =
  lmapPG
    [resultlessStatement|INSERT INTO provider_service_whitelist_rev (team, provider, service)
                         VALUES ($1 :: uuid, $2 :: uuid, $3 :: uuid)
                         ON CONFLICT (team, provider, service) DO NOTHING
        |]

deleteProviderServiceWhitelistRows :: (PGConstraints r) => Maybe TeamId -> ProviderId -> ServiceId -> Sem r ()
deleteProviderServiceWhitelistRows mbTid pid sid = case mbTid of
  Nothing -> do
    teams <- runStatement (pid, sid) lookupRev
    runTransaction Tx.Serializable Tx.Write $ do
      for_ (V.toList teams) $ \tid -> do
        Transaction.statement (tid, pid, sid) deleteWhitelist1
        Transaction.statement (tid, pid, sid) deleteWhitelist1Rev
      Transaction.statement (pid, sid) deleteAllRev
  Just tid ->
    runTransaction Tx.Serializable Tx.Write $ do
      Transaction.statement (tid, pid, sid) deleteWhitelist1
      Transaction.statement (tid, pid, sid) deleteWhitelist1Rev
  where
    lookupRev :: Hasql.Statement (ProviderId, ServiceId) (Vector TeamId)
    lookupRev =
      dimapPG
        [vectorStatement|SELECT (team :: uuid)
                         FROM provider_service_whitelist_rev
                         WHERE provider = ($1 :: uuid) AND service = ($2 :: uuid)
        |]
    deleteWhitelist1 :: Hasql.Statement (TeamId, ProviderId, ServiceId) ()
    deleteWhitelist1 =
      lmapPG
        [resultlessStatement|DELETE FROM provider_service_whitelist
                             WHERE team = ($1 :: uuid) AND provider = ($2 :: uuid) AND service = ($3 :: uuid)
        |]
    deleteWhitelist1Rev :: Hasql.Statement (TeamId, ProviderId, ServiceId) ()
    deleteWhitelist1Rev =
      lmapPG
        [resultlessStatement|DELETE FROM provider_service_whitelist_rev
                             WHERE team = ($1 :: uuid) AND provider = ($2 :: uuid) AND service = ($3 :: uuid)
        |]
    deleteAllRev :: Hasql.Statement (ProviderId, ServiceId) ()
    deleteAllRev =
      lmapPG
        [resultlessStatement|DELETE FROM provider_service_whitelist_rev
                             WHERE provider = ($1 :: uuid) AND service = ($2 :: uuid)
        |]

selectProviderServiceWhitelistStatus :: Hasql.Statement (TeamId, ProviderId, ServiceId) Bool
selectProviderServiceWhitelistStatus =
  dimapPG
    [singletonStatement|SELECT EXISTS (
                          SELECT 1
                          FROM provider_service_whitelist
                          WHERE team = ($1 :: uuid) AND provider = ($2 :: uuid) AND service = ($3 :: uuid)
                        ) :: bool
    |]

--------------------------------------------------------------------------------
-- Pagination

-- | The merge/trim logic of @Brig.Provider.DB.paginateServiceTags@ (see
-- Note [buggy pagination] there) with per-tag PostgreSQL queries replacing the
-- Cassandra @paginate@ calls.
paginateProviderServiceTags ::
  (PGConstraints r) =>
  QueryAnyTags 1 3 ->
  Maybe Text ->
  Int32 ->
  Maybe ProviderId ->
  Sem r ServiceProfilePage
paginateProviderServiceTags tags start size providerFilter = do
  let size' = size + 1
  let tags' = Set.toList . fromRange . queryAnyTagsRange $ tags
  let start' = maybe "" Text.toLower start
  p0 <- case tags' of
    [] -> pure emptyPage
    [t] -> do
      p <- queryTag start' size' t
      pure $! p {result = trim size (result p)}
    ts -> do
      ps <- traverse (queryTag start' size') ts
      let rows = trim size' (unfoldr nextRow (map result ps))
      let more = any hasMore ps || length rows > fromIntegral size
      pure $! emptyPage {hasMore = more, result = trim size rows}
  let p = filterResults providerFilter start' p0
  r <- traverse (resolveRowPG . thd) (result p)
  -- See Note [buggy pagination]
  pure $! ServiceProfilePage (hasMore p) (catMaybes r)
  where
    queryTag :: (PGConstraints r) => Text -> Int32 -> QueryAllTags 1 3 -> Sem r (Page IndexRow)
    queryTag s l t = do
      let t' = foldTags (queryAllTagsRange t)
      rows <- runStatement (defBucket, t', s, l) selectTagRows
      let rs = V.toList rows
          more = length rs >= fromIntegral l
      pure $! emptyPage {hasMore = more, result = rs}
    selectTagRows :: Hasql.Statement (Bucket, Int64, Text, Int32) (Vector IndexRow)
    selectTagRows =
      dimapPG
        [vectorStatement|SELECT (name :: text), (provider :: uuid), (service :: uuid)
                         FROM provider_service_tag
                         WHERE bucket = ($1 :: int) AND tag = ($2 :: bigint) AND name >= ($3 :: text)
                         ORDER BY name, service
                         LIMIT ($4 :: int)
        |]
    thd (_, pid, sid) = (pid, sid)

resolveRowPG :: (PGConstraints r) => (ProviderId, ServiceId) -> Sem r (Maybe ServiceProfile)
resolveRowPG (pid, sid) = lookupProviderServiceProfileRow pid sid

-- | The merge/trim logic of @Brig.Provider.DB.paginateServiceNames@ (see
-- Note [buggy pagination] there); the Cassandra @queryAll@/@queryPrefixes@
-- scans become ordered @provider_service_prefix@ SELECTs.
paginateProviderServiceNames ::
  (PGConstraints r) =>
  Maybe (Range 1 128 Text) ->
  Int32 ->
  Maybe ProviderId ->
  Sem r ServiceProfilePage
paginateProviderServiceNames mbPrefix size providerFilter = do
  let size' = size + 1
  p <- case mbPrefix of
    Nothing -> filterResults providerFilter "" <$> queryAll size'
    Just prefix ->
      let prefix' = Text.toLower (fromRange prefix)
       in filterResults providerFilter prefix' <$> queryPrefixes prefix' size'
  r <- traverse (resolveRowPG . thd) (result p)
  -- See Note [buggy pagination]
  pure $! ServiceProfilePage (hasMore p) (catMaybes r)
  where
    queryAll :: (PGConstraints r) => Int32 -> Sem r (Page IndexRow)
    queryAll len = do
      rows <- runStatement len selectAllRows
      let rs = V.toList rows
          more = length rs >= fromIntegral len
      pure $! emptyPage {hasMore = more, result = trim size rs}
    selectAllRows :: Hasql.Statement Int32 (Vector IndexRow)
    selectAllRows =
      dimapPG
        [vectorStatement|SELECT (name :: text), (provider :: uuid), (service :: uuid)
                         FROM provider_service_prefix
                         ORDER BY name, service
                         LIMIT ($1 :: int)
        |]
    queryPrefixes :: (PGConstraints r) => Text -> Int32 -> Sem r (Page IndexRow)
    queryPrefixes prefix len = do
      rows <- runStatement (mkPrefixIndex (Name prefix), prefix, len) selectPrefixRows
      let rs = V.toList rows
          more = length rs >= fromIntegral len
      pure $! emptyPage {hasMore = more, result = trim size rs}
    selectPrefixRows :: Hasql.Statement (Text, Text, Int32) (Vector IndexRow)
    selectPrefixRows =
      dimapPG
        [vectorStatement|SELECT (name :: text), (provider :: uuid), (service :: uuid)
                         FROM provider_service_prefix
                         WHERE prefix = ($1 :: text) AND name >= ($2 :: text)
                         ORDER BY name, service
                         LIMIT ($3 :: int)
        |]
    thd (_, pid, sid) = (pid, sid)

-- | The inefficient-but-correct all-services scan of
-- @Brig.Provider.DB.paginateServiceWhitelist@.
paginateProviderServiceWhitelist ::
  (PGConstraints r) =>
  TeamId ->
  Maybe (Range 1 128 Text) ->
  Bool ->
  Int32 ->
  Sem r ServiceProfilePage
paginateProviderServiceWhitelist tid mbPrefix filterDisabled size = do
  pids <- runStatement tid selectWhitelist
  profiles <- catMaybes <$> traverse (uncurry lookupProviderServiceProfileRow) (V.toList pids)
  let r =
        maybeFilterPrefix
          . sortOn (Text.toLower . fromName . serviceProfileName)
          . maybeFilterDisabled
          $ profiles
  pure $!
    ServiceProfilePage
      (length r > fromIntegral size)
      (trim size r)
  where
    selectWhitelist :: Hasql.Statement TeamId (Vector (ProviderId, ServiceId))
    selectWhitelist =
      dimapPG
        [vectorStatement|SELECT (provider :: uuid), (service :: uuid)
                         FROM provider_service_whitelist
                         WHERE team = ($1 :: uuid)
        |]
    maybeFilterDisabled
      | filterDisabled = filter serviceProfileEnabled
      | otherwise = id
    maybeFilterPrefix
      | Just prefix <- mbPrefix =
          let prefix' = Text.toLower (fromRange prefix)
           in filter ((prefix' `Text.isPrefixOf`) . Text.toLower . fromName . serviceProfileName)
      | otherwise = id

--------------------------------------------------------------------------------
