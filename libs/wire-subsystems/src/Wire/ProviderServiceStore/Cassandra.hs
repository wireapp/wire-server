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

-- | The service sections of @Brig.Provider.DB@ moved here verbatim (CQL
-- strings, consistency levels, batch composition, pagination merge logic) as
-- the Cassandra interpreter of the 'Wire.ProviderServiceStore' effect.
module Wire.ProviderServiceStore.Cassandra
  ( interpretProviderServiceStoreToCassandra,

    -- * Row-level helpers used by "Wire.ProviderServiceStore.Migration"
    selectAllProviderServices,
    selectAllProviderServiceTags,
    selectAllProviderServicePrefixes,
    selectAllProviderServiceWhitelist,
    selectAllProviderServiceWhitelistRev,
  )
where

import Cassandra as C
import Control.Arrow ((&&&))
import Data.Id
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Misc
import Data.Range (Range, fromRange, rcast, rnil)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Imports
import Polysemy
import Polysemy.TinyLog
import UnliftIO (mapConcurrently)
import Wire.API.Provider
import Wire.API.Provider.Service hiding (updateServiceTags)
import Wire.API.Provider.Service.Tag
import Wire.API.User
import Wire.ProviderServiceStore
import Wire.Util

--------------------------------------------------------------------------------
-- Interpreter

interpretProviderServiceStoreToCassandra ::
  ( Member (Embed IO) r,
    Member TinyLog r
  ) =>
  ClientState ->
  Sem (ProviderServiceStore ': r) a ->
  Sem r a
interpretProviderServiceStoreToCassandra cassClient = interpret $ \case
  CreateProviderService pid name summary descr url token key fprint assets tags -> do
    logEffect "ProviderServiceStore.CreateProviderService"
    embedClient cassClient $ insertService pid name summary descr url token key fprint assets tags
  LookupProviderService pid sid -> do
    logEffect "ProviderServiceStore.LookupProviderService"
    embedClient cassClient $ lookupService pid sid
  ListProviderServices pid -> do
    logEffect "ProviderServiceStore.ListProviderServices"
    embedClient cassClient $ listServices pid
  UpdateProviderService pid sid svcName svcTags nameChange summary descr assets tagsChange enabled -> do
    logEffect "ProviderServiceStore.UpdateProviderService"
    embedClient cassClient $
      retry x5 . batch $ do
        setConsistency LocalQuorum
        setType BatchUnLogged
        updateServiceBatch pid sid svcName svcTags nameChange summary descr assets tagsChange enabled
  DeleteProviderService pid sid name tags -> do
    logEffect "ProviderServiceStore.DeleteProviderService"
    embedClient cassClient $ deleteService pid sid name tags
  LookupProviderServiceProfile pid sid -> do
    logEffect "ProviderServiceStore.LookupProviderServiceProfile"
    embedClient cassClient $ lookupServiceProfile pid sid
  ListProviderServiceProfiles pid -> do
    logEffect "ProviderServiceStore.ListProviderServiceProfiles"
    embedClient cassClient $ listServiceProfiles pid
  LookupProviderServiceConn pid sid -> do
    logEffect "ProviderServiceStore.LookupProviderServiceConn"
    embedClient cassClient $ lookupServiceConn pid sid
  UpdateProviderServiceConn pid sid url tokens keys enabled -> do
    logEffect "ProviderServiceStore.UpdateProviderServiceConn"
    embedClient cassClient $ updateServiceConn pid sid url tokens keys enabled
  InsertProviderServiceIndexes pid sid name tags -> do
    logEffect "ProviderServiceStore.InsertProviderServiceIndexes"
    embedClient cassClient $
      retry x5 . batch $ do
        setConsistency LocalQuorum
        setType BatchLogged
        insertServicePrefix pid sid name
        insertServiceTags pid sid name tags
  DeleteProviderServiceIndexes pid sid name tags -> do
    logEffect "ProviderServiceStore.DeleteProviderServiceIndexes"
    embedClient cassClient $
      retry x5 . batch $ do
        setConsistency LocalQuorum
        setType BatchLogged
        deleteServicePrefix sid name
        deleteServiceTags pid sid name tags
  PaginateProviderServiceTags tags start size providerFilter -> do
    logEffect "ProviderServiceStore.PaginateProviderServiceTags"
    embedClient cassClient (paginateProviderServiceTagsImpl tags start size providerFilter)
  PaginateProviderServiceNames mbPrefix size providerFilter -> do
    logEffect "ProviderServiceStore.PaginateProviderServiceNames"
    embedClient cassClient (paginateProviderServiceNamesImpl mbPrefix size providerFilter)
  InsertProviderServiceWhitelist tid pid sid -> do
    logEffect "ProviderServiceStore.InsertProviderServiceWhitelist"
    embedClient cassClient $ insertServiceWhitelist tid pid sid
  DeleteProviderServiceWhitelist mbTid pid sid -> do
    logEffect "ProviderServiceStore.DeleteProviderServiceWhitelist"
    embedClient cassClient $ deleteServiceWhitelist mbTid pid sid
  PaginateProviderServiceWhitelist tid mbPrefix filterDisabled size -> do
    logEffect "ProviderServiceStore.PaginateProviderServiceWhitelist"
    embedClient cassClient (paginateServiceWhitelistImpl tid mbPrefix filterDisabled size)
  GetProviderServiceWhitelistStatus tid pid sid -> do
    logEffect "ProviderServiceStore.GetProviderServiceWhitelistStatus"
    embedClient cassClient $ getServiceWhitelistStatus tid pid sid

--------------------------------------------------------------------------------
-- Services

insertService ::
  (MonadClient m) =>
  ProviderId ->
  Name ->
  Text ->
  Text ->
  HttpsUrl ->
  ServiceToken ->
  ServiceKey ->
  Fingerprint Rsa ->
  [Asset] ->
  Set.Set ServiceTag ->
  m ServiceId
insertService pid name summary descr url token key fprint assets tags = do
  sid <- randomId
  let tagSet = C.Set (Set.toList tags)
  retry x5 $
    write cql $
      params
        LocalQuorum
        (pid, sid, name, summary, descr, url, [token], [key], [fprint], assets, tagSet, False)
  pure sid
  where
    cql ::
      PrepQuery
        W
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
          C.Set ServiceTag,
          Bool
        )
        ()
    cql =
      "INSERT INTO service (provider, id, name, summary, descr, base_url, auth_tokens, \
      \pubkeys, fingerprints, assets, tags, enabled) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

lookupService ::
  (MonadClient m) =>
  ProviderId ->
  ServiceId ->
  m (Maybe Service)
lookupService pid sid =
  fmap (fmap mk) $
    retry x1 $
      query1 cql $
        params LocalQuorum (pid, sid)
  where
    cql ::
      PrepQuery
        R
        (ProviderId, ServiceId)
        (Name, Maybe Text, Text, HttpsUrl, CqlNonEmpty ServiceToken, CqlNonEmpty ServiceKey, [Asset], C.Set ServiceTag, Bool)
    cql =
      "SELECT name, summary, descr, base_url, auth_tokens, pubkeys, assets, tags, enabled \
      \FROM service WHERE provider = ? AND id = ?"
    mk (name, summary, descr, url, toks, keys, assets, tags, enabled) =
      Service sid name (fromMaybe mempty summary) descr url (toNonEmpty toks) (toNonEmpty keys) assets (Set.fromList (fromSet tags)) enabled

listServices ::
  (MonadClient m) =>
  ProviderId ->
  m [Service]
listServices p =
  fmap (map mk) $
    retry x1 $
      query cql $
        params LocalQuorum (Identity p)
  where
    cql ::
      PrepQuery
        R
        (Identity ProviderId)
        (ServiceId, Name, Maybe Text, Text, HttpsUrl, CqlNonEmpty ServiceToken, CqlNonEmpty ServiceKey, [Asset], C.Set ServiceTag, Bool)
    cql =
      "SELECT id, name, summary, descr, base_url, auth_tokens, pubkeys, assets, tags, enabled \
      \FROM service WHERE provider = ?"
    mk (sid, name, summary, descr, url, toks, keys, assets, tags, enabled) =
      let tags' = Set.fromList (fromSet tags)
       in Service sid name (fromMaybe mempty summary) descr url (toNonEmpty toks) (toNonEmpty keys) assets tags' enabled

-- | The batch body of @Brig.Provider.DB@'s @updateService@, factored out so
-- the interpreter can compose it inside its own batch.
updateServiceBatch ::
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
  BatchM ()
updateServiceBatch pid sid svcName svcTags nameChange summary descr assets tagsChange enabled = do
  -- If there is a name change, update the service name; if enabled, update indexes
  for_ nameChange $ \(oldName, newName) -> do
    addPrepQuery cqlName (newName, pid, sid)
    when enabled $ do
      updateServicePrefix pid sid oldName newName
      updateServiceTags pid sid (oldName, svcTags) (newName, svcTags)
  -- If there is a tag change, update the service tags; if enabled, update indexes
  for_ tagsChange $ \(oldTags, newTags) -> do
    let newTags' = C.Set . Set.toList . fromRange $ newTags
    addPrepQuery cqlTags (newTags', pid, sid)
    when enabled $ case nameChange of
      Just (old, new) -> updateServiceTags pid sid (old, oldTags) (new, newTags)
      Nothing -> updateServiceTags pid sid (svcName, oldTags) (svcName, newTags)
  for_ summary $ \x -> addPrepQuery cqlSummary (x, pid, sid)
  for_ descr $ \x -> addPrepQuery cqlDescr (x, pid, sid)
  for_ assets $ \x -> addPrepQuery cqlAssets (x, pid, sid)
  where
    cqlName :: PrepQuery W (Name, ProviderId, ServiceId) ()
    cqlName = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET name = ? WHERE provider = ? AND id = ?"
    cqlSummary :: PrepQuery W (Text, ProviderId, ServiceId) ()
    cqlSummary = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET summary = ? WHERE provider = ? AND id = ?"
    cqlDescr :: PrepQuery W (Text, ProviderId, ServiceId) ()
    cqlDescr = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET descr = ? WHERE provider = ? AND id = ?"
    cqlAssets :: PrepQuery W ([Asset], ProviderId, ServiceId) ()
    cqlAssets = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET assets = ? WHERE provider = ? AND id = ?"
    cqlTags :: PrepQuery W (C.Set ServiceTag, ProviderId, ServiceId) ()
    cqlTags = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET tags = ? WHERE provider = ? AND id = ?"

-- NB: can take a significant amount of time if many teams were using the service
deleteService ::
  (MonadClient m) =>
  ProviderId ->
  ServiceId ->
  Name ->
  RangedServiceTags ->
  m ()
deleteService pid sid name tags = do
  -- NB: the 'deleteService' endpoint checks for the existence of the
  -- service, so it's nice to do actual service deletion as the last thing
  -- (or as a part of the last batch, in this case) because otherwise API
  -- consumers won't be able to retry a half-done 'deleteService' call.
  deleteServiceWhitelist Nothing pid sid
  retry x5 . batch $ do
    setConsistency LocalQuorum
    setType BatchUnLogged
    addPrepQuery cql (pid, sid)
    deleteServicePrefix sid name
    deleteServiceTags pid sid name tags
  where
    cql :: PrepQuery W (ProviderId, ServiceId) ()
    cql = "DELETE FROM service WHERE provider = ? AND id = ?"

--------------------------------------------------------------------------------
-- Service Profiles

-- | Note: Consistency = One
lookupServiceProfile ::
  (MonadClient m) =>
  ProviderId ->
  ServiceId ->
  m (Maybe ServiceProfile)
lookupServiceProfile p s =
  fmap (fmap mk) $
    retry x1 $
      query1 cql $
        params One (p, s)
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (Name, Maybe Text, Text, [Asset], C.Set ServiceTag, Bool)
    cql =
      "SELECT name, summary, descr, assets, tags, enabled \
      \FROM service WHERE provider = ? AND id = ?"
    mk (name, summary, descr, assets, tags, enabled) =
      let tags' = Set.fromList (fromSet tags)
       in ServiceProfile s p name (fromMaybe mempty summary) descr assets tags' enabled

-- | Note: Consistency = One
listServiceProfiles ::
  (MonadClient m) =>
  ProviderId ->
  m [ServiceProfile]
listServiceProfiles p =
  fmap (map mk) $
    retry x1 $
      query cql $
        params One (Identity p)
  where
    cql ::
      PrepQuery
        R
        (Identity ProviderId)
        (ServiceId, Name, Maybe Text, Text, [Asset], C.Set ServiceTag, Bool)
    cql =
      "SELECT id, name, summary, descr, assets, tags, enabled \
      \FROM service WHERE provider = ?"
    mk (sid, name, summary, descr, assets, tags, enabled) =
      let tags' = Set.fromList (fromSet tags)
       in ServiceProfile sid p name (fromMaybe mempty summary) descr assets tags' enabled

--------------------------------------------------------------------------------
-- Service Connection Data

-- | Lookup the connection information of a service.
lookupServiceConn ::
  (MonadClient m) =>
  ProviderId ->
  ServiceId ->
  m (Maybe ServiceConn)
lookupServiceConn pid sid =
  fmap (fmap mk) $
    retry x1 $
      query1 cql $
        params LocalQuorum (pid, sid)
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (HttpsUrl, CqlNonEmpty ServiceToken, CqlNonEmpty (Fingerprint Rsa), Bool)
    cql =
      "SELECT base_url, auth_tokens, fingerprints, enabled \
      \FROM service WHERE provider = ? AND id = ?"
    mk (url, tks, fps, ena) = ServiceConn pid sid url (toNonEmpty tks) (toNonEmpty fps) ena

-- | Update connection information of a service.
updateServiceConn ::
  (MonadClient m) =>
  ProviderId ->
  ServiceId ->
  Maybe HttpsUrl ->
  Maybe (NonEmpty ServiceToken) ->
  Maybe (NonEmpty (ServiceKey, Fingerprint Rsa)) ->
  Maybe Bool ->
  m ()
updateServiceConn pid sid url tokens keys enabled = retry x5 . batch $ do
  setConsistency LocalQuorum
  setType BatchLogged
  for_ url $ \x -> addPrepQuery cqlBaseUrl (x, pid, sid)
  for_ tokens $ \x -> addPrepQuery cqlTokens (CqlNonEmpty x, pid, sid)
  for_ pks $ \x -> addPrepQuery cqlKeys (x, pid, sid)
  for_ fps $ \x -> addPrepQuery cqlFps (x, pid, sid)
  for_ enabled $ \x -> addPrepQuery cqlEnabled (x, pid, sid)
  where
    (pks, fps) = (fmap fst &&& fmap snd) (unzip . toList <$> keys)

    cqlBaseUrl :: PrepQuery W (HttpsUrl, ProviderId, ServiceId) ()
    cqlBaseUrl = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET base_url = ? WHERE provider = ? AND id = ?"
    cqlTokens :: PrepQuery W (CqlNonEmpty ServiceToken, ProviderId, ServiceId) ()
    cqlTokens = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET auth_tokens = ? WHERE provider = ? AND id = ?"
    cqlKeys :: PrepQuery W ([ServiceKey], ProviderId, ServiceId) ()
    cqlKeys = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET pubkeys = ? WHERE provider = ? AND id = ?"
    cqlFps :: PrepQuery W ([Fingerprint Rsa], ProviderId, ServiceId) ()
    cqlFps = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET fingerprints = ? WHERE provider = ? AND id = ?"
    cqlEnabled :: PrepQuery W (Bool, ProviderId, ServiceId) ()
    cqlEnabled = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET enabled = ? WHERE provider = ? AND id = ?"

--------------------------------------------------------------------------------
-- Service "Indexes" (tag and prefix); contain only enabled services

--------------------------------------------------------------------------------
-- Service Tag "Index"

insertServiceTags ::
  ProviderId ->
  ServiceId ->
  Name ->
  RangedServiceTags ->
  BatchM ()
insertServiceTags pid sid name tags =
  updateServiceTags
    pid
    sid
    (Name "", rcast rnil)
    (name, tags)

deleteServiceTags ::
  ProviderId ->
  ServiceId ->
  Name ->
  RangedServiceTags ->
  BatchM ()
deleteServiceTags pid sid name tags =
  updateServiceTags
    pid
    sid
    (name, tags)
    (Name "", rcast rnil)

updateServiceTags ::
  ProviderId ->
  ServiceId ->
  -- | Name and tags to remove.
  (Name, RangedServiceTags) ->
  -- | Name and tags to add.
  (Name, RangedServiceTags) ->
  BatchM ()
updateServiceTags pid sid (oldName, oldTags) (newName, newTags)
  | eqTags && eqNames = pure ()
  | eqNames = do
      let name = oldNameLower
      let added = diffTags newTags oldTags
      let removed = diffTags oldTags newTags
      let retained = unfoldTags (diffTags oldTags removed)
      for_ (nonEmptyTags removed) $ \r ->
        deleteTags name (r `unfoldTagsInto` retained)
      for_ (nonEmptyTags added) $ \a ->
        insertTags name (a `unfoldTagsInto` retained)
  | otherwise = do
      deleteTags oldNameLower (unfoldTags oldTags)
      insertTags newNameLower (unfoldTags newTags)
  where
    oldNameLower = Name (Text.toLower (fromName oldName))
    newNameLower = Name (Text.toLower (fromName newName))
    eqTags = oldTags == newTags
    eqNames = oldNameLower == newNameLower
    deleteTags name = mapM_ $ \tag ->
      addPrepQuery cqlDelete (defBucket, tag, name, sid)
    insertTags name = mapM_ $ \tag ->
      addPrepQuery cqlInsert (defBucket, tag, name, sid, pid)
    cqlDelete :: PrepQuery W (Bucket, Int64, Name, ServiceId) ()
    cqlDelete = "DELETE FROM service_tag WHERE bucket = ? AND tag = ? AND name = ? AND service = ?"
    cqlInsert :: PrepQuery W (Bucket, Int64, Name, ServiceId, ProviderId) ()
    cqlInsert =
      "INSERT INTO service_tag (bucket, tag, name, service, provider) \
      \VALUES (?, ?, ?, ?, ?)"

-- Note [buggy pagination]
-- ~~~~~~~~~~~~~~~~
--
-- I'm very much unsure that pagination is implemented correctly
-- in 'paginateProviderServiceNamesImpl' and 'paginateProviderServiceTagsImpl'.
--
-- It's not obvious that it's enough to pass (size+1) and then try to figure
-- out, in various places, whether we should flip 'hasMore' or not; and if
-- this is implemented incorrectly, then hasMore might be false while it
-- should be true – which will become a significant bug once clients
-- actually start implementing pagination.
--
-- In addition, we certainly return less rows than we are asked for (though
-- it's not exactly a bug), and we can even return zero rows and say "but we
-- have more!".
--
-- Luckily, clients never look at hasMore. There are also some tests that
-- break if they look at hasMore, but since hasMore is currently irrelevant,
-- they're commented out. Grep for references to this note.

-- | Note: Consistency = One
paginateProviderServiceTagsImpl ::
  (MonadClient m) =>
  QueryAnyTags 1 3 ->
  Maybe Text ->
  Int32 ->
  Maybe ProviderId ->
  m ServiceProfilePage
paginateProviderServiceTagsImpl tags start size providerFilter = liftClient $ do
  let size' = size + 1
  let tags' = unpackTags tags
  p <- filterResults providerFilter start' <$> queryAll start' size' tags'
  r <- mapConcurrently resolveRow (result p)
  -- See Note [buggy pagination]
  pure $! ServiceProfilePage (hasMore p) (catMaybes r)
  where
    start' = maybe "" Text.toLower start
    unpackTags :: QueryAnyTags 1 3 -> [QueryAllTags 1 3]
    unpackTags = Set.toList . fromRange . queryAnyTagsRange
    queryAll :: Text -> Int32 -> [QueryAllTags 1 3] -> Client (Page IndexRow)
    queryAll _ _ [] = pure emptyPage
    queryAll s l [t] = do
      p <- queryTags s l t
      pure $! p {result = trim size (result p)}
    queryAll s l ts = do
      ps <- mapConcurrently (queryTags s l) ts
      let rows = trim l (unfoldr nextRow (map result ps))
      let more = any hasMore ps || length rows > fromIntegral size
      pure $! emptyPage {hasMore = more, result = trim size rows}
    queryTags s l t =
      let t' = foldTags (queryAllTagsRange t)
       in retry x1 $ paginate cql $ paramsP One (defBucket, t', s) l
    cql :: PrepQuery R (Bucket, Int64, Text) (Name, ProviderId, ServiceId)
    cql =
      "SELECT name, provider, service FROM service_tag \
      \WHERE bucket = ? AND tag = ? AND name >= ?"

--------------------------------------------------------------------------------
-- Service Prefix "Index"

insertServicePrefix ::
  ProviderId ->
  ServiceId ->
  Name ->
  BatchM ()
insertServicePrefix pid sid name =
  addPrepQuery cql (mkPrefixIndex name, toLowerName name, sid, pid)
  where
    cql :: PrepQuery W (Text, Name, ServiceId, ProviderId) ()
    cql =
      "INSERT INTO service_prefix \
      \(prefix, name, service, provider) \
      \VALUES (?, ?, ?, ?)"

deleteServicePrefix ::
  ServiceId ->
  Name ->
  BatchM ()
deleteServicePrefix sid name =
  addPrepQuery cql (mkPrefixIndex name, toLowerName name, sid)
  where
    cql :: PrepQuery W (Text, Name, ServiceId) ()
    cql =
      "DELETE FROM service_prefix \
      \WHERE prefix = ? AND name = ? AND service = ?"

updateServicePrefix ::
  ProviderId ->
  ServiceId ->
  -- | Name to remove.
  Name ->
  -- | Name to add.
  Name ->
  BatchM ()
updateServicePrefix pid sid oldName newName = do
  deleteServicePrefix sid oldName
  insertServicePrefix pid sid newName

paginateProviderServiceNamesImpl ::
  (MonadClient m) =>
  Maybe (Range 1 128 Text) ->
  Int32 ->
  Maybe ProviderId ->
  m ServiceProfilePage
paginateProviderServiceNamesImpl mbPrefix size providerFilter = liftClient $ do
  let size' = size + 1
  p <- case mbPrefix of
    Nothing ->
      filterResults providerFilter "" <$> queryAll size'
    Just prefix ->
      let prefix' = Text.toLower (fromRange prefix)
       in filterResults providerFilter prefix' <$> queryPrefixes prefix' size'
  r <- mapConcurrently resolveRow (result p)
  -- See Note [buggy pagination]
  pure $! ServiceProfilePage (hasMore p) (catMaybes r)
  where
    queryAll len = do
      let cql :: PrepQuery R () IndexRow
          cql =
            "SELECT name, provider, service \
            \FROM service_prefix"
      p <- retry x1 $ paginate cql $ paramsP One () len
      pure $! p {result = trim size (result p)}
    queryPrefixes prefix len = do
      let cql :: PrepQuery R (Text, Text) IndexRow
          cql =
            "SELECT name, provider, service \
            \FROM service_prefix \
            \WHERE prefix = ? AND name >= ?"
      p <-
        retry x1 $
          paginate cql $
            paramsP One (mkPrefixIndex (Name prefix), prefix) len
      pure $! p {result = trim size (result p)}

-- Pagination utilities
--------------------------------------------------------------------------------
-- Service whitelist

resolveRow :: (MonadClient m) => IndexRow -> m (Maybe ServiceProfile)
resolveRow (_, pid, sid) = lookupServiceProfile pid sid

insertServiceWhitelist :: (MonadClient m) => TeamId -> ProviderId -> ServiceId -> m ()
insertServiceWhitelist tid pid sid =
  retry x5 . batch $ do
    addPrepQuery insert1 (tid, pid, sid)
    addPrepQuery insert1Rev (tid, pid, sid)
  where
    insert1 :: PrepQuery W (TeamId, ProviderId, ServiceId) ()
    insert1 =
      "INSERT INTO service_whitelist \
      \(team, provider, service) \
      \VALUES (?, ?, ?)"
    insert1Rev :: PrepQuery W (TeamId, ProviderId, ServiceId) ()
    insert1Rev =
      "INSERT INTO service_whitelist_rev \
      \(team, provider, service) \
      \VALUES (?, ?, ?)"

--

-- NB: Can take a significant amount of time if many teams were using the service
deleteServiceWhitelist :: (MonadClient m) => Maybe TeamId -> ProviderId -> ServiceId -> m ()
deleteServiceWhitelist mbTid pid sid = case mbTid of
  Nothing -> do
    teams <- retry x5 $ query lookupRev $ params LocalQuorum (pid, sid)
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      addPrepQuery deleteAllRev (pid, sid)
      for_ teams $ \(Identity tid) -> addPrepQuery delete1 (tid, pid, sid)
  Just tid ->
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      addPrepQuery delete1 (tid, pid, sid)
      addPrepQuery delete1Rev (tid, pid, sid)
  where
    lookupRev :: PrepQuery R (ProviderId, ServiceId) (Identity TeamId)
    lookupRev =
      "SELECT team FROM service_whitelist_rev \
      \WHERE provider = ? AND service = ?"
    delete1 :: PrepQuery W (TeamId, ProviderId, ServiceId) ()
    delete1 =
      "DELETE FROM service_whitelist \
      \WHERE team = ? AND provider = ? AND service = ?"
    delete1Rev :: PrepQuery W (TeamId, ProviderId, ServiceId) ()
    delete1Rev =
      "DELETE FROM service_whitelist_rev \
      \WHERE team = ? AND provider = ? AND service = ?"
    deleteAllRev :: PrepQuery W (ProviderId, ServiceId) ()
    deleteAllRev =
      "DELETE FROM service_whitelist_rev \
      \WHERE provider = ? AND service = ?"

--

paginateServiceWhitelistImpl ::
  (MonadClient m) =>
  -- | Team for which to list the services
  TeamId ->
  -- | Prefix
  Maybe (Range 1 128 Text) ->
  -- | Whether to filter out disabled services
  Bool ->
  -- | Page size limit
  Int32 ->
  m ServiceProfilePage
paginateServiceWhitelistImpl tid mbPrefix filterDisabled size = liftClient $ do
  -- NB: this function is rather inefficient because it queries all
  -- services, regardless of 'size'. This is because otherwise we would
  -- have to go through multiple passes of query->filter->query a bit
  -- more->filter->... if we get unlucky.
  p <- retry x1 $ query cql $ params One (Identity tid)
  r <-
    maybeFilterPrefix
      . sortOn (Text.toLower . fromName . serviceProfileName)
      . maybeFilterDisabled
      . catMaybes
      <$> mapConcurrently (uncurry lookupServiceProfile) p
  pure $!
    ServiceProfilePage
      (length r > fromIntegral size)
      (trim size r)
  where
    cql :: PrepQuery R (Identity TeamId) (ProviderId, ServiceId)
    cql =
      "SELECT provider, service \
      \FROM service_whitelist \
      \WHERE team = ?"
    maybeFilterDisabled
      | filterDisabled = filter serviceProfileEnabled
      | otherwise = id
    maybeFilterPrefix
      | Just prefix <- mbPrefix =
          let prefix' = Text.toLower (fromRange prefix)
           in filter ((prefix' `Text.isPrefixOf`) . Text.toLower . fromName . serviceProfileName)
      | otherwise = id

getServiceWhitelistStatus ::
  (MonadClient m) =>
  TeamId ->
  ProviderId ->
  ServiceId ->
  m Bool
getServiceWhitelistStatus tid pid sid = liftClient $ do
  fmap isJust $ retry x1 $ query1 cql $ params One (tid, pid, sid)
  where
    cql :: PrepQuery R (TeamId, ProviderId, ServiceId) (Identity TeamId)
    cql =
      "SELECT team \
      \FROM service_whitelist \
      \WHERE team = ? AND provider = ? AND service = ?"

--------------------------------------------------------------------------------
-- Utilities

newtype CqlNonEmpty a = CqlNonEmpty
  { toNonEmpty :: NonEmpty a
  }
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance (Cql a) => Cql (CqlNonEmpty a) where
  ctype = Tagged (ListColumn (untag (ctype :: Tagged a ColumnType)))

  toCql = CqlList . map toCql . toList

  fromCql (CqlList []) = Left "At least 1 element in list required."
  fromCql (CqlList l) = CqlNonEmpty . NonEmpty.fromList <$> mapM fromCql l
  fromCql _ = Left "Expected CqlList."

--------------------------------------------------------------------------------
-- Full-scan queries for the backfill loops
-- (Wire.ProviderServiceStore.Migration)

-- | Full scan of the @service@ table with per-column writetimes. Brig's
-- service writes are field-granular across @updateService@ /
-- @updateServiceConn@, so no single column is written by every path; the
-- backfill uses the maximum of these writetimes as the row timestamp.
--
-- Only scalar columns can carry @writetime()@: Cassandra rejects selection
-- functions on non-frozen collections (CASSANDRA-14533), so the collection
-- columns (@auth_tokens@, @pubkeys@, @fingerprints@, @assets@, @tags@) are
-- excluded. An underestimated row timestamp for a collection-only mutation
-- just means the backfill replay loses to that mutation's own newer
-- dual-write mirror write, which is the intended guard semantics.
--
-- @summary@ and @descr@ are nullable (added post-launch), so their
-- writetimes decode as 'Maybe' (cf. "Wire.UserStore.IndexUser").
selectAllProviderServices ::
  PrepQuery
    R
    ()
    ( ProviderId,
      ServiceId,
      Maybe Name,
      Maybe Text,
      Maybe Text,
      HttpsUrl,
      [ServiceToken],
      [ServiceKey],
      [Fingerprint Rsa],
      [Asset],
      C.Set ServiceTag,
      Bool,
      Int64,
      Maybe Int64,
      Maybe Int64,
      Int64,
      Int64
    )
selectAllProviderServices =
  "SELECT provider, id, name, summary, descr, base_url, auth_tokens, pubkeys, \
  \fingerprints, assets, tags, enabled, \
  \writetime(name), writetime(summary), writetime(descr), writetime(base_url), \
  \writetime(enabled) \
  \FROM service"

selectAllProviderServiceTags :: PrepQuery R () (Bucket, Int64, Name, ServiceId, ProviderId)
selectAllProviderServiceTags =
  "SELECT bucket, tag, name, service, provider FROM service_tag"

selectAllProviderServicePrefixes :: PrepQuery R () (Text, Name, ServiceId, ProviderId)
selectAllProviderServicePrefixes =
  "SELECT prefix, name, service, provider FROM service_prefix"

selectAllProviderServiceWhitelist :: PrepQuery R () (TeamId, ProviderId, ServiceId)
selectAllProviderServiceWhitelist =
  "SELECT team, provider, service FROM service_whitelist"

selectAllProviderServiceWhitelistRev :: PrepQuery R () (TeamId, ProviderId, ServiceId)
selectAllProviderServiceWhitelistRev =
  "SELECT team, provider, service FROM service_whitelist_rev"
