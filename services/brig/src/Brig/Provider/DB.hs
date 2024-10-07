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

module Brig.Provider.DB where

import Brig.Types.Instances ()
import Brig.Types.Provider.Tag
import Cassandra as C
import Control.Arrow ((&&&))
import Data.Id
import Data.List1 (List1)
import Data.Misc
import Data.Range (Range, fromRange, rcast, rnil)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Imports
import UnliftIO (mapConcurrently)
import Wire.API.Password
import Wire.API.Provider
import Wire.API.Provider.Service hiding (updateServiceTags)
import Wire.API.Provider.Service.Tag
import Wire.API.User
import Wire.UserKeyStore

type RangedServiceTags = Range 0 3 (Set.Set ServiceTag)

--------------------------------------------------------------------------------
-- Providers

insertAccount ::
  (MonadClient m) =>
  Name ->
  Password ->
  HttpsUrl ->
  Text ->
  m ProviderId
insertAccount name pass url descr = do
  pid <- randomId
  retry x5 $ write cql $ params LocalQuorum (pid, name, pass, url, descr)
  pure pid
  where
    cql :: PrepQuery W (ProviderId, Name, Password, HttpsUrl, Text) ()
    cql = "INSERT INTO provider (id, name, password, url, descr) VALUES (?, ?, ?, ?, ?)"

updateAccountProfile ::
  (MonadClient m) =>
  ProviderId ->
  Maybe Name ->
  Maybe HttpsUrl ->
  Maybe Text ->
  m ()
updateAccountProfile p name url descr = retry x5 . batch $ do
  setType BatchUnLogged
  setConsistency LocalQuorum
  for_ name $ \x -> addPrepQuery cqlName (x, p)
  for_ url $ \x -> addPrepQuery cqlUrl (x, p)
  for_ descr $ \x -> addPrepQuery cqlDescr (x, p)
  where
    cqlName :: PrepQuery W (Name, ProviderId) ()
    cqlName = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE provider SET name = ? WHERE id = ?"
    cqlUrl :: PrepQuery W (HttpsUrl, ProviderId) ()
    cqlUrl = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE provider SET url = ? WHERE id = ?"
    cqlDescr :: PrepQuery W (Text, ProviderId) ()
    cqlDescr = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE provider SET descr = ? WHERE id = ?"

-- | Lookup the raw account data of a (possibly unverified) provider.
lookupAccountData ::
  (MonadClient m) =>
  ProviderId ->
  m (Maybe (Name, Maybe EmailAddress, HttpsUrl, Text))
lookupAccountData p = retry x1 $ query1 cql $ params LocalQuorum (Identity p)
  where
    cql :: PrepQuery R (Identity ProviderId) (Name, Maybe EmailAddress, HttpsUrl, Text)
    cql = "SELECT name, email, url, descr FROM provider WHERE id = ?"

lookupAccount ::
  (MonadClient m) =>
  ProviderId ->
  m (Maybe Provider)
lookupAccount p = (>>= mk) <$> lookupAccountData p
  where
    mk :: (Name, Maybe EmailAddress, HttpsUrl, Text) -> Maybe Provider
    mk (_, Nothing, _, _) = Nothing
    mk (n, Just e, u, d) = Just $! Provider p n e u d

lookupAccountProfile ::
  (MonadClient m) =>
  ProviderId ->
  m (Maybe ProviderProfile)
lookupAccountProfile p = fmap ProviderProfile <$> lookupAccount p

deleteAccount ::
  (MonadClient m) =>
  ProviderId ->
  m ()
deleteAccount pid = retry x5 $ write cql $ params LocalQuorum (Identity pid)
  where
    cql :: PrepQuery W (Identity ProviderId) ()
    cql = "DELETE FROM provider WHERE id = ?"

updateAccountPassword ::
  (MonadClient m) =>
  ProviderId ->
  PlainTextPassword6 ->
  m ()
updateAccountPassword pid pwd = do
  p <- liftIO $ mkSafePassword pwd
  retry x5 $ write cql $ params LocalQuorum (p, pid)
  where
    cql :: PrepQuery W (Password, ProviderId) ()
    cql = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE provider SET password = ? where id = ?"

--------------------------------------------------------------------------------
-- Unique (Natural) Keys

insertKey ::
  (MonadClient m) =>
  ProviderId ->
  Maybe EmailKey ->
  EmailKey ->
  m ()
insertKey p old new = retry x5 . batch $ do
  setConsistency LocalQuorum
  setType BatchLogged
  for_ old $ \old' -> addPrepQuery cqlKeyDelete (Identity (emailKeyUniq old'))
  addPrepQuery cqlKeyInsert (emailKeyUniq new, p)
  addPrepQuery cqlEmail (emailKeyOrig new, p)
  where
    cqlKeyInsert :: PrepQuery W (Text, ProviderId) ()
    cqlKeyInsert = "INSERT INTO provider_keys (key, provider) VALUES (?, ?)"

    cqlKeyDelete :: PrepQuery W (Identity Text) ()
    cqlKeyDelete = "DELETE FROM provider_keys WHERE key = ?"

    cqlEmail :: PrepQuery W (EmailAddress, ProviderId) ()
    cqlEmail = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE provider SET email = ? WHERE id = ?"

lookupKey ::
  (MonadClient m) =>
  EmailKey ->
  m (Maybe ProviderId)
lookupKey k =
  fmap (fmap runIdentity) $
    retry x1 $
      query1 cql $
        params LocalQuorum (Identity (emailKeyUniq k))
  where
    cql :: PrepQuery R (Identity Text) (Identity ProviderId)
    cql = "SELECT provider FROM provider_keys WHERE key = ?"

deleteKey :: (MonadClient m) => EmailKey -> m ()
deleteKey k = retry x5 $ write cql $ params LocalQuorum (Identity (emailKeyUniq k))
  where
    cql :: PrepQuery W (Identity Text) ()
    cql = "DELETE FROM provider_keys WHERE key = ?"

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
        (Name, Maybe Text, Text, HttpsUrl, List1 ServiceToken, List1 ServiceKey, [Asset], C.Set ServiceTag, Bool)
    cql =
      "SELECT name, summary, descr, base_url, auth_tokens, pubkeys, assets, tags, enabled \
      \FROM service WHERE provider = ? AND id = ?"
    mk (name, summary, descr, url, toks, keys, assets, tags, enabled) =
      Service sid name (fromMaybe mempty summary) descr url toks keys assets (Set.fromList (fromSet tags)) enabled

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
        (ServiceId, Name, Maybe Text, Text, HttpsUrl, List1 ServiceToken, List1 ServiceKey, [Asset], C.Set ServiceTag, Bool)
    cql =
      "SELECT id, name, summary, descr, base_url, auth_tokens, pubkeys, assets, tags, enabled \
      \FROM service WHERE provider = ?"
    mk (sid, name, summary, descr, url, toks, keys, assets, tags, enabled) =
      let tags' = Set.fromList (fromSet tags)
       in Service sid name (fromMaybe mempty summary) descr url toks keys assets tags' enabled

updateService ::
  (MonadClient m) =>
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
  m ()
updateService pid sid svcName svcTags nameChange summary descr assets tagsChange enabled = retry x5 . batch $ do
  setConsistency LocalQuorum
  setType BatchUnLogged
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

data ServiceConn = ServiceConn
  { sconProvider :: !ProviderId,
    sconService :: !ServiceId,
    sconBaseUrl :: !HttpsUrl,
    sconAuthTokens :: !(List1 ServiceToken),
    sconFingerprints :: !(List1 (Fingerprint Rsa)),
    sconEnabled :: !Bool
  }

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
    cql :: PrepQuery R (ProviderId, ServiceId) (HttpsUrl, List1 ServiceToken, List1 (Fingerprint Rsa), Bool)
    cql =
      "SELECT base_url, auth_tokens, fingerprints, enabled \
      \FROM service WHERE provider = ? AND id = ?"
    mk (url, tks, fps, ena) = ServiceConn pid sid url tks fps ena

-- | Update connection information of a service.
updateServiceConn ::
  (MonadClient m) =>
  ProviderId ->
  ServiceId ->
  Maybe HttpsUrl ->
  Maybe (List1 ServiceToken) ->
  Maybe (List1 (ServiceKey, Fingerprint Rsa)) ->
  Maybe Bool ->
  m ()
updateServiceConn pid sid url tokens keys enabled = retry x5 . batch $ do
  setConsistency LocalQuorum
  setType BatchLogged
  for_ url $ \x -> addPrepQuery cqlBaseUrl (x, pid, sid)
  for_ tokens $ \x -> addPrepQuery cqlTokens (x, pid, sid)
  for_ pks $ \x -> addPrepQuery cqlKeys (x, pid, sid)
  for_ fps $ \x -> addPrepQuery cqlFps (x, pid, sid)
  for_ enabled $ \x -> addPrepQuery cqlEnabled (x, pid, sid)
  where
    (pks, fps) = (fmap fst &&& fmap snd) (unzip . toList <$> keys)

    cqlBaseUrl :: PrepQuery W (HttpsUrl, ProviderId, ServiceId) ()
    cqlBaseUrl = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET base_url = ? WHERE provider = ? AND id = ?"
    cqlTokens :: PrepQuery W (List1 ServiceToken, ProviderId, ServiceId) ()
    cqlTokens = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET auth_tokens = ? WHERE provider = ? AND id = ?"
    cqlKeys :: PrepQuery W ([ServiceKey], ProviderId, ServiceId) ()
    cqlKeys = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET pubkeys = ? WHERE provider = ? AND id = ?"
    cqlFps :: PrepQuery W ([Fingerprint Rsa], ProviderId, ServiceId) ()
    cqlFps = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET fingerprints = ? WHERE provider = ? AND id = ?"
    cqlEnabled :: PrepQuery W (Bool, ProviderId, ServiceId) ()
    cqlEnabled = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE service SET enabled = ? WHERE provider = ? AND id = ?"

--------------------------------------------------------------------------------
-- Service "Indexes" (tag and prefix); contain only enabled services

insertServiceIndexes ::
  (MonadClient m) =>
  ProviderId ->
  ServiceId ->
  Name ->
  RangedServiceTags ->
  m ()
insertServiceIndexes pid sid name tags =
  retry x5 . batch $ do
    setConsistency LocalQuorum
    setType BatchLogged
    insertServicePrefix pid sid name
    insertServiceTags pid sid name tags

deleteServiceIndexes ::
  (MonadClient m) =>
  ProviderId ->
  ServiceId ->
  Name ->
  RangedServiceTags ->
  m ()
deleteServiceIndexes pid sid name tags =
  retry x5 . batch $ do
    setConsistency LocalQuorum
    setType BatchLogged
    deleteServicePrefix sid name
    deleteServiceTags pid sid name tags

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

-- Used both by service_tag and service_prefix
type IndexRow = (Name, ProviderId, ServiceId)

-- Note [buggy pagination]
-- ~~~~~~~~~~~~~~~~
--
-- I'm very much unsure that pagination is implemented correctly
-- in 'paginateServiceNames' and 'paginateServiceTags'.
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
paginateServiceTags ::
  (MonadClient m) =>
  QueryAnyTags 1 3 ->
  Maybe Text ->
  Int32 ->
  Maybe ProviderId ->
  m ServiceProfilePage
paginateServiceTags tags start size providerFilter = liftClient $ do
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
    nextRow :: [[IndexRow]] -> Maybe (IndexRow, [[IndexRow]])
    nextRow rs = case mapMaybe uncons rs of
      [] -> Nothing
      hs ->
        let next = fst $ minimumBy (compare `on` fst) hs
            cons (r, rs') = if r == next then rs' else r : rs'
            rest = map cons hs
         in Just (next, rest)
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

paginateServiceNames ::
  (MonadClient m) =>
  Maybe (Range 1 128 Text) ->
  Int32 ->
  Maybe ProviderId ->
  m ServiceProfilePage
paginateServiceNames mbPrefix size providerFilter = liftClient $ do
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
filterResults :: Maybe ProviderId -> Text -> Page IndexRow -> Page IndexRow
filterResults providerFilter start = maybe id filterbyProvider providerFilter . filterPrefix start

filterbyProvider :: ProviderId -> Page IndexRow -> Page IndexRow
filterbyProvider pid p = do
  let filtered = filter (\(_, provider, _) -> pid == provider) (result p)
      -- check if we have filtered out any result
      allValid = length filtered == length (result p)
      more = allValid && hasMore p
   in p {hasMore = more, result = filtered}

filterPrefix :: Text -> Page IndexRow -> Page IndexRow
filterPrefix prefix p = do
  let prefixed = filter (\(Name n, _, _) -> prefix `Text.isPrefixOf` Text.toLower n) (result p)
      -- if they were all valid prefixes, there may be more in Cassandra
      allValid = length prefixed == length (result p)
      more = allValid && hasMore p
   in p {hasMore = more, result = prefixed}

resolveRow :: (MonadClient m) => IndexRow -> m (Maybe ServiceProfile)
resolveRow (_, pid, sid) = lookupServiceProfile pid sid

--------------------------------------------------------------------------------
-- Service whitelist

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

paginateServiceWhitelist ::
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
paginateServiceWhitelist tid mbPrefix filterDisabled size = liftClient $ do
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

mkPrefixIndex :: Name -> Text
mkPrefixIndex = Text.toLower . Text.take 1 . fromName

toLowerName :: Name -> Name
toLowerName = Name . Text.toLower . fromName

trim :: Int32 -> [a] -> [a]
trim = take . fromIntegral
