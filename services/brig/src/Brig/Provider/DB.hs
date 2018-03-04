{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Brig.Provider.DB where

import Brig.Data.Instances ()
import Brig.Email (EmailKey, emailKeyUniq, emailKeyOrig)
import Brig.Password
import Brig.Provider.DB.Instances ()
import Brig.Provider.DB.Tag
import Brig.Types.Common
import Brig.Types.Provider hiding (updateServiceTags)
import Cassandra
import Control.Arrow ((&&&))
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.Functor.Identity
import Data.Id
import Data.Int
import Data.List1 (List1)
import Data.List (unfoldr, minimumBy, uncons)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Misc
import Data.Range (Range, fromRange, rnil, rcast)
import Data.Text (Text, toLower, isPrefixOf)

import qualified Data.Set as Set
import qualified Data.Text as Text

type RangedServiceTags = Range 0 3 (Set.Set ServiceTag)

--------------------------------------------------------------------------------
-- Providers

insertAccount :: MonadClient m
    => Name
    -> Password
    -> HttpsUrl
    -> Text
    -> m ProviderId
insertAccount name pass url descr = do
    pid <- randomId
    retry x5 $ write cql $ params Quorum (pid, name, pass, url, descr)
    return pid
  where
    cql :: PrepQuery W (ProviderId, Name, Password, HttpsUrl, Text) ()
    cql = "INSERT INTO provider (id, name, password, url, descr) VALUES (?, ?, ?, ?, ?)"

updateAccountProfile :: MonadClient m
    => ProviderId
    -> Maybe Name
    -> Maybe HttpsUrl
    -> Maybe Text
    -> m ()
updateAccountProfile p name url descr = retry x5 $ batch $ do
    setType BatchUnLogged
    setConsistency Quorum
    for_ name  $ \x -> addPrepQuery cqlName  (x, p)
    for_ url   $ \x -> addPrepQuery cqlUrl   (x, p)
    for_ descr $ \x -> addPrepQuery cqlDescr (x, p)
  where
    cqlName :: PrepQuery W (Name, ProviderId) ()
    cqlName = "UPDATE provider SET name = ? WHERE id = ?"

    cqlUrl :: PrepQuery W (HttpsUrl, ProviderId) ()
    cqlUrl = "UPDATE provider SET url = ? WHERE id = ?"

    cqlDescr :: PrepQuery W (Text, ProviderId) ()
    cqlDescr = "UPDATE provider SET descr = ? WHERE id = ?"

-- | Lookup the raw account data of a (possibly unverified) provider.
lookupAccountData :: MonadClient m
    => ProviderId
    -> m (Maybe (Name, Maybe Email, HttpsUrl, Text))
lookupAccountData p = retry x1 $ query1 cql $ params Quorum (Identity p)
  where
    cql :: PrepQuery R (Identity ProviderId) (Name, Maybe Email, HttpsUrl, Text)
    cql = "SELECT name, email, url, descr FROM provider WHERE id = ?"

lookupAccount :: MonadClient m
    => ProviderId
    -> m (Maybe Provider)
lookupAccount p = (>>= mk) <$> lookupAccountData p
  where
    mk :: (Name, Maybe Email, HttpsUrl, Text) -> Maybe Provider
    mk (_, Nothing, _, _) = Nothing
    mk (n, Just  e, u, d) = Just $! Provider p n e u d

lookupAccountProfile :: MonadClient m
    => ProviderId
    -> m (Maybe ProviderProfile)
lookupAccountProfile p = fmap ProviderProfile <$> lookupAccount p

lookupPassword :: MonadClient m
    => ProviderId
    -> m (Maybe Password)
lookupPassword p = fmap (fmap runIdentity) $
    retry x1 $ query1 cql $ params Quorum (Identity p)
  where
    cql :: PrepQuery R (Identity ProviderId) (Identity Password)
    cql = "SELECT password FROM provider WHERE id = ?"

deleteAccount :: MonadClient m
    => ProviderId
    -> m ()
deleteAccount pid = retry x5 $ write cql $ params Quorum (Identity pid)
  where
    cql :: PrepQuery W (Identity ProviderId) ()
    cql = "DELETE FROM provider WHERE id = ?"

updateAccountPassword :: MonadClient m
    => ProviderId
    -> PlainTextPassword
    -> m ()
updateAccountPassword pid pwd = do
    p <- liftIO $ mkSafePassword pwd
    retry x5 $ write cql $ params Quorum (p, pid)
  where
    cql :: PrepQuery W (Password, ProviderId) ()
    cql = "UPDATE provider SET password = ? where id = ?"

--------------------------------------------------------------------------------
-- Unique (Natural) Keys

insertKey :: MonadClient m
    => ProviderId
    -> Maybe EmailKey
    -> EmailKey
    -> m ()
insertKey p old new = retry x5 $ batch $ do
    setConsistency Quorum
    setType BatchLogged
    for_ old $ \old' -> addPrepQuery cqlKeyDelete (Identity (emailKeyUniq old'))
    addPrepQuery cqlKeyInsert (emailKeyUniq new, p)
    addPrepQuery cqlEmail     (emailKeyOrig new, p)
  where
    cqlKeyInsert :: PrepQuery W (Text, ProviderId) ()
    cqlKeyInsert = "INSERT INTO provider_keys (key, provider) VALUES (?, ?)"

    cqlKeyDelete :: PrepQuery W (Identity Text) ()
    cqlKeyDelete = "DELETE FROM provider_keys WHERE key = ?"

    cqlEmail :: PrepQuery W (Email, ProviderId) ()
    cqlEmail = "UPDATE provider SET email = ? WHERE id = ?"

lookupKey :: MonadClient m
    => EmailKey
    -> m (Maybe ProviderId)
lookupKey k = fmap (fmap runIdentity) $
    retry x1 $ query1 cql $ params Quorum (Identity (emailKeyUniq k))
  where
    cql :: PrepQuery R (Identity Text) (Identity ProviderId)
    cql = "SELECT provider FROM provider_keys WHERE key = ?"

deleteKey :: MonadClient m => EmailKey -> m ()
deleteKey k = retry x5 $ write cql $ params Quorum (Identity (emailKeyUniq k))
  where
    cql :: PrepQuery W (Identity Text) ()
    cql = "DELETE FROM provider_keys WHERE key = ?"

--------------------------------------------------------------------------------
-- Services

insertService :: MonadClient m
    => ProviderId
    -> Name
    -> Text
    -> Text
    -> HttpsUrl
    -> ServiceToken
    -> ServiceKey
    -> Fingerprint Rsa
    -> [Asset]
    -> Set.Set ServiceTag
    -> m ServiceId
insertService pid name summary descr url token key fprint assets tags = do
    sid <- randomId
    let tagSet = Set (Set.toList tags)
    retry x5 $ write cql $ params Quorum
        (pid, sid, name, summary, descr, url, [token], [key], [fprint], assets, tagSet, False)
    return sid
  where
    cql :: PrepQuery W (ProviderId, ServiceId, Name, Text, Text, HttpsUrl, [ServiceToken],
                        [ServiceKey], [Fingerprint Rsa], [Asset], Set ServiceTag, Bool)
                       ()
    cql = "INSERT INTO service (provider, id, name, summary, descr, base_url, auth_tokens, \
                               \pubkeys, fingerprints, assets, tags, enabled) \
          \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

lookupService :: MonadClient m
    => ProviderId
    -> ServiceId
    -> m (Maybe Service)
lookupService pid sid = fmap (fmap mk) $
    retry x1 $ query1 cql $ params Quorum (pid, sid)
  where
    cql :: PrepQuery R (ProviderId, ServiceId)
                       (Name, Maybe Text, Text, HttpsUrl, List1 ServiceToken, List1 ServiceKey, [Asset], Set ServiceTag, Bool)
    cql = "SELECT name, summary, descr, base_url, auth_tokens, pubkeys, assets, tags, enabled \
          \FROM service WHERE provider = ? AND id = ?"

    mk (name, summary, descr, url, toks, keys, assets, tags, enabled) =
        Service sid name (fromMaybe mempty summary) descr url toks keys assets (Set.fromList (fromSet tags)) enabled

listServices :: MonadClient m
    => ProviderId
    -> m [Service]
listServices p = fmap (map mk) $
    retry x1 $ query cql $ params Quorum (Identity p)
  where
    cql :: PrepQuery R (Identity ProviderId)
                       (ServiceId, Name, Maybe Text, Text, HttpsUrl, List1 ServiceToken, List1 ServiceKey, [Asset], Set ServiceTag, Bool)
    cql = "SELECT id, name, summary, descr, base_url, auth_tokens, pubkeys, assets, tags, enabled \
          \FROM service WHERE provider = ?"

    mk (sid, name, summary, descr, url, toks, keys, assets, tags, enabled) =
        let tags' = Set.fromList (fromSet tags)
        in Service sid name (fromMaybe mempty summary) descr url toks keys assets tags' enabled

updateService :: MonadClient m
    => ProviderId
    -> ServiceId
    -> Name
    -> RangedServiceTags
    -> Maybe (Name, Name)
    -> Maybe Text
    -> Maybe Text
    -> Maybe [Asset]
    -> Maybe (RangedServiceTags, RangedServiceTags)
    -> Bool
    -> m ()
updateService pid sid svcName svcTags nameChange summary descr assets tagsChange enabled = retry x5 $ batch $ do
    setConsistency Quorum
    setType BatchUnLogged
    -- If there is a name change, update the service name; if enabled, update indexes
    for_ nameChange $ \(oldName, newName) -> do
        addPrepQuery cqlName (newName, pid, sid)
        when enabled $ do
            updateServicePrefix pid sid oldName newName
            updateServiceTags   pid sid (oldName, svcTags) (newName, svcTags)
    -- If there is a tag change, update the service tags; if enabled, update indexes
    for_ tagsChange $ \(oldTags, newTags) -> do
        let newTags' = Set . Set.toList . fromRange $ newTags
        addPrepQuery cqlTags (newTags', pid, sid)
        when enabled $ case nameChange of
            Just (old, new) -> updateServiceTags pid sid (old, oldTags)     (new, newTags)
            Nothing         -> updateServiceTags pid sid (svcName, oldTags) (svcName, newTags)

    for_ summary    $ \x -> addPrepQuery cqlSummary  (x, pid, sid)
    for_ descr      $ \x -> addPrepQuery cqlDescr    (x, pid, sid)
    for_ assets     $ \x -> addPrepQuery cqlAssets   (x, pid, sid)
  where
    cqlName :: PrepQuery W (Name, ProviderId, ServiceId) ()
    cqlName = "UPDATE service SET name = ? WHERE provider = ? AND id = ?"

    cqlSummary :: PrepQuery W (Text, ProviderId, ServiceId) ()
    cqlSummary = "UPDATE service SET summary = ? WHERE provider = ? AND id = ?"

    cqlDescr :: PrepQuery W (Text, ProviderId, ServiceId) ()
    cqlDescr = "UPDATE service SET descr = ? WHERE provider = ? AND id = ?"

    cqlAssets :: PrepQuery W ([Asset], ProviderId, ServiceId) ()
    cqlAssets = "UPDATE service SET assets = ? WHERE provider = ? AND id = ?"

    cqlTags :: PrepQuery W (Set ServiceTag, ProviderId, ServiceId) ()
    cqlTags = "UPDATE service SET tags = ? WHERE provider = ? AND id = ?"

deleteService :: MonadClient m
    => ProviderId
    -> ServiceId
    -> Name
    -> RangedServiceTags
    -> m ()
deleteService pid sid name tags =  retry x5 $ batch $ do
    setConsistency Quorum
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
lookupServiceProfile :: MonadClient m
    => ProviderId
    -> ServiceId
    -> m (Maybe ServiceProfile)
lookupServiceProfile p s = fmap (fmap mk) $
    retry x1 $ query1 cql $ params One (p, s)
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (Name, Maybe Text, Text, [Asset], Set ServiceTag, Bool)
    cql = "SELECT name, summary, descr, assets, tags, enabled \
          \FROM service WHERE provider = ? AND id = ?"

    mk (name, summary, descr, assets, tags, enabled) =
        let tags' = Set.fromList (fromSet tags)
        in ServiceProfile s p name (fromMaybe mempty summary) descr assets tags' enabled

-- | Note: Consistency = One
listServiceProfiles :: MonadClient m
    => ProviderId
    -> m [ServiceProfile]
listServiceProfiles p = fmap (map mk) $
    retry x1 $ query cql $ params One (Identity p)
  where
    cql :: PrepQuery R (Identity ProviderId)
                       (ServiceId, Name, Maybe Text, Text, [Asset], Set ServiceTag, Bool)
    cql = "SELECT id, name, summary, descr, assets, tags, enabled \
          \FROM service WHERE provider = ?"

    mk (sid, name, summary, descr, assets, tags, enabled) =
        let tags' = Set.fromList (fromSet tags)
        in ServiceProfile sid p name (fromMaybe mempty summary) descr assets tags' enabled

--------------------------------------------------------------------------------
-- Service Connection Data

data ServiceConn = ServiceConn
    { sconProvider     :: !ProviderId
    , sconService      :: !ServiceId
    , sconBaseUrl      :: !HttpsUrl
    , sconAuthTokens   :: !(List1 ServiceToken)
    , sconFingerprints :: !(List1 (Fingerprint Rsa))
    , sconEnabled      :: !Bool
    }

-- | Lookup the connection information of a service.
lookupServiceConn :: MonadClient m
    => ProviderId
    -> ServiceId
    -> m (Maybe ServiceConn)
lookupServiceConn pid sid = fmap (fmap mk) $
    retry x1 $ query1 cql $ params Quorum (pid, sid)
  where
    cql :: PrepQuery R (ProviderId, ServiceId) (HttpsUrl, List1 ServiceToken, List1 (Fingerprint Rsa), Bool)
    cql = "SELECT base_url, auth_tokens, fingerprints, enabled \
          \FROM service WHERE provider = ? AND id = ?"

    mk (url, tks, fps, ena) = ServiceConn pid sid url tks fps ena

-- | Update connection information of a service.
updateServiceConn :: MonadClient m
    => ProviderId
    -> ServiceId
    -> Maybe HttpsUrl
    -> Maybe (List1 ServiceToken)
    -> Maybe (List1 (ServiceKey, Fingerprint Rsa))
    -> Maybe Bool
    -> m ()
updateServiceConn pid sid url tokens keys enabled = retry x5 $ batch $ do
    setConsistency Quorum
    setType BatchLogged
    for_ url     $ \x -> addPrepQuery cqlBaseUrl (x, pid, sid)
    for_ tokens  $ \x -> addPrepQuery cqlTokens  (x, pid, sid)
    for_ pks     $ \x -> addPrepQuery cqlKeys    (x, pid, sid)
    for_ fps     $ \x -> addPrepQuery cqlFps     (x, pid, sid)
    for_ enabled $ \x -> addPrepQuery cqlEnabled (x, pid, sid)
  where
    (pks, fps) = (fmap fst &&& fmap snd) (unzip . toList <$> keys)

    cqlBaseUrl :: PrepQuery W (HttpsUrl, ProviderId, ServiceId) ()
    cqlBaseUrl = "UPDATE service SET base_url = ? WHERE provider = ? AND id = ?"

    cqlTokens :: PrepQuery W (List1 ServiceToken, ProviderId, ServiceId) ()
    cqlTokens = "UPDATE service SET auth_tokens = ? WHERE provider = ? AND id = ?"

    cqlKeys :: PrepQuery W ([ServiceKey], ProviderId, ServiceId) ()
    cqlKeys = "UPDATE service SET pubkeys = ? WHERE provider = ? AND id = ?"

    cqlFps :: PrepQuery W ([Fingerprint Rsa], ProviderId, ServiceId) ()
    cqlFps = "UPDATE service SET fingerprints = ? WHERE provider = ? AND id = ?"

    cqlEnabled :: PrepQuery W (Bool, ProviderId, ServiceId) ()
    cqlEnabled = "UPDATE service SET enabled = ? WHERE provider = ? AND id = ?"

--------------------------------------------------------------------------------
-- Service "Indexes" (tag and prefix)

insertServiceIndexes :: MonadClient m
                     => ProviderId
                     -> ServiceId
                     -> Name
                     -> RangedServiceTags
                     -> m ()
insertServiceIndexes pid sid name tags =
    retry x5 $ batch $ do
        setConsistency Quorum
        setType BatchLogged
        insertServicePrefix pid sid name
        insertServiceTags pid sid name tags

deleteServiceIndexes :: MonadClient m
                     => ProviderId
                     -> ServiceId
                     -> Name
                     -> RangedServiceTags
                     -> m ()
deleteServiceIndexes pid sid name tags =
    retry x5 $ batch $ do
        setConsistency Quorum
        setType BatchLogged
        deleteServicePrefix sid name
        deleteServiceTags pid sid name tags

updateServiceIndexes :: MonadClient m
                     => ProviderId
                     -> ServiceId
                     -> (Name, RangedServiceTags) -- ^ Name and tags to remove.
                     -> (Name, RangedServiceTags) -- ^ Name and tags to add.
                     -> m ()
updateServiceIndexes pid sid (oldName, oldTags) (newName, newTags) =
    retry x5 $ batch $ do
        setConsistency Quorum
        setType BatchLogged
        updateServicePrefix pid sid oldName newName
        updateServiceTags pid sid (oldName, oldTags) (newName, newTags)

--------------------------------------------------------------------------------
-- Service Tag "Index"

insertServiceTags
    :: ProviderId
    -> ServiceId
    -> Name
    -> RangedServiceTags
    -> BatchM ()
insertServiceTags pid sid name tags = updateServiceTags
    pid sid (Name "", rcast rnil) (name, tags)

deleteServiceTags
    :: ProviderId
    -> ServiceId
    -> Name
    -> RangedServiceTags
    -> BatchM ()
deleteServiceTags pid sid name tags = updateServiceTags
    pid sid (name, tags) (Name "", rcast rnil)

updateServiceTags
    :: ProviderId
    -> ServiceId
    -> (Name, RangedServiceTags) -- ^ Name and tags to remove.
    -> (Name, RangedServiceTags) -- ^ Name and tags to add.
    -> BatchM ()
updateServiceTags pid sid (oldName, oldTags) (newName, newTags)
    | eqTags && eqNames = return ()
    | eqNames = do
        let name     = oldNameLower
        let added    = diffTags newTags oldTags
        let removed  = diffTags oldTags newTags
        let retained = unfoldTags (diffTags oldTags removed)
        for_ (nonEmptyTags removed) $ \r ->
            deleteTags name (r `unfoldTagsInto` retained)
        for_ (nonEmptyTags added) $ \a ->
            insertTags name (a `unfoldTagsInto` retained)
    | otherwise = do
        deleteTags oldNameLower (unfoldTags oldTags)
        insertTags newNameLower (unfoldTags newTags)
  where
    oldNameLower = Name (toLower (fromName oldName))
    newNameLower = Name (toLower (fromName newName))

    eqTags  = oldTags      == newTags
    eqNames = oldNameLower == newNameLower

    deleteTags name = mapM_ $ \tag ->
        addPrepQuery cqlDelete (defBucket, tag, name, sid)

    insertTags name = mapM_ $ \tag ->
        addPrepQuery cqlInsert (defBucket, tag, name, sid, pid)

    cqlDelete :: PrepQuery W (Bucket, Int64, Name, ServiceId) ()
    cqlDelete = "DELETE FROM service_tag WHERE bucket = ? AND tag = ? AND name = ? AND service = ?"

    cqlInsert :: PrepQuery W (Bucket, Int64, Name, ServiceId, ProviderId) ()
    cqlInsert = "INSERT INTO service_tag (bucket, tag, name, service, provider) \
                \VALUES (?, ?, ?, ?, ?)"

-- Used both by service_tag and service_prefix
type IndexRow = (Name, ProviderId, ServiceId)

-- | Note: Consistency = One
paginateServiceTags :: MonadClient m
    => QueryAnyTags 1 3
    -> Maybe Text
    -> Int32
    -> Maybe ProviderId
    -> m ServiceProfilePage
paginateServiceTags tags start size providerFilter = liftClient $ do
    let size' = size + 1
    let tags' = unpackTags tags
    p <- filterResults providerFilter start' <$> queryAll start' size' tags'
    r <- mapConcurrently resolveRow (result p)
    return $! ServiceProfilePage (hasMore p) (catMaybes r)
  where
    start' = maybe "" toLower start

    unpackTags :: QueryAnyTags 1 3 -> [QueryAllTags 1 3]
    unpackTags = Set.toList . fromRange . queryAnyTagsRange

    queryAll :: Text -> Int32 -> [QueryAllTags 1 3] -> Client (Page IndexRow)
    queryAll _ _  [] = return emptyPage
    queryAll s l [t] = do
        p <- queryTags s l t
        return $! p { result = trim size (result p) }
    queryAll s l ts  = do
        ps <- mapConcurrently (queryTags s l) ts
        let rows = trim l (unfoldr nextRow (map result ps))
        let more = any hasMore ps || length rows > fromIntegral size
        return $! emptyPage { hasMore = more, result = trim size rows }

    nextRow :: [[IndexRow]] -> Maybe (IndexRow, [[IndexRow]])
    nextRow rs = case mapMaybe uncons rs of
        [] -> Nothing
        hs -> let next = fst $ minimumBy (compare `on` fst) hs
                  cons (r, rs') = if r == next then rs' else r:rs'
                  rest = map cons hs
              in Just (next, rest)

    queryTags s l t =
        let t' = foldTags (queryAllTagsRange t)
        in retry x1 $ paginate cql $ paramsP One (defBucket, t', s) l

    cql :: PrepQuery R (Bucket, Int64, Text) (Name, ProviderId, ServiceId)
    cql = "SELECT name, provider, service FROM service_tag \
          \WHERE bucket = ? AND tag = ? AND name >= ?"

--------------------------------------------------------------------------------
-- Service Prefix "Index"

insertServicePrefix
    :: ProviderId
    -> ServiceId
    -> Name
    -> BatchM ()
insertServicePrefix pid sid name =
    addPrepQuery cql (mkPrefixIndex name, toLowerName name, sid, pid)
  where
    cql :: PrepQuery W (Text, Name, ServiceId, ProviderId) ()
    cql = "INSERT INTO service_prefix \
          \(prefix, name, service, provider) \
          \VALUES (?, ?, ?, ?)"

deleteServicePrefix
    :: ServiceId
    -> Name
    -> BatchM ()
deleteServicePrefix sid name =
    addPrepQuery cql (mkPrefixIndex name, toLowerName name, sid)
  where
    cql :: PrepQuery W (Text, Name, ServiceId) ()
    cql = "DELETE FROM service_prefix \
          \WHERE prefix = ? AND name = ? AND service = ?"

updateServicePrefix
    :: ProviderId
    -> ServiceId
    -> Name -- ^ Name to remove.
    -> Name -- ^ Name to add.
    -> BatchM ()
updateServicePrefix pid sid oldName newName = do
    deleteServicePrefix sid oldName
    insertServicePrefix pid sid newName

paginateServiceNames :: MonadClient m
    => Range 1 128 Text
    -> Int32
    -> Maybe ProviderId
    -> m ServiceProfilePage
paginateServiceNames start size providerFilter = liftClient $ do
    let size' = size + 1
    p <- filterResults providerFilter prefix <$> queryPrefixes size'
    r <- mapConcurrently resolveRow (result p)
    return $! ServiceProfilePage (hasMore p) (catMaybes r)
  where
    prefix = toLower (fromRange start)

    queryPrefixes len = do
        p <- retry x1 $ paginate cql $ paramsP One (mkPrefixIndex (Name prefix), prefix) len
        return $! p { result = trim size (result p) }

    cql :: PrepQuery R (Text, Text) IndexRow
    cql = "SELECT name, provider, service \
          \FROM service_prefix \
          \WHERE prefix = ? AND name >= ?"

-- Pagination utilities
filterResults :: Maybe ProviderId -> Text -> Page IndexRow -> Page IndexRow
filterResults providerFilter start = maybe id filterbyProvider providerFilter . filterPrefix start

filterbyProvider :: ProviderId -> Page IndexRow -> Page IndexRow
filterbyProvider pid p = do
    let filtered = filter (\(_, provider, _) -> pid == provider) (result p)
        -- check if we have filtered out any result
        allValid = length filtered == length (result p)
        more     = allValid && hasMore p
     in p { hasMore = more, result = filtered }

filterPrefix :: Text -> Page IndexRow -> Page IndexRow
filterPrefix prefix p = do
    let prefixed = filter (\(Name n, _, _) -> prefix `isPrefixOf` (toLower n)) (result p)
        -- if they were all valid prefixes, there may be more in Cassandra
        allValid = length prefixed == length (result p)
        more     = allValid && hasMore p
     in p { hasMore = more, result = prefixed }

resolveRow :: MonadClient m => IndexRow -> m (Maybe ServiceProfile)
resolveRow (_, pid, sid) = lookupServiceProfile pid sid

--------------------------------------------------------------------------------
-- Utilities
mkPrefixIndex :: Name -> Text
mkPrefixIndex = Text.toLower . Text.take 1 . fromName

toLowerName :: Name -> Name
toLowerName = Name . toLower . fromName

trim :: Int32 -> [a] -> [a]
trim = take . fromIntegral
