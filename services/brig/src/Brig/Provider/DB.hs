{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Brig.Provider.DB where

import Brig.Data.Instances ()
import Brig.Email (EmailKey, emailKeyUniq, emailKeyOrig)
import Brig.Password
import Brig.Provider.DB.Instances ()
import Brig.Provider.DB.Tag
import Brig.Types.Common
import Brig.Types.Provider
import Cassandra
import Control.Arrow ((&&&))
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.Functor.Identity
import Data.Id
import Data.Int
import Data.List1 (List1)
import Data.List (unfoldr, minimumBy, uncons)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Misc (Fingerprint, Rsa)
import Data.Range (Range, fromRange, rnil, rcast)
import Data.Text (Text, toLower, isPrefixOf)

import qualified Data.Set as Set

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

--------------------------------------------------------------------------------
-- Unique (Natural) Keys

insertKey :: MonadClient m
    => ProviderId
    -> EmailKey
    -> m ()
insertKey p k = retry x5 $ batch $ do
    setConsistency Quorum
    setType BatchLogged
    addPrepQuery cqlKey   (emailKeyUniq k, p)
    addPrepQuery cqlEmail (emailKeyOrig k, p)
  where
    cqlKey :: PrepQuery W (Text, ProviderId) ()
    cqlKey = "INSERT INTO provider_keys (key, provider) VALUES (?, ?)"

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
    -> HttpsUrl
    -> ServiceToken
    -> ServiceKey
    -> Fingerprint Rsa
    -> [Asset]
    -> Set.Set ServiceTag
    -> m ServiceId
insertService pid name descr url token key fprint assets tags = do
    sid <- randomId
    let tagSet = Set (Set.toList tags)
    retry x5 $ write cql $ params Quorum
        (pid, sid, name, descr, url, [token], [key], [fprint], assets, tagSet, False)
    return sid
  where
    cql :: PrepQuery W (ProviderId, ServiceId, Name, Text, HttpsUrl, [ServiceToken],
                        [ServiceKey], [Fingerprint Rsa], [Asset], Set ServiceTag, Bool)
                       ()
    cql = "INSERT INTO service (provider, id, name, descr, base_url, auth_tokens, \
                               \pubkeys, fingerprints, assets, tags, enabled) \
          \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

lookupService :: MonadClient m
    => ProviderId
    -> ServiceId
    -> m (Maybe Service)
lookupService pid sid = fmap (fmap mk) $
    retry x1 $ query1 cql $ params Quorum (pid, sid)
  where
    cql :: PrepQuery R (ProviderId, ServiceId)
                       (Name, Text, HttpsUrl, List1 ServiceToken, List1 ServiceKey, [Asset], Set ServiceTag, Bool)
    cql = "SELECT name, descr, base_url, auth_tokens, pubkeys, assets, tags, enabled \
          \FROM service WHERE provider = ? AND id = ?"

    mk (name, descr, url, toks, keys, assets, tags, enabled) =
        Service sid name descr url toks keys assets (Set.fromList (fromSet tags)) enabled

listServices :: MonadClient m
    => ProviderId
    -> m [Service]
listServices p = fmap (map mk) $
    retry x1 $ query cql $ params Quorum (Identity p)
  where
    cql :: PrepQuery R (Identity ProviderId)
                       (ServiceId, Name, Text, HttpsUrl, List1 ServiceToken, List1 ServiceKey, [Asset], Set ServiceTag, Bool)
    cql = "SELECT id, name, descr, base_url, auth_tokens, pubkeys, assets, tags, enabled \
          \FROM service WHERE provider = ?"

    mk (sid, name, descr, url, toks, keys, assets, tags, enabled) =
        let tags' = Set.fromList (fromSet tags)
        in Service sid name descr url toks keys assets tags' enabled

updateService :: MonadClient m
    => ProviderId
    -> ServiceId
    -> Maybe Name
    -> Maybe Text
    -> Maybe [Asset]
    -> Maybe (Range 1 3 (Set.Set ServiceTag))
    -> m ()
updateService pid sid name descr assets tags = retry x5 $ batch $ do
    setConsistency Quorum
    setType BatchUnLogged
    let tags' = Set . Set.toList . fromRange <$> tags
    for_ name   $ \x -> addPrepQuery cqlName     (x, pid, sid)
    for_ descr  $ \x -> addPrepQuery cqlDescr    (x, pid, sid)
    for_ assets $ \x -> addPrepQuery cqlAssets   (x, pid, sid)
    for_ tags'  $ \x -> addPrepQuery cqlTags     (x, pid, sid)
  where
    cqlName :: PrepQuery W (Name, ProviderId, ServiceId) ()
    cqlName = "UPDATE service SET name = ? WHERE provider = ? AND id = ?"

    cqlDescr :: PrepQuery W (Text, ProviderId, ServiceId) ()
    cqlDescr = "UPDATE service SET descr = ? WHERE provider = ? AND id = ?"

    cqlAssets :: PrepQuery W ([Asset], ProviderId, ServiceId) ()
    cqlAssets = "UPDATE service SET assets = ? WHERE provider = ? AND id = ?"

    cqlTags :: PrepQuery W (Set ServiceTag, ProviderId, ServiceId) ()
    cqlTags = "UPDATE service SET tags = ? WHERE provider = ? AND id = ?"

deleteService :: MonadClient m
    => ProviderId
    -> ServiceId
    -> m ()
deleteService pid sid =
    retry x5 $ write cql $ params Quorum (pid, sid)
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
    cql :: PrepQuery R (ProviderId, ServiceId) (Name, Text, [Asset], Set ServiceTag, Bool)
    cql = "SELECT name, descr, assets, tags, enabled \
          \FROM service WHERE provider = ? AND id = ?"

    mk (name, descr, assets, tags, enabled) =
        let tags' = Set.fromList (fromSet tags)
        in ServiceProfile s p name descr assets tags' enabled

-- | Note: Consistency = One
listServiceProfiles :: MonadClient m
    => ProviderId
    -> m [ServiceProfile]
listServiceProfiles p = fmap (map mk) $
    retry x1 $ query cql $ params One (Identity p)
  where
    cql :: PrepQuery R (Identity ProviderId)
                       (ServiceId, Name, Text, [Asset], Set ServiceTag, Bool)
    cql = "SELECT id, name, descr, assets, tags, enabled \
          \FROM service WHERE provider = ?"

    mk (sid, name, descr, assets, tags, enabled) =
        let tags' = Set.fromList (fromSet tags)
        in ServiceProfile sid p name descr assets tags' enabled

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
-- Service Tag "Index"

insertServiceTags :: MonadClient m
    => ProviderId
    -> ServiceId
    -> Name
    -> Range 0 3 (Set.Set ServiceTag)
    -> m ()
insertServiceTags pid sid name tags = Brig.Provider.DB.updateServiceTags
    pid sid (Name "", rcast rnil) (name, tags)

deleteServiceTags :: MonadClient m
    => ProviderId
    -> ServiceId
    -> Name
    -> Range 0 3 (Set.Set ServiceTag)
    -> m ()
deleteServiceTags pid sid name tags = Brig.Provider.DB.updateServiceTags
    pid sid (name, tags) (Name "", rcast rnil)

updateServiceTags :: MonadClient m
    => ProviderId
    -> ServiceId
    -> (Name, Range 0 3 (Set.Set ServiceTag)) -- ^ Name and tags to remove.
    -> (Name, Range 0 3 (Set.Set ServiceTag)) -- ^ Name and tags to add.
    -> m ()
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
        retry x5 $ write cqlDelete $ params Quorum (defBucket, tag, name, sid)

    insertTags name = mapM_ $ \tag ->
        retry x5 $ write cqlInsert $ params Quorum (defBucket, tag, name, sid, pid)

    cqlDelete :: PrepQuery W (Bucket, Int64, Name, ServiceId) ()
    cqlDelete = "DELETE FROM service_tag WHERE bucket = ? AND tag = ? AND name = ? AND service = ?"

    cqlInsert :: PrepQuery W (Bucket, Int64, Name, ServiceId, ProviderId) ()
    cqlInsert = "INSERT INTO service_tag (bucket, tag, name, service, provider) \
                \VALUES (?, ?, ?, ?, ?)"

type TagRow = (Name, ProviderId, ServiceId)

-- | Note: Consistency = One
paginateServiceTags :: MonadClient m
    => QueryAnyTags 1 3
    -> Maybe Name
    -> Int32
    -> m ServiceProfilePage
paginateServiceTags tags start size = liftClient $ do
    let start' = Name (maybe "" (toLower . fromName) start)
    let size'  = size + 1
    let tags'  = unpackTags tags
    p <- filterPrefix (fromName start') <$> queryAll start' size' tags'
    r <- mapConcurrently resolveRow (result p)
    return $! ServiceProfilePage (hasMore p) (catMaybes r)
  where
    filterPrefix :: Text -> Page TagRow -> Page TagRow
    filterPrefix prefix p = do
        let prefixed = filter (\(Name n, _, _) -> prefix `isPrefixOf` (toLower n)) (result p)
            -- if they were all valid prefixes, there may be more in Cassandra
            allValid = length prefixed == length (result p)
            more     = allValid && hasMore p
         in p { hasMore = more, result = prefixed }

    unpackTags :: QueryAnyTags 1 3 -> [QueryAllTags 1 3]
    unpackTags = Set.toList . fromRange . queryAnyTagsRange

    queryAll :: Name -> Int32 -> [QueryAllTags 1 3] -> Client (Page TagRow)
    queryAll _ _  [] = return emptyPage
    queryAll s l [t] = do
        p <- queryTags s l t
        return $! p { result = trim size (result p) }
    queryAll s l ts  = do
        ps <- mapConcurrently (queryTags s l) ts
        let rows = trim l (unfoldr nextRow (map result ps))
        let more = any hasMore ps || length rows > fromIntegral size
        return $! emptyPage { hasMore = more, result = trim size rows }

    nextRow :: [[TagRow]] -> Maybe (TagRow, [[TagRow]])
    nextRow rs = case mapMaybe uncons rs of
        [] -> Nothing
        hs -> let next = fst $ minimumBy (compare `on` fst) hs
                  cons (r, rs') = if r == next then rs' else r:rs'
                  rest = map cons hs
              in Just (next, rest)

    resolveRow :: MonadClient m => TagRow -> m (Maybe ServiceProfile)
    resolveRow (_, pid, sid) = lookupServiceProfile pid sid

    trim l = take (fromIntegral l)

    queryTags s l t =
        let t' = foldTags (queryAllTagsRange t)
        in retry x1 $ paginate cql $ paramsP One (defBucket, t', s) l

    cql :: PrepQuery R (Bucket, Int64, Name) (Name, ProviderId, ServiceId)
    cql = "SELECT name, provider, service FROM service_tag \
          \WHERE bucket = ? AND tag = ? AND name >= ?"

