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

module Brig.Data.MLS.KeyPackage
  ( insertKeyPackages,
    claimKeyPackage,
    mapKeyPackageRef,
    countKeyPackages,
    derefKeyPackage,
    keyPackageRefConvId,
    keyPackageRefSetConvId,
    addKeyPackageRef,
    updateKeyPackageRef,
  )
where

import Brig.API.MLS.KeyPackages.Validation
import Brig.App
import Brig.Options hiding (Timeout)
import Cassandra
import Cassandra.Settings
import Control.Arrow
import Control.Error
import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Random (randomRIO)
import Data.Domain
import Data.Functor
import Data.Id
import Data.Qualified
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Imports
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.Routes.Internal.Brig

insertKeyPackages :: MonadClient m => UserId -> ClientId -> [(KeyPackageRef, KeyPackageData)] -> m ()
insertKeyPackages uid cid kps = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ kps $ \(ref, kp) -> do
    addPrepQuery q (uid, cid, kp, ref)
  where
    q :: PrepQuery W (UserId, ClientId, KeyPackageData, KeyPackageRef) ()
    q = "INSERT INTO mls_key_packages (user, client, data, ref) VALUES (?, ?, ?, ?)"

claimKeyPackage ::
  ( MonadReader Env m,
    MonadUnliftIO m,
    MonadClient m
  ) =>
  Local UserId ->
  ClientId ->
  MaybeT m (KeyPackageRef, KeyPackageData)
claimKeyPackage u c = do
  -- FUTUREWORK: investigate better locking strategies
  lock <- lift $ view keyPackageLocalLock
  -- get a random key package and delete it
  (ref, kpd) <- MaybeT . withMVar lock . const $ do
    kps <- getNonClaimedKeyPackages u c
    mk <- liftIO (pick kps)
    for mk $ \(ref, kpd) -> do
      retry x5 $ write deleteByRef (params LocalQuorum (tUnqualified u, c, ref))
      pure (ref, kpd)
  lift $ mapKeyPackageRef ref (qUntagged u) c
  pure (ref, kpd)
  where
    deleteByRef :: PrepQuery W (UserId, ClientId, KeyPackageRef) ()
    deleteByRef = "DELETE FROM mls_key_packages WHERE user = ? AND client = ? AND ref = ?"

-- | Fetch all unclaimed non-expired key packages for a given client and delete
-- from the database those that have expired.
getNonClaimedKeyPackages ::
  ( MonadReader Env m,
    MonadClient m
  ) =>
  Local UserId ->
  ClientId ->
  m [(KeyPackageRef, KeyPackageData)]
getNonClaimedKeyPackages u c = do
  kps <- retry x1 $ query lookupQuery (params LocalQuorum (tUnqualified u, c))
  let decodedKps = foldMap (keepDecoded . (decodeKp &&& id)) kps

  now <- liftIO getPOSIXTime
  mMaxLifetime <- setKeyPackageMaximumLifetime <$> view settings

  let (kpsExpired, kpsNonExpired) =
        partition (hasExpired now mMaxLifetime) decodedKps
  -- delete expired key packages
  let kpsExpired' = fmap (\(_, (ref, _)) -> ref) kpsExpired
   in retry x5 $
        write
          deleteByRefs
          (params LocalQuorum (tUnqualified u, c, kpsExpired'))
  pure $ fmap snd kpsNonExpired
  where
    lookupQuery :: PrepQuery R (UserId, ClientId) (KeyPackageRef, KeyPackageData)
    lookupQuery = "SELECT ref, data FROM mls_key_packages WHERE user = ? AND client = ?"

    deleteByRefs :: PrepQuery W (UserId, ClientId, [KeyPackageRef]) ()
    deleteByRefs = "DELETE FROM mls_key_packages WHERE user = ? AND client = ? AND ref in ?"

    decodeKp :: (a, KeyPackageData) -> Maybe KeyPackage
    decodeKp = hush . decodeMLS' . kpData . snd

    keepDecoded :: (Maybe a, b) -> [(a, b)]
    keepDecoded (Nothing, _) = []
    keepDecoded (Just v, w) = [(v, w)]

    hasExpired :: POSIXTime -> Maybe NominalDiffTime -> (KeyPackage, a) -> Bool
    hasExpired now mMaxLifetime (kp, _) =
      case findExtensions (kpExtensions kp) of
        Left _ -> True -- the assumption is the key package is valid and has the
        -- required extensions so we return 'True'
        Right (runIdentity . reLifetime -> lt) ->
          either (const True) (const False) . validateLifetime' now mMaxLifetime $ lt

-- | Add key package ref to mapping table.
mapKeyPackageRef :: MonadClient m => KeyPackageRef -> Qualified UserId -> ClientId -> m ()
mapKeyPackageRef ref u c =
  write insertQuery (params LocalQuorum (ref, qDomain u, qUnqualified u, c))
  where
    insertQuery :: PrepQuery W (KeyPackageRef, Domain, UserId, ClientId) ()
    insertQuery = "INSERT INTO mls_key_package_refs (ref, domain, user, client) VALUES (?, ?, ?, ?)"

countKeyPackages ::
  ( MonadReader Env m,
    MonadClient m
  ) =>
  Local UserId ->
  ClientId ->
  m Int64
countKeyPackages u c = fromIntegral . length <$> getNonClaimedKeyPackages u c

derefKeyPackage :: MonadClient m => KeyPackageRef -> MaybeT m ClientIdentity
derefKeyPackage ref = do
  (d, u, c) <- MaybeT . retry x1 $ query1 q (params LocalQuorum (Identity ref))
  pure $ ClientIdentity d u c
  where
    q :: PrepQuery R (Identity KeyPackageRef) (Domain, UserId, ClientId)
    q = "SELECT domain, user, client from mls_key_package_refs WHERE ref = ?"

keyPackageRefConvId :: MonadClient m => KeyPackageRef -> MaybeT m (Qualified ConvId)
keyPackageRefConvId ref = MaybeT $ do
  qr <- retry x1 $ query1 q (params LocalSerial (Identity ref))
  pure $ do
    (domain, cid) <- qr
    Qualified <$> cid <*> domain
  where
    q :: PrepQuery R (Identity KeyPackageRef) (Maybe Domain, Maybe ConvId)
    q = "SELECT conv_domain, conv FROM mls_key_package_refs WHERE ref = ?"

-- We want to proper update, not an upsert, to avoid "ghost" refs without user+client
keyPackageRefSetConvId :: MonadClient m => KeyPackageRef -> Qualified ConvId -> m Bool
keyPackageRefSetConvId ref convId = do
  updated <-
    retry x5 $
      trans
        q
        (params LocalQuorum (qDomain convId, qUnqualified convId, ref))
          { serialConsistency = Just LocalSerialConsistency
          }
  case updated of
    [] -> pure False
    [_] -> pure True
    _ -> throwM $ ErrorCall "Primary key violation detected mls_key_package_refs.ref"
  where
    q :: PrepQuery W (Domain, ConvId, KeyPackageRef) x
    q = "UPDATE mls_key_package_refs SET conv_domain = ?, conv = ? WHERE ref = ? IF EXISTS"

addKeyPackageRef :: MonadClient m => KeyPackageRef -> NewKeyPackageRef -> m ()
addKeyPackageRef ref nkpr = do
  retry x5 $
    write
      q
      (params LocalQuorum (nkprClientId nkpr, qUnqualified (nkprConversation nkpr), qDomain (nkprConversation nkpr), qDomain (nkprUserId nkpr), qUnqualified (nkprUserId nkpr), ref))
  where
    q :: PrepQuery W (ClientId, ConvId, Domain, Domain, UserId, KeyPackageRef) x
    q = "UPDATE mls_key_package_refs SET client = ?, conv = ?, conv_domain = ?, domain = ?, user = ? WHERE ref = ?"

-- | Update key package ref, used in Galley when commit reveals key package ref update for the sender.
-- Nothing is changed if the previous key package ref is not found in the table.
-- Updating amounts to INSERT the new key package ref, followed by DELETE the
-- previous one.
--
-- FUTUREWORK: this function has to be extended if a table mapping (client,
-- conversation) to key package ref is added, for instance, when implementing
-- external delete proposals.
updateKeyPackageRef :: MonadClient m => KeyPackageRef -> KeyPackageRef -> m ()
updateKeyPackageRef prevRef newRef =
  void . runMaybeT $ do
    backup <- backupKeyPackageMeta prevRef
    lift $ restoreKeyPackageMeta newRef backup >> deleteKeyPackage prevRef

--------------------------------------------------------------------------------
-- Utilities

backupKeyPackageMeta :: MonadClient m => KeyPackageRef -> MaybeT m (ClientId, Maybe (Qualified ConvId), Qualified UserId)
backupKeyPackageMeta ref = do
  (clientId, convId, convDomain, userDomain, userId) <- MaybeT . retry x1 $ query1 q (params LocalQuorum (Identity ref))
  pure (clientId, Qualified <$> convId <*> convDomain, Qualified userId userDomain)
  where
    q :: PrepQuery R (Identity KeyPackageRef) (ClientId, Maybe ConvId, Maybe Domain, Domain, UserId)
    q = "SELECT client, conv, conv_domain, domain, user FROM mls_key_package_refs WHERE ref = ?"

restoreKeyPackageMeta :: MonadClient m => KeyPackageRef -> (ClientId, Maybe (Qualified ConvId), Qualified UserId) -> m ()
restoreKeyPackageMeta ref (clientId, convId, userId) = do
  write q (params LocalQuorum (ref, clientId, qUnqualified <$> convId, qDomain <$> convId, qDomain userId, qUnqualified userId))
  where
    q :: PrepQuery W (KeyPackageRef, ClientId, Maybe ConvId, Maybe Domain, Domain, UserId) ()
    q = "INSERT INTO mls_key_package_refs (ref, client, conv, conv_domain, domain, user) VALUES (?, ?, ?, ?, ?, ?)"

deleteKeyPackage :: MonadClient m => KeyPackageRef -> m ()
deleteKeyPackage ref =
  retry x5 $
    write
      q
      (params LocalQuorum (Identity ref))
  where
    q :: PrepQuery W (Identity KeyPackageRef) x
    q = "DELETE FROM mls_key_package_refs WHERE ref = ?"

pick :: [a] -> IO (Maybe a)
pick [] = pure Nothing
pick xs = do
  i <- randomRIO (0, length xs - 1)
  pure (atMay xs i)
