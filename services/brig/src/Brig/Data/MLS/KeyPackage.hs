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
    countKeyPackages,
    deleteKeyPackages,
  )
where

import Brig.API.MLS.KeyPackages.Validation
import Brig.App
import Brig.Options hiding (Timeout)
import Cassandra
import Control.Arrow
import Control.Error
import Control.Lens
import Control.Monad.Random (randomRIO)
import Data.Functor
import Data.Id
import Data.Qualified
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Imports
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Serialisation

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
      retry x5 $ write delete1Query (params LocalQuorum (tUnqualified u, c, ref))
      pure (ref, kpd)
  pure (ref, kpd)
  where
    delete1Query :: PrepQuery W (UserId, ClientId, KeyPackageRef) ()
    delete1Query = "DELETE FROM mls_key_packages WHERE user = ? AND client = ? AND ref = ?"

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
  deleteKeyPackages (tUnqualified u) c (map (\(_, (ref, _)) -> ref) kpsExpired)
  pure $ fmap snd kpsNonExpired
  where
    lookupQuery :: PrepQuery R (UserId, ClientId) (KeyPackageRef, KeyPackageData)
    lookupQuery = "SELECT ref, data FROM mls_key_packages WHERE user = ? AND client = ?"

    decodeKp :: (a, KeyPackageData) -> Maybe KeyPackage
    decodeKp = hush . decodeMLS' . kpData . snd

    keepDecoded :: (Maybe a, b) -> [(a, b)]
    keepDecoded (Nothing, _) = []
    keepDecoded (Just v, w) = [(v, w)]

    hasExpired :: POSIXTime -> Maybe NominalDiffTime -> (KeyPackage, a) -> Bool
    hasExpired now mMaxLifetime (kp, _) =
      case kp.leafNode.source of
        LeafNodeSourceKeyPackage lt ->
          either (const True) (const False) . validateLifetime' now mMaxLifetime $ lt
        _ -> True -- the assumption is the key package is valid and has the
        -- required extensions so we return 'True'

countKeyPackages ::
  ( MonadReader Env m,
    MonadClient m
  ) =>
  Local UserId ->
  ClientId ->
  m Int64
countKeyPackages u c = fromIntegral . length <$> getNonClaimedKeyPackages u c

deleteKeyPackages :: MonadClient m => UserId -> ClientId -> [KeyPackageRef] -> m ()
deleteKeyPackages u c refs =
  retry x5 $
    write
      deleteQuery
      (params LocalQuorum (u, c, refs))
  where
    deleteQuery :: PrepQuery W (UserId, ClientId, [KeyPackageRef]) ()
    deleteQuery = "DELETE FROM mls_key_packages WHERE user = ? AND client = ? AND ref in ?"

--------------------------------------------------------------------------------
-- Utilities

pick :: [a] -> IO (Maybe a)
pick [] = pure Nothing
pick xs = do
  i <- randomRIO (0, length xs - 1)
  pure (atMay xs i)
