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
  )
where

import Brig.App
import Cassandra
import Cassandra.Settings
import Control.Error
import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Random (randomRIO)
import Data.Domain
import Data.Functor
import Data.Id
import Data.Qualified
import Imports
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage

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
    kps <- retry x1 $ query lookupQuery (params LocalQuorum (tUnqualified u, c))
    mk <- liftIO (pick kps)
    for mk $ \(ref, kpd) -> do
      retry x5 $ write deleteQuery (params LocalQuorum (tUnqualified u, c, ref))
      pure (ref, kpd)
  lift $ mapKeyPackageRef ref (qUntagged u) c
  pure (ref, kpd)
  where
    lookupQuery :: PrepQuery R (UserId, ClientId) (KeyPackageRef, KeyPackageData)
    lookupQuery = "SELECT ref, data FROM mls_key_packages WHERE user = ? AND client = ?"

    deleteQuery :: PrepQuery W (UserId, ClientId, KeyPackageRef) ()
    deleteQuery = "DELETE FROM mls_key_packages WHERE user = ? AND client = ? AND ref = ?"

-- | Add key package ref to mapping table.
mapKeyPackageRef :: MonadClient m => KeyPackageRef -> Qualified UserId -> ClientId -> m ()
mapKeyPackageRef ref u c =
  write insertQuery (params LocalQuorum (ref, qDomain u, qUnqualified u, c))
  where
    insertQuery :: PrepQuery W (KeyPackageRef, Domain, UserId, ClientId) ()
    insertQuery = "INSERT INTO mls_key_package_refs (ref, domain, user, client) VALUES (?, ?, ?, ?)"

countKeyPackages :: MonadClient m => UserId -> ClientId -> m Int64
countKeyPackages u c =
  retry x1 $ sum . fmap runIdentity <$> query1 q (params LocalQuorum (u, c))
  where
    q :: PrepQuery R (UserId, ClientId) (Identity Int64)
    q = "SELECT COUNT(*) FROM mls_key_packages WHERE user = ? AND client = ?"

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

--------------------------------------------------------------------------------
-- Utilities

pick :: [a] -> IO (Maybe a)
pick [] = pure Nothing
pick xs = do
  i <- randomRIO (0, length xs - 1)
  pure (atMay xs i)
