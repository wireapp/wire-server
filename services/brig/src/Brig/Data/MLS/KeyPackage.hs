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
  )
where

import Brig.App
import Brig.Data.MLS.KeyPackage.Instances ()
import Cassandra
import Control.Error
import Control.Lens
import Control.Monad.Random (randomRIO)
import Data.Functor
import Data.Id
import Imports
import Wire.API.MLS.KeyPackage

insertKeyPackages :: UserId -> ClientId -> [(KeyPackageRef, KeyPackageData)] -> AppIO r ()
insertKeyPackages uid cid kps = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ kps $ \(ref, kp) -> addPrepQuery q (uid, cid, ref, kp)
  where
    q :: PrepQuery W (UserId, ClientId, KeyPackageRef, KeyPackageData) ()
    q = "INSERT INTO mls_key_packages (uid, text, ref, data) VALUES (?, ?, ?, ?)"

claimKeyPackage :: UserId -> ClientId -> MaybeT AppIO KeyPackageData
claimKeyPackage u c = MaybeT $ do
  -- FUTUREWORK: investigate better locking strategies
  lock <- view keyPackageLocalLock
  withMVar lock . const $ do
    kps <- retry x1 $ query lookupQuery (params LocalQuorum (u, c))
    mk <- liftIO (pick kps)
    for mk $ \(ref, kpd) -> do
      retry x5 $ write deleteQuery (params LocalQuorum (u, c, ref))
      pure kpd
  where
    lookupQuery :: PrepQuery R (UserId, ClientId) (KeyPackageRef, KeyPackageData)
    lookupQuery = "SELECT ref, data FROM mls_key_packages WHERE uid = ? AND client = ?"

    deleteQuery :: PrepQuery W (UserId, ClientId, KeyPackageRef) ()
    deleteQuery = "DELETE FROM mls_key_packages WHERE uid = ? AND client = ? AND ref = ?"

countKeyPackages :: UserId -> ClientId -> AppIO Int32
countKeyPackages u c =
  retry x1 $ sum . fmap runIdentity <$> query q (params LocalQuorum (u, c))
  where
    q :: PrepQuery R (UserId, ClientId) (Identity Int32)
    q = "SELECT COUNT(*) FROM mls_key_packages WHERE uid = ? AND client = ?"

--------------------------------------------------------------------------------
-- Utilities

pick :: [a] -> IO (Maybe a)
pick [] = pure Nothing
pick xs = do
  i <- randomRIO (0, length xs - 1)
  pure (atMay xs i)
