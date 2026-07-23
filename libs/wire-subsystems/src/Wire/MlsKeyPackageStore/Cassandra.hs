-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MlsKeyPackageStore.Cassandra (interpretMlsKeyPackageStoreToCassandra) where

import Cassandra as C hiding (Client)
import Data.Id
import Imports
import Polysemy
import Polysemy.Embed
import UnliftIO.Async (pooledForConcurrentlyN_)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage (KeyPackageData, KeyPackageRef)
import Wire.MlsKeyPackageStore (MlsKeyPackageStore (..))

interpretMlsKeyPackageStoreToCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor MlsKeyPackageStore r
interpretMlsKeyPackageStoreToCassandra cas =
  interpret $
    runEmbedded (runClient cas) . \case
      InsertKeyPackages u c ps -> embed $ insertKeyPackages u c ps
      LookupKeyPackages u c s -> embed $ lookupKeyPackages u c s
      DeleteKeyPackages u c s rs -> embed $ deleteKeyPackages u c s rs
      DeleteAllKeyPackages u c ss -> embed $ deleteAllKeyPackages u c ss
      DeleteKeyPackage u c s r -> embed $ deleteKeyPackage u c s r

insertKeyPackages :: (MonadClient m) => UserId -> ClientId -> [(KeyPackageRef, CipherSuiteTag, KeyPackageData)] -> m ()
insertKeyPackages u c ps = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ ps $ \(r, s, p) -> addPrepQuery insertQuery (u, c, s, p, r)

lookupKeyPackages :: (MonadClient m) => UserId -> ClientId -> CipherSuiteTag -> m [(KeyPackageRef, KeyPackageData)]
lookupKeyPackages u c s = retry x1 $ query lookupQuery (params LocalQuorum (u, c, s))

deleteKeyPackages :: (MonadClient m) => UserId -> ClientId -> CipherSuiteTag -> [KeyPackageRef] -> m ()
deleteKeyPackages u c s rs = retry x5 $ write deleteQuery (params LocalQuorum (u, c, s, rs))

deleteAllKeyPackages :: (MonadClient m, MonadUnliftIO m, Foldable f) => UserId -> ClientId -> f CipherSuiteTag -> m ()
deleteAllKeyPackages u c ss = pooledForConcurrentlyN_ 16 ss $ \s -> retry x5 $ write deleteAllQuery (params LocalQuorum (u, c, s))

deleteKeyPackage :: (MonadClient m) => UserId -> ClientId -> CipherSuiteTag -> KeyPackageRef -> m ()
deleteKeyPackage u c s r = do
  retry x5 $ write deleteKeyPackageQuery (params LocalQuorum (u, c, s, r))

insertQuery :: PrepQuery W (UserId, ClientId, CipherSuiteTag, KeyPackageData, KeyPackageRef) ()
insertQuery = "INSERT INTO mls_key_packages (user, client, cipher_suite, data, ref) VALUES (?, ?, ?, ?, ?)"

lookupQuery :: PrepQuery R (UserId, ClientId, CipherSuiteTag) (KeyPackageRef, KeyPackageData)
lookupQuery = "SELECT ref, data FROM mls_key_packages WHERE user = ? AND client = ? AND cipher_suite = ?"

deleteQuery :: PrepQuery W (UserId, ClientId, CipherSuiteTag, [KeyPackageRef]) ()
deleteQuery = "DELETE FROM mls_key_packages WHERE user = ? AND client = ? AND cipher_suite = ? AND ref IN ?"

deleteAllQuery :: PrepQuery W (UserId, ClientId, CipherSuiteTag) ()
deleteAllQuery = "DELETE FROM mls_key_packages WHERE user = ? AND client = ? AND cipher_suite = ?"

deleteKeyPackageQuery :: PrepQuery W (UserId, ClientId, CipherSuiteTag, KeyPackageRef) ()
deleteKeyPackageQuery = "DELETE FROM mls_key_packages WHERE user = ? AND client = ? AND cipher_suite = ? AND ref = ?"
