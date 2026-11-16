{-# LANGUAGE TemplateHaskell #-}

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

module Wire.MlsKeyPackageStore where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage

data MlsKeyPackageStore m a where
  InsertKeyPackages :: UserId -> ClientId -> [(KeyPackageRef, CipherSuiteTag, KeyPackageData)] -> MlsKeyPackageStore m ()
  LookupKeyPackages :: UserId -> ClientId -> CipherSuiteTag -> MlsKeyPackageStore m [(KeyPackageRef, KeyPackageData)]
  LookupKeyPackagesBulk :: [(UserId, ClientId, CipherSuiteTag)] -> MlsKeyPackageStore m (Map.Map (UserId, ClientId, CipherSuiteTag) [(KeyPackageRef, KeyPackageData)])
  DeleteKeyPackages :: UserId -> ClientId -> CipherSuiteTag -> [KeyPackageRef] -> MlsKeyPackageStore m ()
  DeleteAllKeyPackages :: UserId -> ClientId -> [CipherSuiteTag] -> MlsKeyPackageStore m ()
  -- | Remove and return one of the given candidate key packages, picked at
  -- random. Returns 'Nothing' if none of the candidates is (still) stored.
  -- Interpreters backed by a transactional store must make this atomic, so
  -- that concurrent claims never return the same key package twice.
  ClaimKeyPackage :: UserId -> ClientId -> CipherSuiteTag -> [KeyPackageRef] -> MlsKeyPackageStore m (Maybe (KeyPackageRef, KeyPackageData))

makeSem ''MlsKeyPackageStore
