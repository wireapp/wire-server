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

module Wire.MlsKeyPackageSubsystem where

import Data.Id
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Imports
import Polysemy
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Lifetime

data MlsKeyPackageSubsystem m a where
  InsertMlsKeyPackages :: UserId -> ClientId -> [(KeyPackageRef, CipherSuiteTag, KeyPackageData)] -> MlsKeyPackageSubsystem m ()
  ClaimMlsKeyPackage :: UserId -> ClientId -> CipherSuiteTag -> MlsKeyPackageSubsystem m (Maybe (KeyPackageRef, KeyPackageData))
  HasMlsKeyPackages :: UserId -> ClientId -> CipherSuiteTag -> MlsKeyPackageSubsystem m Bool
  HasMlsKeyPackagesBulk :: [(UserId, ClientId, CipherSuiteTag)] -> MlsKeyPackageSubsystem m (Set (UserId, ClientId, CipherSuiteTag))
  CountMlsKeyPackages :: UserId -> ClientId -> CipherSuiteTag -> MlsKeyPackageSubsystem m Int64
  DeleteMlsKeyPackages :: UserId -> ClientId -> CipherSuiteTag -> [KeyPackageRef] -> MlsKeyPackageSubsystem m ()
  DeleteAllMlsKeyPackages :: UserId -> ClientId -> [CipherSuiteTag] -> MlsKeyPackageSubsystem m ()

makeSem ''MlsKeyPackageSubsystem

validateKeyPackageLifetime :: POSIXTime -> Maybe NominalDiffTime -> Lifetime -> Either Text ()
validateKeyPackageLifetime now maxLifetime lifetime = do
  when (tsPOSIX lifetime.ltNotBefore > now) $ Left "Key package not_before date is in the future"
  when (tsPOSIX lifetime.ltNotAfter <= now) $ Left "Key package is expired"
  for_ maxLifetime $ \maxAge -> when (tsPOSIX lifetime.ltNotAfter > now + maxAge) $ Left "Key package expiration time is too far in the future"
