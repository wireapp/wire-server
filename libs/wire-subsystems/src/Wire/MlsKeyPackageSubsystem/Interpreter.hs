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

module Wire.MlsKeyPackageSubsystem.Interpreter (interpretMlsKeyPackageSubsystem) where

import Control.Concurrent qualified as C
import Control.Error (atMay)
import Control.Monad.Random (randomRIO)
import Data.Id
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX
import Imports
import Polysemy
import Polysemy.Resource (Resource, bracket)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Serialisation
import Wire.MlsKeyPackageStore qualified as Store
import Wire.MlsKeyPackageSubsystem (MlsKeyPackageSubsystem (..), validateKeyPackageLifetime)

interpretMlsKeyPackageSubsystem :: (Member Store.MlsKeyPackageStore r, Member (Embed IO) r, Member Resource r) => Maybe NominalDiffTime -> C.MVar () -> InterpreterFor MlsKeyPackageSubsystem r
interpretMlsKeyPackageSubsystem configuredLifetime lock = interpret $ \case
  InsertMlsKeyPackages u c ps -> insertMlsKeyPackages u c ps
  ClaimMlsKeyPackage u c s -> claimMlsKeyPackage configuredLifetime lock u c s
  CountMlsKeyPackages u c s -> countMlsKeyPackages configuredLifetime u c s
  DeleteMlsKeyPackages u c s rs -> deleteMlsKeyPackages u c s rs
  DeleteAllMlsKeyPackages u c ss -> deleteAllMlsKeyPackages u c ss

insertMlsKeyPackages :: (Member Store.MlsKeyPackageStore r) => UserId -> ClientId -> [(KeyPackageRef, CipherSuiteTag, KeyPackageData)] -> Sem r ()
insertMlsKeyPackages u c ps = Store.insertKeyPackages u c ps

claimMlsKeyPackage ::
  (Member Store.MlsKeyPackageStore r, Member (Embed IO) r, Member Resource r) =>
  Maybe NominalDiffTime ->
  C.MVar () ->
  UserId ->
  ClientId ->
  CipherSuiteTag ->
  Sem r (Maybe (KeyPackageRef, KeyPackageData))
claimMlsKeyPackage maxLifetime lock u c s =
  bracket (embed $ C.takeMVar lock) (embed . C.putMVar lock) (const claim)
  where
    claim :: (Member Store.MlsKeyPackageStore r, Member (Embed IO) r) => Sem r (Maybe (KeyPackageRef, KeyPackageData))
    claim = do
      candidates <- getNonClaimedKeyPackages maxLifetime u c s
      case candidates of
        [] -> pure Nothing
        _ -> do
          mk <- embed (pick candidates)
          for mk $ \candidate -> do
            Store.deleteKeyPackage u c s (fst candidate)
            pure candidate
    pick :: [a] -> IO (Maybe a)
    pick [] = pure Nothing
    pick xs = do
      i <- randomRIO (0, length xs - 1)
      pure (atMay xs i)

countMlsKeyPackages ::
  ( Member Store.MlsKeyPackageStore r,
    Member (Embed IO) r
  ) =>
  Maybe NominalDiffTime ->
  UserId ->
  ClientId ->
  CipherSuiteTag ->
  Sem r Int64
countMlsKeyPackages configuredLifetime u c s = fromIntegral . length <$> getNonClaimedKeyPackages configuredLifetime u c s

deleteMlsKeyPackages ::
  (Member Store.MlsKeyPackageStore r) =>
  UserId ->
  ClientId ->
  CipherSuiteTag ->
  [KeyPackageRef] ->
  Sem r ()
deleteMlsKeyPackages u c s rs = Store.deleteKeyPackages u c s rs

deleteAllMlsKeyPackages :: (Member Store.MlsKeyPackageStore r) => UserId -> ClientId -> [CipherSuiteTag] -> Sem r ()
deleteAllMlsKeyPackages u c ss = Store.deleteAllKeyPackages u c ss

-- | Fetch all unclaimed non-expired key packages for a given client and delete
-- from the database those that have expired.
getNonClaimedKeyPackages :: (Member Store.MlsKeyPackageStore r, Member (Embed IO) r) => Maybe NominalDiffTime -> UserId -> ClientId -> CipherSuiteTag -> Sem r [(KeyPackageRef, KeyPackageData)]
getNonClaimedKeyPackages maxLifetime u c s = do
  rows <- Store.lookupKeyPackages u c s
  now <- embed getPOSIXTime
  let decoded = mapMaybe decode rows
      (expired, usable) = partition (isExpired now maxLifetime) decoded
  Store.deleteKeyPackages u c s (map (fst . snd) expired)
  pure (map snd usable)
  where
    decode :: (KeyPackageRef, KeyPackageData) -> Maybe (KeyPackage, (KeyPackageRef, KeyPackageData))
    decode row@(_, packageData) = do
      package <- either (const Nothing) Just (decodeMLS' (kpData packageData) :: Either Text (RawMLS KeyPackage))
      pure (package.value, row)
    isExpired now configuredLifetime (package, _) = case package.leafNode.source of
      LeafNodeSourceKeyPackage lifetime -> isLeft (validateKeyPackageLifetime now configuredLifetime lifetime)
      _ -> True
