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

module Wire.MockInterpreters.MlsKeyPackageStore where

import Data.Id
import Data.Map qualified as Map
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.State
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage (KeyPackageData, KeyPackageRef)
import Wire.MlsKeyPackageStore

type KeyPackageStoreState = Map (UserId, ClientId, CipherSuiteTag) [(KeyPackageRef, KeyPackageData)]

runInMemoryMlsKeyPackageStore :: KeyPackageStoreState -> InterpreterFor MlsKeyPackageStore r
runInMemoryMlsKeyPackageStore initial =
  evalState initial
    . inMemoryMlsKeyPackageStoreInterpreter
    . raiseUnder

inMemoryMlsKeyPackageStoreInterpreter ::
  (Member (State KeyPackageStoreState) r) =>
  InterpreterFor MlsKeyPackageStore r
inMemoryMlsKeyPackageStoreInterpreter = interpret \case
  InsertKeyPackages userId clientId packages ->
    for_ packages \(ref, suite, packageData) ->
      -- Like Cassandra, re-inserting a ref overwrites the old entry.
      -- `insertWith f key new` calls `f new old`.
      modify $
        Map.insertWith
          (\newPackages oldPackages -> dropRefs [ref] oldPackages <> newPackages)
          (userId, clientId, suite)
          [(ref, packageData)]
  LookupKeyPackages userId clientId suite ->
    gets $ Map.findWithDefault [] (userId, clientId, suite)
  LookupKeyPackagesBulk requests ->
    gets \store -> Map.restrictKeys store (Set.fromList requests)
  DeleteKeyPackages userId clientId suite refs ->
    modify $ Map.update (nothingIfEmpty . dropRefs refs) (userId, clientId, suite)
  DeleteAllKeyPackages userId clientId suites ->
    modify \store -> foldr (\suite -> Map.delete (userId, clientId, suite)) store suites
  DeleteKeyPackage userId clientId suite ref ->
    modify $ Map.update (nothingIfEmpty . dropRefs [ref]) (userId, clientId, suite)
  where
    dropRefs refs = filter ((`notElem` refs) . fst)
    -- Cassandra has no row for a client without key packages.
    nothingIfEmpty packages = if null packages then Nothing else Just packages
