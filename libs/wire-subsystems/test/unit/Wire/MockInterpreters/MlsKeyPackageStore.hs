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

-- REVIEW: Appreviations as binding names make this hard to understand.
inMemoryMlsKeyPackageStoreInterpreter ::
  (Member (State KeyPackageStoreState) r) =>
  InterpreterFor MlsKeyPackageStore r
inMemoryMlsKeyPackageStoreInterpreter = interpret \case
  InsertKeyPackages u c ps ->
    for_ ps \(ref, suite, packageData) ->
      -- REVIEW: Why does `<>` need to be flipped?
      modify $ Map.insertWith (flip (<>)) (u, c, suite) [(ref, packageData)]
  LookupKeyPackages u c s -> gets $ Map.findWithDefault [] (u, c, s)
  LookupKeyPackagesBulk requests -> gets \st -> Map.restrictKeys st (Set.fromList requests)
  DeleteKeyPackages u c s refs ->
    modify $ Map.adjust (filter ((`notElem` refs) . fst)) (u, c, s)
  DeleteAllKeyPackages u c suites ->
    modify \st -> foldr (\s -> Map.delete (u, c, s)) st suites
  DeleteKeyPackage u c s ref ->
    modify $ Map.adjust (filter ((/= ref) . fst)) (u, c, s)
