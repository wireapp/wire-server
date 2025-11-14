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

module Wire.MockInterpreters.Now where

import Data.Time
import Imports
import Polysemy
import Polysemy.State
import Wire.Sem.Now

interpretNowConst ::
  UTCTime ->
  Sem (Now : r) a ->
  Sem r a
interpretNowConst time = interpret \case
  Wire.Sem.Now.Get -> pure time

type MockNow = State UTCTime

interpretNowAsState :: (Member (State UTCTime) r) => InterpreterFor Now r
interpretNowAsState =
  interpret $ \case
    Wire.Sem.Now.Get -> Polysemy.State.get

defaultTime :: UTCTime
defaultTime = UTCTime (ModifiedJulianDay 0) 0

passTime :: (Member MockNow r) => NominalDiffTime -> Sem r ()
passTime t = modify (addUTCTime t)
