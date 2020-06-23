-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module TestSetup
  ( test,
    tsManager,
    tsCargohold,
    TestSignature,
    TestSetup (..),
    CargoHold,
  )
where

import Bilge (Request)
import Bilge.IO (Http, Manager, runHttpT)
import Control.Lens (makeLenses, (^.))
import Imports
import Test.Tasty
import Test.Tasty.HUnit

type CargoHold = Request -> Request

type TestSignature a = CargoHold -> Http a

data TestSetup = TestSetup
  { _tsManager :: Manager,
    _tsCargohold :: CargoHold
  }

makeLenses ''TestSetup

test :: IO TestSetup -> TestName -> TestSignature a -> TestTree
test s n h = testCase n runTest
  where
    runTest = do
      setup <- s
      (void $ runHttpT (setup ^. tsManager) (h (setup ^. tsCargohold)))
