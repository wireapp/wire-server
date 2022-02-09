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

module API.Version (tests) where

import Bilge
import Bilge.Assert
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.Routes.Version

tests :: Manager -> Brig -> TestTree
tests p brig =
  testGroup
    "version"
    [test p "GET /api-version" $ testVersion brig]

testVersion :: Brig -> Http ()
testVersion brig = do
  vinfo <-
    responseJsonError =<< get (brig . path "/api-version")
      <!! const 200 === statusCode
  liftIO $
    vinfoSupported vinfo @?= [V0, V1]
