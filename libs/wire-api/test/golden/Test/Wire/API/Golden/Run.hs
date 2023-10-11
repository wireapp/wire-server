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

module Test.Wire.API.Golden.Run (main) where

import Imports
import Test.Tasty
import Test.Wire.API.Golden.FromJSON qualified as Golden.FromJSON
import Test.Wire.API.Golden.Generated qualified as Golden.Generated
import Test.Wire.API.Golden.Manual qualified as Golden.Manual
import Test.Wire.API.Golden.Protobuf qualified as Golden.Protobuf

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Golden.Generated.tests,
        Golden.Manual.tests,
        Golden.FromJSON.tests,
        Golden.Protobuf.tests
      ]
