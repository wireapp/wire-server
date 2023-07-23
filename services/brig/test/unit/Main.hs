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

module Main
  ( main,
  )
where

import Imports
import qualified Test.Brig.Calling
import qualified Test.Brig.Calling.Internal
import qualified Test.Brig.InternalNotification
import qualified Test.Brig.MLS
import qualified Test.Brig.Roundtrip
import qualified Test.Brig.Swagger
import qualified Test.Brig.User.Search.Index.Types
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Test.Brig.Calling.Internal.tests,
        Test.Brig.Calling.tests,
        Test.Brig.InternalNotification.tests,
        Test.Brig.MLS.tests,
        Test.Brig.Roundtrip.tests,
        Test.Brig.Swagger.tests,
        Test.Brig.User.Search.Index.Types.tests
      ]
