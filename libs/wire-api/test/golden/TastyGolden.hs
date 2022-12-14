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

module TastyGolden where

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
import Imports
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Wire.API.Golden.Manual.CreateScimToken

tests :: TestTree
tests =
  testGroup
    "Manual golden tests"
    [ testGroup
        "CreateScimToken"
        [ goldenTestToJson "testObject_CreateScimToken_1" testObject_CreateScimToken_1,
          goldenTestToJson "testObject_CreateScimToken_2" testObject_CreateScimToken_2,
          goldenTestToJson "testObject_CreateScimToken_3" testObject_CreateScimToken_3
        ]
    ]
  where
    goldenTestToJson :: ToJSON a => String -> a -> TestTree
    goldenTestToJson fp a =
      goldenVsString fp ("test/golden/golden-files/" <> fp <> ".json") (pure $ encodePretty' config a)

    config :: Config
    config = defConfig {confCompare = compare, confTrailingNewline = True}
