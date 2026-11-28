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

module Test.Cannon.Options (tests) where

import Cannon.Options (Opts)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Yaml qualified as YAML
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup "Fan-in configuration" $
    [ testCase ("parses " <> path) $ do
        value <- YAML.decodeFileThrow @IO @A.Value path
        case A.fromJSON @Opts value of
          A.Success _ -> pure ()
          A.Error err -> assertFailure err
    | path <- ["cannon.integration.yaml", "cannon2.integration.yaml"]
    ]
      <> [ testCase "requires a notification database" $ do
             value <- YAML.decodeFileThrow @IO @A.Value "cannon.integration.yaml"
             case value of
               A.Object object -> case A.fromJSON @Opts (A.Object (KeyMap.delete "notificationFanInPostgres" object)) of
                 A.Error _ -> pure ()
                 A.Success _ -> assertFailure "Accepted missing notification database"
               _ -> assertFailure "Invalid fixture"
         ]
