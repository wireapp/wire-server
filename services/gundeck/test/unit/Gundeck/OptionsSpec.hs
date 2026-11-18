-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Gundeck.OptionsSpec (tests) where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Yaml qualified as Yaml
import Gundeck.Options (Opts)
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Required notification database configuration"
    [ testCase "the integration configuration parses" $ do
        cfg <- readConfig
        case A.fromJSON @Opts (A.Object cfg) of
          A.Success _ -> pure ()
          A.Error err -> assertFailure err,
      testCase "missing configuration is rejected" $ do
        cfg <- readConfig
        assertRejected (KeyMap.delete "notificationFanInPostgres" cfg),
      testCase "null configuration is rejected" $ do
        cfg <- readConfig
        assertRejected (KeyMap.insert "notificationFanInPostgres" A.Null cfg)
    ]

assertRejected :: A.Object -> Assertion
assertRejected cfg = case A.fromJSON @Opts (A.Object cfg) of
  A.Error _ -> pure ()
  A.Success _ -> assertFailure "Accepted a configuration without a notification database"

-- Parsing configuration does not connect to any of the configured services.
readConfig :: IO A.Object
readConfig =
  Yaml.decodeFileEither "gundeck.integration.yaml" >>= \case
    Right (A.Object cfg) -> pure cfg
    Right _ -> fail "The integration configuration is not an object"
    Left err -> fail (show err)
