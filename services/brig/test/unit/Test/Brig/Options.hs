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

module Test.Brig.Options (tests) where

import Brig.Options
import Data.Aeson (eitherDecode, object)
import Data.Map.Strict qualified as Map
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Brig.Options"
    [ testGroup
        "deriveSsoIdpChangeDetectionEnabled"
        [ testCase "absent inputs disable" $
            deriveSsoIdpChangeDetectionEnabled Nothing @?= False,
          testCase "empty inputs disable" $
            deriveSsoIdpChangeDetectionEnabled (Just (SsoIdpChangeDetectionInputs Map.empty [])) @?= False,
          testCase "domains without allowlist disable" $
            deriveSsoIdpChangeDetectionEnabled (Just (SsoIdpChangeDetectionInputs (Map.singleton "example.com" (object [])) [])) @?= False,
          testCase "allowlist without domains disable" $
            deriveSsoIdpChangeDetectionEnabled (Just (SsoIdpChangeDetectionInputs Map.empty ["AA:BB"])) @?= False,
          testCase "domains and allowlist enable" $
            deriveSsoIdpChangeDetectionEnabled (Just (SsoIdpChangeDetectionInputs (Map.singleton "example.com" (object [])) ["AA:BB"])) @?= True,
          testCase "chart-rendered JSON parses" $
            eitherDecode
              "{\"multiIngressDomainConfigs\":{\"example.com\":{}},\"idpCertFingerprintAllowlist\":[\"AA:BB\"]}"
              @?= Right (SsoIdpChangeDetectionInputs (Map.singleton "example.com" (object [])) ["AA:BB"])
        ]
    ]
