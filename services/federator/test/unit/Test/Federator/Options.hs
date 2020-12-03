{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

module Test.Federator.Options where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Domain (mkDomain)
import qualified Data.Yaml as Yaml
import Federator.Options
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Options"
    [ parseFederationStrategy
    ]

parseFederationStrategy :: TestTree
parseFederationStrategy =
  testCase "parse FederationStrategy examples" $ do
    assertParsesAs AllowAll $
      "allowAll: null"
    assertParsesAs (withAllowList []) $
      "allowedDomains: []"
    assertParsesAs (withAllowList ["test.org"]) $
      "allowedDomains:\n\
      \  - test.org"
    assertParsesAs (withAllowList ["example.com", "wire.com"]) $
      "allowedDomains:\n\
      \  - example.com\n\
      \  - wire.com"
    -- manual roundtrip example AllowAll
    let allowA = toStrict $ Aeson.encode AllowAll
    assertParsesAs AllowAll $ allowA
    -- manual roundtrip example AllowList
    let allowWire = (withAllowList ["wire.com"])
    let allowedDom = toStrict $ Aeson.encode allowWire
    assertParsesAs allowWire $ allowedDom
  where
    withAllowList =
      AllowList . AllowedDomains . map (either error id . mkDomain)

assertParsesAs :: (HasCallStack, Eq a, FromJSON a, Show a) => a -> ByteString -> Assertion
assertParsesAs v bs =
  assertEqual "YAML parsing" (Right v) $
    either (Left . show) Right (Yaml.decodeEither' bs)
