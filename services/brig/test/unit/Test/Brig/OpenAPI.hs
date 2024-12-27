{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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

module Test.Brig.OpenAPI where

import Brig.API.Public.Swagger
import Control.Lens
import Data.Aeson
import Data.Aeson.Diff qualified as AD
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens
import Data.String.Conversions (cs)
import Imports
import Test.Tasty qualified as T
import Test.Tasty.HUnit
import Wire.API.Routes.Version

tests :: T.TestTree
tests =
  T.testGroup "OpenAPI3 aka Swagger" $
    [ T.testGroup "frozen api versions" [testCase "must be stable" frozenApiVersions]
    ]

frozenApiVersions :: Assertion
frozenApiVersions = do
  filter (not . isDevelopmentVersion) [minBound ..] @=? [V0 .. V7]
  -- if not, change the expected value and add more cases in `frozenApiVersions` below and in
  -- `frozenSpec`

  frozenApiVersion @V7
  frozenApiVersion @V6
  frozenApiVersion @V5
  frozenApiVersion @V4
  frozenApiVersion @V3
  frozenApiVersion @V2
  frozenApiVersion @V1
  frozenApiVersion @V0

frozenSpec :: Version -> LByteString
frozenSpec V0 = $(pregenSwagger V0)
frozenSpec V1 = $(pregenSwagger V1)
frozenSpec V2 = $(pregenSwagger V2)
frozenSpec V3 = $(pregenSwagger V3)
frozenSpec V4 = $(pregenSwagger V4)
frozenSpec V5 = $(pregenSwagger V5)
frozenSpec V6 = $(pregenSwagger V6)
frozenSpec V7 = $(pregenSwagger V7)

frozenApiVersion :: forall (v :: Version). (KnownVersion v, HasAllOpenAPIs v) => Assertion
frozenApiVersion = do
  let frozen = parseSpec $ frozenSpec (versionVal @v)
      generated = toJSON $ genSwagger @v
  frozen `shouldMatchJSON` generated

parseSpec :: LByteString -> Value
parseSpec = either error id . eitherDecode'

-- | Inspired by the  `shouldMatchWithMsg` from /integration.
shouldMatchJSON :: Value -> Value -> Assertion
shouldMatchJSON a b = do
  unless (a == b) do
    let pa = encodePretty a
        pb = encodePretty b

        -- show diff, but only in the interesting cases.
        diff =
          if isObj || isArr
            then "\nDiff:\n" <> encodePretty (AD.diff a b)
            else ""
          where
            isObj = isJust (a ^? _Object) && isJust (b ^? _Object)
            isArr = isJust (a ^? _Array) && isJust (b ^? _Array)

    assertFailure . cs $ "\n*** JSON values do not coincide.\n" <> "Actual:\n" <> pa <> "\nExpected:\n" <> pb <> diff
