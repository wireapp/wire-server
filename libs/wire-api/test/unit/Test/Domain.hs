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

module Test.Domain
  ( tests,
  )
where

import Data.Domain (DomainText (DomainText), domainText, mkDomain)
import qualified Data.Text as Text
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Domain"
    [ testGroup "serialization" testDomainSerialization
    ]

testDomainSerialization :: [TestTree]
testDomainSerialization =
  [ testCase "parses some example domains" $ do
      let validDomains =
            [ "wire.com",
              "some.long.example.with.lots.of.labels.com",
              "multiple-------dashes.co.uk",
              "0123456789.abcdefghijklmnopqrstuvwxyz.ABCDEFGHIJKLMNOPQRSTUVWXYZ",
              "Domain-with-dashes-and-Uppercase.Even-in-TLD",
              "1.1.1.g1", -- just TLD can't start with digit
              Text.replicate 63 "x" <> ".foo",
              Text.intercalate "." (replicate 127 "h") -- 253 chars
            ]
      for_ validDomains $ \d ->
        case mkDomain d of
          Right _ -> pure ()
          Left err -> assertFailure $ "valid domain " <> show d <> " not parsed successfully: " <> err,
    testCase "rejects invalid domains" $ do
      let invalidDomains =
            [ "dotless-domain",
              "label.starting.with.-dash.com",
              "label.ending.with.dash-.com",
              "exämple.com",
              "(comment)domain.com",
              "[1.1.1.1]", -- bracketed IP
              "192.168.1.65", -- IP
              Text.replicate 64 "x" <> ".foo",
              Text.intercalate "." (replicate 128 "h") -- 255 chars
            ]
      for_ invalidDomains $ \h ->
        case mkDomain h of
          Left _ -> pure ()
          Right parsed -> assertFailure $ "invalid domain parsed successfully: " <> show (h, parsed),
    testProperty "Arbitrary DomainText generates valid domains" $
      \(DomainText x) ->
        isRight $ mkDomain x,
    testProperty "parsing a domain normalizes it" $
      \(DomainText x) ->
        (domainText <$> mkDomain x) === Right (Text.toCaseFold x)
  ]
