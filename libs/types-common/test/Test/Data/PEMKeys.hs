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

module Test.Data.PEMKeys
  ( tests,
  )
where

import Data.ByteString.Conversion
import Data.PEMKeys
import Data.String.Conversions (cs)
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "PEMKeys"
    [ testCase "ByteString conversion" $ do
        Just (cs pem) @=? (toByteString <$> fromByteString @PEMKeys pem)
    ]
  where
    pem :: ByteString
    pem =
      "-----BEGIN PRIVATE KEY-----\n"
        <> "MC4CAQAwBQYDK2VwBCIEIFANnxZLNE4p+GDzWzR3wm/v8x/0bxZYkCyke1aTRucX\n"
        <> "-----END PRIVATE KEY-----\n"
        <> "-----BEGIN PUBLIC KEY-----\n"
        <> "MCowBQYDK2VwAyEACPvhIdimF20tOPjbb+fXJrwS2RKDp7686T90AZ0+Th8=\n"
        <> "-----END PUBLIC KEY-----\n"
