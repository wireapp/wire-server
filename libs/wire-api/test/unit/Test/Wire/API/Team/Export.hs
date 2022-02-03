{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Wire.API.Team.Export where

import Data.ByteString.Arbitrary
import qualified Data.ByteString.Char8 as C
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (conjoin, counterexample, testProperty, (.||.), (===))
import Wire.API.Team.Export

tests :: TestTree
tests =
  testGroup
    "Export"
    [ testTrivialExamples,
      testRoundTrip,
      testUnquotedProp,
      testQuotedProp
    ]

testTrivialExamples :: TestTree
testTrivialExamples = testCase "quoted/unquoted examples" $ do
  unquoted "'foobar" @?= "foobar"
  unquoted "foobar" @?= "foobar"
  unquoted "" @?= ""
  quoted "" @?= ""
  quoted "foobar" @?= "foobar"
  quoted "=1+2" @?= "'=1+2"

testRoundTrip :: TestTree
testRoundTrip = testProperty "quoted roundtrip" prop
  where
    prop (ABS bs) = counterexample (show $ quoted bs) $ bs === (unquoted . quoted) bs

testUnquotedProp :: TestTree
testUnquotedProp = testProperty "unquoted arbitrary" prop
  where
    prop (ABS bs) = counterexample (show $ unquoted bs) $ (bs === unquoted bs) .||. startsWithSingleQuote bs
    startsWithSingleQuote bs = case C.uncons bs of
      Just ('\'', _) -> True
      _ -> False

testQuotedProp :: TestTree
testQuotedProp = testProperty "quoted" prop
  where
    prop (ABS bs) = counterexample (show $ quoted bs) $ conjoin (checkQuoted bs <$> disallowedChars)
    checkQuoted bs char = quoted (char `C.cons` bs) === '\'' `C.cons` char `C.cons` bs
    disallowedChars = ['@', '+', '-', '=', '\'', '\x0009', '\x000D']
