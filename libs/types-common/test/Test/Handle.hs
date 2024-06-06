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

module Test.Handle
  ( tests,
  )
where

import Data.Handle (BadHandle (fromBadHandle), Handle (fromHandle), parseHandleEither)
import Data.Text qualified as Text
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Handle"
    [ testGroup "serialization" testHandleSerialization
    ]

testHandleSerialization :: [TestTree]
testHandleSerialization =
  [ testCase "parses some example handles" $ do
      let validHandles =
            [ "handle",
              Text.replicate 256 "a",
              "--",
              "__",
              "..",
              "0123456789"
            ]
      for_ validHandles $ \h ->
        case parseHandleEither h of
          Right _ -> pure ()
          Left err -> assertFailure $ "valid handle " <> show h <> " not parsed successfully: " <> err,
    testCase "rejects invalid handles" $ do
      let invalidHandles =
            [ "h", -- too short
              Text.replicate 257 "a", -- too long
              "myhändle",
              "myhàndle",
              "myh@ndle",
              "$pecial",
              "some+name",
              "upperCase",
              "with space"
            ]
      for_ invalidHandles $ \h ->
        case parseHandleEither h of
          Left _ -> pure ()
          Right parsed -> assertFailure $ "invalid handle parsed successfully: " <> show (h, parsed),
    testProperty "roundtrip for Handle" $
      \(x :: Handle) ->
        parseHandleEither (fromHandle x) === Right x,
    testProperty "roundtrip for BadHandle" $
      \(x :: BadHandle) ->
        property . isLeft . parseHandleEither $ fromBadHandle x
  ]
