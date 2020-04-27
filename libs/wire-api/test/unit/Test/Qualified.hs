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

module Test.Qualified
  ( tests,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Conversion as BS.C
import Data.Domain (Domain (Domain))
import Data.Handle (Handle (Handle, fromHandle))
import Data.Id (Id (Id, toUUID), UserId)
import Data.Qualified (OptionallyQualified, Qualified (Qualified), eitherQualifiedOrNot, mkQualifiedHandle, mkQualifiedId, renderQualifiedHandle, renderQualifiedId)
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as Text.E
import qualified Data.UUID as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Type.Reflection (typeRep)

tests :: TestTree
tests =
  testGroup
    "Qualified"
    [ testGroup "serialization" testQualifiedSerialization
    ]

testQualifiedSerialization :: [TestTree]
testQualifiedSerialization =
  [ testCase "render foo@bar.com" $ do
      assertEqual "" "foo@bar.com" $
        (renderQualifiedHandle (Qualified (Handle "foo") (Domain "bar.com"))),
    testCase "render 61a73a52-e526-4892-82a9-3d638d77629f@example.com" $ do
      uuid <-
        maybe (assertFailure "invalid UUID") pure $
          UUID.fromString "61a73a52-e526-4892-82a9-3d638d77629f"
      assertEqual "" "61a73a52-e526-4892-82a9-3d638d77629f@example.com" $
        (renderQualifiedId (Qualified (Id uuid) (Domain "example.com"))),
    testProperty "roundtrip for Qualified Handle" $
      \(x :: Qualified Handle) ->
        mkQualifiedHandle (renderQualifiedHandle x) === Right x,
    testProperty "roundtrip for Qualified UserId" $
      \(x :: Qualified UserId) ->
        mkQualifiedId (renderQualifiedId x) === Right x,
    testProperty "roundtrip for OptionallyQualified Handle" $
      \(x :: OptionallyQualified Handle) -> do
        let render = Text.E.encodeUtf8 . either fromHandle renderQualifiedHandle . eitherQualifiedOrNot
        let parse = BS.C.runParser BS.C.parser
        parse (render x) === Right x,
    testProperty "roundtrip for OptionallyQualified UserId" $
      \(x :: OptionallyQualified UserId) -> do
        let render = Text.E.encodeUtf8 . either (cs . UUID.toString . toUUID) renderQualifiedId . eitherQualifiedOrNot
        let parse = BS.C.runParser BS.C.parser
        parse (render x) === Right x,
    jsonRoundtrip @(Qualified Handle),
    jsonRoundtrip @(Qualified UserId)
  ]

jsonRoundtrip ::
  forall a.
  (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a) =>
  TestTree
jsonRoundtrip = testProperty msg trip
  where
    msg = "jsonRoundTrip @(" <> show (typeRep @a) <> ")"
    trip (v :: a) =
      counterexample (show $ toJSON v) $
        Right v === (Aeson.parseEither parseJSON . toJSON) v
