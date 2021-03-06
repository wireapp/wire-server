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
import Data.Domain (Domain (..))
import Data.Handle (Handle)
import Data.Id (Id (..), UserId)
import Data.Qualified (Qualified (..), renderQualifiedId)
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
  [ testCase "render 61a73a52-e526-4892-82a9-3d638d77629f@example.com" $ do
      uuid <-
        maybe (assertFailure "invalid UUID") pure $
          UUID.fromString "61a73a52-e526-4892-82a9-3d638d77629f"
      assertEqual "" "61a73a52-e526-4892-82a9-3d638d77629f@example.com" $
        (renderQualifiedId (Qualified (Id uuid) (Domain "example.com"))),
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
