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

module Test.Brig.Roundtrip where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import Data.ByteString.Conversion
import Data.Swagger (ToSchema, validatePrettyToJSON)
import Imports
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty, (.&&.), (===))
import Type.Reflection (typeRep)

-- FUTUREWORK: make this an alias for 'testRoundTripWithSwagger' (or just remove the latter).
testRoundTrip ::
  forall a.
  (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a) =>
  TestTree
testRoundTrip = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample (show $ toJSON v) $
        Right v === (parseEither parseJSON . toJSON) v

testRoundTripWithSwagger ::
  forall a.
  (Arbitrary a, Typeable a, ToJSON a, FromJSON a, ToSchema a, Eq a, Show a) =>
  TestTree
testRoundTripWithSwagger = testProperty msg (trip .&&. scm)
  where
    msg = show (typeRep @a)

    trip (v :: a) =
      counterexample (show $ toJSON v) $
        Right v === (parseEither parseJSON . toJSON) v

    scm (v :: a) =
      counterexample
        ( fromMaybe "Schema validation failed, but there were no errors. This looks like a bug in swagger2!" $
            validatePrettyToJSON v
        )
        $ isNothing (validatePrettyToJSON v)

testRoundTripByteString ::
  forall a.
  (Arbitrary a, Typeable a, ToByteString a, FromByteString a, Eq a, Show a) =>
  TestTree
testRoundTripByteString = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample (show $ toByteString' v) $
        Just v === (fromByteString . toByteString') v
