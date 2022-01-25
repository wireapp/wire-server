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

module Test.Spar.Roundtrip.ByteString where

import Arbitrary ()
import Data.ByteString.Conversion
import Imports
import Test.Hspec
import Test.QuickCheck
import Type.Reflection (typeRep)

testRoundTrip ::
  forall a.
  (Arbitrary a, Typeable a, ToByteString a, FromByteString a, Eq a, Show a) =>
  Spec
testRoundTrip = it msg $ property trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample (show $ toByteString' v) $
        Just v === (fromByteString . toByteString') v
