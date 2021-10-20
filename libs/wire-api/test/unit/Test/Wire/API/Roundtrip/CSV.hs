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

module Test.Wire.API.Roundtrip.CSV where

import Control.Arrow ((>>>))
import Data.Csv
import qualified Data.Vector as V
import Imports
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty, (===))
import Type.Reflection (typeRep)
import qualified Wire.API.Team.Export as Team.Export

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "CSV roundtrip tests" $
    [testRoundTrip @Team.Export.TeamExportUser]

testRoundTrip ::
  forall a.
  (Arbitrary a, Typeable a, ToNamedRecord a, FromNamedRecord a, DefaultOrdered a, Eq a, Show a) =>
  T.TestTree
testRoundTrip = testProperty msg trip
  where
    msg = show (typeRep @[a])

    trip (v :: [a]) =
      counterexample (show $ encodeCSV v) $
        Right v === (decodeCSV . encodeCSV) v

    encodeCSV :: (DefaultOrdered a, ToNamedRecord a) => [a] -> LByteString
    encodeCSV = encodeDefaultOrderedByName

    decodeCSV :: FromNamedRecord a => LByteString -> Either String [a]
    decodeCSV bstr = decodeByName bstr <&> (snd >>> V.toList)
