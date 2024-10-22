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

module Test.Wire.API.Roundtrip.CSV where

import Control.Arrow ((>>>))
import Data.Csv
import Data.Time.Clock
import Data.Vector qualified as V
import Imports
import Test.Tasty qualified as T
import Test.Tasty.QuickCheck
import Type.Reflection (typeRep)
import Wire.API.Team.Export

newtype ValidTeamExportUser = ValidTeamExportUser
  {unValidTeamExportUser :: TeamExportUser}
  deriving newtype (FromNamedRecord, ToNamedRecord, DefaultOrdered, Eq, Show)

instance Arbitrary ValidTeamExportUser where
  arbitrary = do
    u <- arbitrary
    let resetTime (UTCTime d _) = UTCTime d 0
    pure $
      ValidTeamExportUser
        u
          { tExportLastActive = fmap resetTime (tExportLastActive u)
          }

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "CSV roundtrip tests" $
    [testRoundTrip @ValidTeamExportUser]

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

    encodeCSV :: [a] -> LByteString
    encodeCSV = encodeDefaultOrderedByName

    decodeCSV :: LByteString -> Either String [a]
    decodeCSV bstr = decodeByName bstr <&> (snd >>> V.toList)
