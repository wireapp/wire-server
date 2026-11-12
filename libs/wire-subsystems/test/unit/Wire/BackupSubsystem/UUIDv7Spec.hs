-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.BackupSubsystem.UUIDv7Spec (spec) where

import Data.Bits (shiftR)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.UUID.Util qualified as UUID
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.BackupSubsystem.UUIDv7

newtype U = U UUID.UUID deriving (Show)

instance Arbitrary U where
  arbitrary = U <$> (UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

-- | Arbitrary time with sub-millisecond part, years 1970..~2527.
newtype T = T UTCTime deriving (Show)

instance Arbitrary T where
  arbitrary = T . posixSecondsToUTCTime . (/ 1000000) . fromInteger <$> choose (0, 17000000000000000)

truncMs :: UTCTime -> UTCTime
truncMs t = posixSecondsToUTCTime (fromInteger (floor (utcTimeToPOSIXSeconds t * 1000)) / 1000)

spec :: Spec
spec = describe "uuidV7" $ do
  prop "sets version 7" $ \(T t) (U u) ->
    UUID.version (uuidV7 t u) === 7
  prop "keeps variant bits" $ \(T t) (U u) ->
    let w = UUID.toWords (uuidV7 t u)
        (_, _, w3, _) = w
     in (w3 `shiftR` 30) === (let (_, _, x, _) = UUID.toWords u in x `shiftR` 30)
  prop "uuidV7Time truncates to ms" $ \(T t) (U u) ->
    uuidV7Time (uuidV7 t u) === truncMs t
  prop "ordered by time" $ \(T t1) (T t2) (U a) (U b) ->
    truncMs t1 < truncMs t2 ==>
      (uuidV7 t1 a < uuidV7 t2 b)
        .&&. (UUID.toText (uuidV7 t1 a) < UUID.toText (uuidV7 t2 b))
