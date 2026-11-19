-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foun(Domain "a.com")tion, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.FanInNotificationStore.TimestampSpec (spec) where

import Data.Aeson qualified as JSON
import Data.Bits ((.&.), (.|.))
import Data.Id (Id (..))
import Data.Time (UTCTime (..), fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Wire.FanInNotificationStore

spec :: Spec
spec = describe "UUIDv7 notification partition timestamps" $ do
  it "decodes the RFC 9562 appendix A.6 reference vector" $
    fanInNotificationCreatedAt (notification "017f22e2-79b0-7cc3-98c4-dc0c0c07398f")
      `shouldBe` Right (UTCTime (fromGregorian 2022 2 22) (19 * 3600 + 22 * 60 + 22))
  it "preserves a single millisecond after the Unix epoch" $
    fanInNotificationCreatedAt (notification "00000000-0001-7000-8000-000000000000")
      `shouldBe` Right (UTCTime (fromGregorian 1970 1 1) 0.001)
  it "reads timestamp bits from both of the first two words" $ do
    fanInNotificationCreatedAt (notification "00000000-ffff-7000-8000-000000000000")
      `shouldBe` Right (posixSecondsToUTCTime 65.535)
    fanInNotificationCreatedAt (notification "00000001-0000-7000-8000-000000000000")
      `shouldBe` Right (posixSecondsToUTCTime 65.536)
  it "preserves the maximum 48-bit timestamp without overflow or rounding" $
    fanInNotificationCreatedAt (notification "ffffffff-ffff-7fff-bfff-ffffffffffff")
      `shouldBe` Right (posixSecondsToUTCTime 281474976710.655)
  it "allows later ids to share the cursor's partition timestamp" $ do
    let cursor = (notification "017f22e2-79b0-7000-8000-000000000000").fanInNotificationId
        later = (notification "017f22e2-79b0-7fff-bfff-ffffffffffff").fanInNotificationId
    later `shouldSatisfy` (> cursor)
    fanInNotificationIdCreatedAt later `shouldBe` fanInNotificationIdCreatedAt cursor
  prop "a later UUIDv7 never has an earlier partition timestamp" $ \words1 words2 ->
    let first = uuidV7 words1
        second = uuidV7 words2
     in case (fanInNotificationIdCreatedAt (max first second), fanInNotificationIdCreatedAt (min first second)) of
          (Right later, Right earlier) -> later >= earlier
          _ -> False
  it "rejects legacy and random UUID versions" $
    for_ ["017f22e2-79b0-1cc3-98c4-dc0c0c07398f", "017f22e2-79b0-4cc3-98c4-dc0c0c07398f"] $ \value -> do
      let n = notification value
      fanInNotificationCreatedAt n `shouldBe` Left (InvalidFanInNotificationId n.fanInNotificationId)
  it "rejects non-RFC variants even when the version bits are seven" $
    for_ ["017f22e2-79b0-7cc3-18c4-dc0c0c07398f", "017f22e2-79b0-7cc3-d8c4-dc0c0c07398f", "017f22e2-79b0-7cc3-f8c4-dc0c0c07398f"] $ \value -> do
      let n = notification value
      fanInNotificationCreatedAt n `shouldBe` Left (InvalidFanInNotificationId n.fanInNotificationId)

notification :: String -> FanInNotification
notification value = FanInNotification (Id (fromJust (UUID.fromString value))) (JSON.object []) Nothing

-- Preserve arbitrary timestamp and random bits while fixing the version and variant.
uuidV7 :: (Word32, Word32, Word32, Word32) -> Id a
uuidV7 (high, low, variant, randomBits) =
  Id (UUID.fromWords high ((low .&. 0xffff0fff) .|. 0x7000) ((variant .&. 0x3fffffff) .|. 0x80000000) randomBits)
