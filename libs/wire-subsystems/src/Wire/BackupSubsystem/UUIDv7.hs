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

-- | UUIDv7 (RFC 9562) helpers for time-ordered backup file ids.
module Wire.BackupSubsystem.UUIDv7 (uuidV7, uuidV7Time) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Imports

-- | Overwrites the first 48 bits with unix millis and the version nibble with 7 (RFC 9562).
-- Input is a random (v4) UUID, its variant bits are already RFC-conformant.
uuidV7 :: UTCTime -> UUID -> UUID
uuidV7 t u =
  let (_, w2, w3, w4) = UUID.toWords u
      ms :: Word64
      ms = floor (utcTimeToPOSIXSeconds t * 1000)
      hi = fromIntegral (ms `shiftR` 16)
      mid = (fromIntegral (ms .&. 0xffff) `shiftL` 16) .|. 0x7000 .|. (w2 .&. 0x0fff)
   in UUID.fromWords hi mid w3 w4

-- | Millisecond precision.
uuidV7Time :: UUID -> UTCTime
uuidV7Time u =
  let (w1, w2, _, _) = UUID.toWords u
      ms :: Word64
      ms = (fromIntegral w1 `shiftL` 16) .|. fromIntegral (w2 `shiftR` 16)
   in posixSecondsToUTCTime (fromIntegral ms / 1000)
