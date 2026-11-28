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

module Wire.FanInAckStore.PostgresSpec (spec) where

import Data.Domain (Domain (..))
import Data.Id
import Data.UUID qualified as UUID
import Hasql.Pool (UsageError)
import Imports
import Polysemy
import Polysemy.Error
import Test.Hspec
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.Notification (NotificationId)
import Wire.FanInAckStore
import Wire.FanInAckStore.Postgres

type TestEffects = '[FanInAckStore, Error FanInAckStoreError, Error UsageError, Embed IO]

-- The pool must not be evaluated on invalid input. These tests exercise the
-- actual interpreter's write boundary without needing a database or service.
-- Cursor advancement/reset and row isolation need the later delivery round trip.
spec :: Spec
spec = describe "Fan-in acknowledgement write validation" $
  for_ operations $ \(name, operation) -> describe name $
    for_ keys $ \key -> describe (show key) $
      for_ invalidIds $ \nid -> it ("rejects " <> show nid <> " before accessing PostgreSQL") $ do
        result <- runM . runError @UsageError . runError @FanInAckStoreError . interpretFanInAckStoreToPostgres (error "Invalid cursor accessed the notification database") $ operation key nid
        result `shouldBe` Right (Left (InvalidFanInAckNotificationId nid))

operations :: [(String, FanInAckKey -> NotificationId -> Sem TestEffects ())]
operations = [("advance", advanceFanInAck), ("explicit reset", upsertFanInAck), ("initialization", \key value -> void (initializeFanInAck key value))]

keys :: [FanInAckKey]
keys =
  [ FanInUserAck uid (ClientId 1),
    FanInClientAck uid (ClientId 1),
    FanInTeamAck uid (ClientId 1) (Id UUID.nil),
    FanInEpochAck uid (ClientId 1) (GroupId "group") (Epoch 7),
    FanInLocalConnectionAck uid connected,
    FanInRemoteConnectionAck uid (Domain "remote.example.com") connected
  ]
  where
    uid = Id UUID.nil
    connected = Id (UUID.fromWords 0 0 0 1)

invalidIds :: [NotificationId]
invalidIds =
  [ Id UUID.nil,
    Id (UUID.fromWords 0 0x1000 0x80000000 1), -- UUIDv1
    Id (UUID.fromWords 0 0x4000 0x80000000 1), -- UUIDv4
    Id (UUID.fromWords 0 0x7000 0xc0000000 1) -- Invalid RFC variant
  ]
