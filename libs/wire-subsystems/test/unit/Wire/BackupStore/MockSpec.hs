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

module Wire.BackupStore.MockSpec (spec) where

import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as Map
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.State
import Test.Hspec
import Wire.API.Backup
import Wire.BackupStore
import Wire.BackupSubsystem.Rules
import Wire.MockInterpreters

limits :: BackupLimits
limits =
  BackupLimits
    { maxFullBytes = 1073741824,
      maxIncrementalBytes = 10485760,
      maxFilesPerEpoch = 3650
    }

spec :: Spec
spec = describe "BackupStore mock" $ do
  it "concurrent-style insert re-checks rules" $ do
    let uid = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))
        cid = ClientId 1
        fid n = BackupFileId (UUID.fromWords 0 0 0 n)
        existing = BackupFile {id = fid 1, kind = BackupFull, size = 1, createdAt = toUTCTimeMillis defaultTime}
        st0 = Map.singleton (fid 1) (uid, cid, BackupEpoch 1, existing)
        new = NewBackupFile {id = fid 2, user = uid, client = cid, epoch = BackupEpoch 1, size = 20 * 1024 * 1024}
        (st, res) =
          run . interpretNowConst defaultTime . runState st0 . inMemoryBackupStoreInterpreter $
            insertBackupFile limits new
    res `shouldBe` Left TooLarge
    st `shouldBe` st0
