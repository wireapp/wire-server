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

module Wire.BackupSubsystem.DisabledSpec (spec) where

import Data.Domain
import Data.Id
import Data.Qualified
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Test.Hspec
import Wire.API.Backup
import Wire.BackupSubsystem
import Wire.BackupSubsystem.Disabled

runDisabled :: Sem '[BackupSubsystem, Error BackupError] a -> Either BackupError a
runDisabled = run . runError . interpretBackupSubsystemDisabled

u :: UserId
u = Id (UUID.fromWords64 0 1)

lu :: Local UserId
lu = toLocalUnsafe (Domain "example.com") u

c :: ClientId
c = ClientId 0xa1

e :: BackupEpoch
e = BackupEpoch 1

spec :: Spec
spec = describe "BackupSubsystem disabled" $ do
  it "list epochs rejected" $
    void (runDisabled (listEpochs lu c c)) `shouldBe` Left BackupErrFeatureDisabled
  it "upload rejected" $
    void (runDisabled (uploadBackup lu c c e 1 (pure ()))) `shouldBe` Left BackupErrFeatureDisabled
  it "list files rejected" $
    void (runDisabled (listFiles lu c c e)) `shouldBe` Left BackupErrFeatureDisabled
  it "download rejected" $
    void (runDisabled (downloadUrl lu c c e BackupFull (BackupFileId UUID.nil)))
      `shouldBe` Left BackupErrFeatureDisabled
  it "deletes are no-ops" $
    runDisabled
      ( do
          deleteEpochBackups lu c c e
          deleteClientBackups lu c c
          internalDeleteClientBackups u c
          internalDeleteUserBackups u
      )
      `shouldBe` Right ()
