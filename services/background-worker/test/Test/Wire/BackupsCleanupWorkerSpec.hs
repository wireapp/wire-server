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

module Test.Wire.BackupsCleanupWorkerSpec (spec) where

import Data.Aeson (eitherDecode)
import Imports
import Polysemy
import Polysemy.State
import Test.Hspec
import Wire.API.Backup (BackupCleanupResult (..))
import Wire.BackgroundWorker.Options (BackupsCleanupConfig)
import Wire.BackupsCleanupWorker
import Wire.CargoholdAPIAccess

-- | Every cleanup call pops the next number of deleted files; 0 when there are none left.
runCleanups :: [Word32] -> Sem '[CargoholdAPIAccess] a -> a
runCleanups results =
  run
    . evalState results
    . interpret
      ( \CleanupBackups ->
          get >>= \case
            [] -> pure (BackupCleanupResult 0)
            n : rest -> put rest $> BackupCleanupResult n
      )
    . raiseUnder

spec :: Spec
spec = do
  describe "cleanupBackupsLoop" $ do
    it "stops after the first call that deletes nothing" $
      runCleanups [5, 3, 0, 7] (cleanupBackupsLoop backupsCleanupMaxCalls) `shouldBe` (8, 3)

    it "stops after the maximal number of calls" $
      runCleanups (replicate 30 1) (cleanupBackupsLoop backupsCleanupMaxCalls) `shouldBe` (20, 20)

    it "calls at least once" $
      runCleanups [] (cleanupBackupsLoop backupsCleanupMaxCalls) `shouldBe` (0, 1)

  describe "BackupsCleanupConfig" $
    it "parses backupsCleanup" $ do
      let parse = eitherDecode @BackupsCleanupConfig
      parse "{\"schedule\": \"*/5 * * * *\"}" `shouldSatisfy` isRight
      parse "{\"schedule\": \"nope\"}" `shouldSatisfy` isLeft
