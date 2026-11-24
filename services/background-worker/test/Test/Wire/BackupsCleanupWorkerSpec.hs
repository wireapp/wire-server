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

import Control.Exception (throwIO)
import Data.Aeson (eitherDecode)
import Data.Text qualified as T
import Imports
import Polysemy
import Polysemy.State
import System.IO.Error (userError)
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

  describe "guardedCleanup" $ do
    -- the first call deletes 5, the second throws an IO exception like the Rpc layer
    let runFailing = do
          calls <- newIORef (0 :: Int)
          guardedCleanup
            ( fmap Right
                . runM
                . interpret
                  ( \CleanupBackups -> do
                      n <- embed @IO (atomicModifyIORef' calls (\k -> (k + 1, k)))
                      if n == 0
                        then pure (BackupCleanupResult 5)
                        else embed @IO (throwIO (userError "connection refused"))
                  )
            )
            backupsCleanupMaxCalls

    it "ends the run with the failure and the progress, without rethrowing" $ do
      result <- runFailing
      case result of
        Left (err, progress) -> do
          err `shouldSatisfy` T.isInfixOf "connection refused"
          progress `shouldBe` (5, 1)
        Right done -> expectationFailure ("expected a failure, got " <> show done)

    it "returns the totals when nothing fails" $ do
      result <-
        guardedCleanup
          (fmap Right . runM . interpret (\CleanupBackups -> pure (BackupCleanupResult 0)))
          backupsCleanupMaxCalls
      result `shouldBe` Right (0, 1)

  describe "BackupsCleanupConfig" $
    it "parses backupsCleanup" $ do
      let parse = eitherDecode @BackupsCleanupConfig
      parse "{\"schedule\": \"*/5 * * * *\"}" `shouldSatisfy` isRight
      parse "{\"schedule\": \"nope\"}" `shouldSatisfy` isLeft
