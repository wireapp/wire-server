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

module Test.Migration.Util where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Stack
import SetupHelpers hiding (deleteUser)
import Testlib.Prelude
import Text.Regex.TDFA ((=~))
import UnliftIO

waitForMigration :: (HasCallStack) => String -> String -> App ()
waitForMigration domain metricName =
  maybe failWithContext pure =<< timeout 30_000_000 go
  where
    failWithContext = do
      getMetrics domain BackgroundWorker `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        assertFailure "Timed out waiting for postgresql migration"
    go = do
      metrics <-
        getMetrics domain BackgroundWorker `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          pure $ Text.decodeUtf8 resp.body
      let (_, _, _, finishedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack (metricName <> "\\ ([0-9]+\\.[0-9]+)$"))
      when (finishedMatches /= [Text.pack "1.0"]) $ do
        liftIO $ threadDelay 100_000
        go

assertMigrationSuccessful :: (HasCallStack) => String -> String -> App ()
assertMigrationSuccessful domain failedMetricName = do
  getMetrics domain BackgroundWorker `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    let metrics = Text.decodeUtf8 resp.body
        (_, _, _, failedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack (failedMetricName <> "\\ ([0-9]+\\.[0-9]+)$"))
    failedMatches `shouldMatch` [Text.pack "0.0"]
