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

waitForMigration :: (HasCallStack) => String -> String -> App ()
waitForMigration domain name = do
  metrics <-
    getMetrics domain BackgroundWorker `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      pure $ Text.decodeUtf8 resp.body
  let (_, _, _, finishedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack (name <> "\\ ([0-9]+\\.[0-9]+)$"))
  when (finishedMatches /= [Text.pack "1.0"]) $ do
    liftIO $ threadDelay 100_000
    waitForMigration domain name
