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

module Wire.TeamNotificationStoreSpec (spec) where

import Data.Id
import Data.Time
import Data.UUID.V4 qualified as UUID
import Imports
import Test.Hspec
import Wire.API.Notification qualified as API
import Wire.TeamNotificationStore.Postgres (notificationIdTime)

spec :: Spec
spec = do
  now <- runIO getCurrentTime
  freshNotificationId <- runIO (API.mkNotificationId @IO)
  describe "notificationIdTime" $ do
    it "decodes the v1 timestamp of a freshly generated notification id" $ do
      let t = notificationIdTime freshNotificationId
      fmap (abs . diffUTCTime now) t `shouldSatisfy` maybe False (< 300)
    it "returns Nothing for a v4 (random) UUID" $ do
      v4 <- UUID.nextRandom
      notificationIdTime (Id v4) `shouldBe` Nothing
