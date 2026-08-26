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

module Test.Data.Hourglass.ConstSpec where

import Data.Hourglass
import Data.Hourglass.Const
import Imports
import Test.Hspec

spec :: Spec
spec =
  describe "Data.Hourglass.Const" $ do
    describe "midnight" $ do
      it "should represent midnight (00:00:00.000)" $ do
        let TimeOfDay (Hours h) (Minutes m) (Seconds s) ns = midnight
        h `shouldBe` 0
        m `shouldBe` 0
        s `shouldBe` 0
        ns `shouldBe` 0
