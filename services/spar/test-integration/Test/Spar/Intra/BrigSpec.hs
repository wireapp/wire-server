-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Spar.Intra.BrigSpec (spec) where

import Imports hiding (head)
import Util

spec :: SpecWith TestEnv
spec = do
  describe "user deletion between brig and spar" $ do
    it "if a user gets deleted on brig, it will be deleted on spar as well." $ do
      pending
    it "if a user gets deleted on spar, it will be deleted on spar as well." $ do
      pendingWith "or deactivated?  we should decide what we want here."
