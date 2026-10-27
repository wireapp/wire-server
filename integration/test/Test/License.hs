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

module Test.License where

import API.Galley
import SetupHelpers
import Test.Meetings (withLicensedBackend)
import Testlib.Prelude

testLicense :: (HasCallStack) => App ()
testLicense = do
  -- Unlicensed static backend: all flags false.
  (owner, _, _) <- createTeam OwnDomain 1
  resp <- getLicense owner >>= getJSON 200
  forM_ ["invitation_links", "emails_sending", "calendars_integration"] $ \flag -> do
    v <- resp %. ("meetings." <> flag)
    asBool v >>= (`shouldMatch` False)
  -- Licensed dynamic backend: all flags true (meetings premium activated).
  withLicensedBackend $ \dom -> do
    (ownerL, _, _) <- createTeam dom 1
    respL <- getLicense ownerL >>= getJSON 200
    forM_ ["invitation_links", "emails_sending", "calendars_integration"] $ \flag -> do
      v <- respL %. ("meetings." <> flag)
      asBool v >>= (`shouldMatch` True)
