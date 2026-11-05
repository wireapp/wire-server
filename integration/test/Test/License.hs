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
import Test.Meetings (withLicensedBackend, withLicensedBackendFlags)
import Testlib.Prelude

assertMeetingsFlags :: (HasCallStack, MakesValue resp) => resp -> [(String, Bool)] -> App ()
assertMeetingsFlags resp flags =
  forM_ flags $ \(flag, expected) -> do
    v <- resp %. ("meetings." <> flag)
    asBool v >>= (`shouldMatch` expected)

testLicense :: (HasCallStack) => App ()
testLicense = do
  -- Unlicensed static backend: all flags false.
  (owner, _, _) <- createTeam OwnDomain 1
  resp <- getLicense owner >>= getJSON 200
  assertMeetingsFlags
    resp
    [("invitation_links", False), ("emails_sending", False), ("calendars_integration", False)]
  -- Licensed dynamic backend: all flags true (meetings premium activated).
  withLicensedBackend $ \dom -> do
    (ownerL, _, _) <- createTeam dom 1
    respL <- getLicense ownerL >>= getJSON 200
    assertMeetingsFlags
      respL
      [("invitation_links", True), ("emails_sending", True), ("calendars_integration", True)]

-- | Mixed licence: invitation links and calendars integration licensed while
-- email sending is off — each flag is served independently, exactly as
-- licensed (review feedback on WPB-29669).
testLicenseMixedFlags :: (HasCallStack) => App ()
testLicenseMixedFlags =
  withLicensedBackendFlags True False True $ \dom -> do
    (owner, _, _) <- createTeam dom 1
    resp <- getLicense owner >>= getJSON 200
    assertMeetingsFlags
      resp
      [("invitation_links", True), ("emails_sending", False), ("calendars_integration", True)]
