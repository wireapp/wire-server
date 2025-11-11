-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.FeatureFlags.EnforceFileDownloadLocation where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchEnforceFileDownloadLocation :: (HasCallStack) => App ()
testPatchEnforceFileDownloadLocation = do
  checkPatch OwnDomain "enforceFileDownloadLocation"
    $ object ["lockStatus" .= "unlocked"]
  checkPatch OwnDomain "enforceFileDownloadLocation"
    $ object ["status" .= "enabled"]
  checkPatch OwnDomain "enforceFileDownloadLocation"
    $ object ["lockStatus" .= "unlocked", "status" .= "enabled"]
  checkPatch OwnDomain "enforceFileDownloadLocation"
    $ object ["lockStatus" .= "locked", "config" .= object []]
  checkPatch OwnDomain "enforceFileDownloadLocation"
    $ object ["config" .= object ["enforcedDownloadLocation" .= "/tmp"]]

  do
    (user, tid, _) <- createTeam OwnDomain 0
    bindResponse
      ( Internal.patchTeamFeature
          user
          tid
          "enforceFileDownloadLocation"
          (object ["config" .= object ["enforcedDownloadLocation" .= ""]])
      )
      $ \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "empty-download-location"

testEnforceDownloadLocation :: (HasCallStack) => APIAccess -> App ()
testEnforceDownloadLocation access = do
  mkFeatureTests
    "enforceFileDownloadLocation"
    & addUpdate
      ( object
          [ "status" .= "enabled",
            "config" .= object ["enforcedDownloadLocation" .= "/tmp"]
          ]
      )
    & addUpdate
      (object ["status" .= "disabled", "config" .= object []])
    & addInvalidUpdate
      ( object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "enforcedDownloadLocation" .= object []
                ]
          ]
      )
    & runFeatureTests OwnDomain access
