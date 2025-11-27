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

module Test.FeatureFlags.AppLock where

import qualified Data.Aeson as A
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchAppLock :: (HasCallStack) => App ()
testPatchAppLock = do
  checkPatch OwnDomain "appLock"
    $ object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "appLock"
    $ object ["status" .= "disabled"]
  checkPatch OwnDomain "appLock"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "appLock"
    $ object
      [ "lockStatus" .= "unlocked",
        "config"
          .= object
            [ "enforceAppLock" .= True,
              "inactivityTimeoutSecs" .= A.Number 120
            ]
      ]
  checkPatch OwnDomain "appLock"
    $ object
      [ "config"
          .= object
            [ "enforceAppLock" .= True,
              "inactivityTimeoutSecs" .= A.Number 240
            ]
      ]
