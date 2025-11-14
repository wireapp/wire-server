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

module Test.FeatureFlags.Channels where

import Test.FeatureFlags.Util
import Testlib.Prelude

testChannels :: (HasCallStack) => APIAccess -> App ()
testChannels access =
  mkFeatureTests "channels"
    & addUpdate validConfig
    & addInvalidUpdate invalidConfig
    & runFeatureTests OwnDomain access

validConfig :: Value
validConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "allowed_to_create_channels" .= "everyone",
            "allowed_to_open_channels" .= "everyone"
          ]
    ]

invalidConfig :: Value
invalidConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "allowed_to_create_channels" .= "everyone",
            "allowed_to_open_channels" .= "INVALID"
          ]
    ]

testPatchChannels :: (HasCallStack) => App ()
testPatchChannels = do
  checkPatch OwnDomain "channels"
    $ object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "channels"
    $ object ["status" .= "disabled"]
  checkPatch OwnDomain "channels"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "channels"
    $ object
      [ "lockStatus" .= "unlocked",
        "config"
          .= object
            [ "allowed_to_create_channels" .= "admins",
              "allowed_to_open_channels" .= "everyone"
            ]
      ]
