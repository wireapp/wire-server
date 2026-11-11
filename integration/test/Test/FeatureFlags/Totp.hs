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

module Test.FeatureFlags.Totp where

import Test.FeatureFlags.Util
import Testlib.Prelude

testTotp :: (HasCallStack) => APIAccess -> App ()
testTotp access =
  mkFeatureTests "mfa-totp"
    & addUpdate validConfig
    & addInvalidUpdate invalidConfig
    & runFeatureTests OwnDomain access

-- | Valid update: both booleans set to True.
validConfig :: Value
validConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "require_for_all_members" .= True,
            "compulsory_for_owners_and_admins" .= True
          ]
    ]

-- | Invalid update: sends a string where a Bool is expected.
-- Verifies that the schema parser rejects the wrong type.
invalidConfig :: Value
invalidConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "require_for_all_members" .= ("yes" :: String),
            "compulsory_for_owners_and_admins" .= False
          ]
    ]

testPatchTotp :: (HasCallStack) => App ()
testPatchTotp = do
  checkPatch OwnDomain "mfa-totp"
    $ object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "mfa-totp"
    $ object ["status" .= "disabled"]
  checkPatch OwnDomain "mfa-totp"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "mfa-totp"
    $ object
      [ "lockStatus" .= "unlocked",
        "config"
          .= object
            [ "require_for_all_members" .= True,
              "compulsory_for_owners_and_admins" .= False
            ]
      ]
