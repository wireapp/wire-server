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

module Test.TOTP.OtherRequestBodies where

import qualified API.Brig as Brig
import API.Common (defPassword)
import SetupHelpers
import Test.TOTP.MockBodies
import Testlib.Prelude

-- One endpoint stands in for all of them.  Which bodies accept a totp_code,
-- and that a malformed one is rejected, is settled by the schemas and asserted
-- per type in the unit test (@Test.Wire.API.User.Auth.TOTP@); what is left for
-- an integration test is that a handler which already authenticates the caller
-- neither chokes on the new field nor lets it replace the password.

testMockTotpChangePassword :: (HasCallStack) => App ()
testMockTotpChangePassword = do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  let newPassword = "new-test-password"
      body =
        object
          [ "old_password" .= defPassword,
            "new_password" .= newPassword,
            "totp_code" .= mockLoginTotpCode
          ]
      request = Brig.putPasswordWithBody alice
  -- Do not accept a 4xx from business logic as proof that parsing worked:
  -- these must all fail with 400 before a valid request can mutate the state.
  for_ [toJSON ("123456" :: String), toJSON ("totp:abc" :: String), toJSON ("recovery:invalid" :: String), Number 123456, Bool True] $ \code ->
    setField "totp_code" code body >>= request >>= assertStatus 400
  setField "old_password" ("wrong-password" :: String) body >>= request >>= assertStatus 403
  request body >>= assertStatus 200
  Brig.login alice email newPassword >>= assertStatus 200
