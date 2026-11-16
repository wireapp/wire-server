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

module Test.TOTP.Login where

import qualified API.Brig as Brig
import API.Common (defPassword)
import SetupHelpers
import Test.TOTP.MockBodies
import Testlib.Prelude

-- | Login with a totp_code field succeeds: the mock parses the field and
-- ignores it.
testLoginWithValidTotpCode :: (HasCallStack) => App ()
testLoginWithValidTotpCode = do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  Brig.loginWithTotp alice email defPassword mockLoginTotpCode >>= assertStatus 200

-- | Login with a malformed totp_code value (not a valid SecondFactorCode
-- format) returns 400 — parsing is real even though enforcement is not.
testLoginWithInvalidTotpCode :: (HasCallStack) => App ()
testLoginWithInvalidTotpCode = do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  req <- baseRequest alice Brig Versioned "login"
  submit
    "POST"
    ( req
        & addJSONObject ["email" .= email, "password" .= defPassword, "totp_code" .= ("not-a-valid-totp-code" :: String)]
        & addQueryParams [("persist", "true")]
    )
    >>= assertStatus 400
