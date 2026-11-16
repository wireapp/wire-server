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

module Test.TOTP.VersionGate where

import SetupHelpers
import Test.TOTP.Helpers (credentialIdPlaceholder)
import Testlib.Prelude

testTotpNotOnV18 :: (HasCallStack) => App ()
testTotpNotOnV18 = do
  alice <- randomUser OwnDomain def
  let routes =
        [ ("POST", "/self/totp"),
          ("GET", "/self/totp"),
          ("POST", "/self/totp/" <> credentialIdPlaceholder <> "/activate"),
          ("PUT", "/self/totp/" <> credentialIdPlaceholder),
          ("DELETE", "/self/totp/" <> credentialIdPlaceholder)
        ]
  for_ routes $ \(method, path) -> do
    req <- baseRequest alice Brig (ExplicitVersion 18) path
    bindResponse (submit method (req & addJSONObject ["name" .= ("work phone" :: String)])) $ \resp ->
      resp.status `shouldMatchOneOf` [Number 404, Number 405]
