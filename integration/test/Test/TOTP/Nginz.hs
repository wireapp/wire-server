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

module Test.TOTP.Nginz where

import qualified API.Brig as Brig
import API.Common (defPassword)
import SetupHelpers
import Testlib.Prelude

testTotpThroughNginz :: (HasCallStack) => App ()
testTotpThroughNginz = do
  alice <- randomUser OwnDomain def
  email <- asString (alice %. "email")
  token <- do
    resp <- Brig.login alice email defPassword >>= getJSON 200
    asString (resp %. "access_token")

  -- authenticated: the request reaches brig
  do
    req <-
      rawBaseRequest alice Nginz Versioned "/self/totp"
        <&> addHeader "Authorization" ("Bearer " <> token)
    bindResponse (submit "GET" req) $ \resp -> do
      resp.status `shouldMatchInt` 200
      void $ asList resp.json

  -- a bad token: nginz rejects it before brig sees anything
  do
    req <-
      rawBaseRequest alice Nginz Versioned "/self/totp"
        <&> addHeader "Authorization" "Bearer not-a-token"
    submit "GET" req >>= assertStatus 401

  -- no credentials at all
  do
    req <- rawBaseRequest alice Nginz Versioned "/self/totp"
    bindResponse (submit "GET" req) $ \resp ->
      assertBool
        ("unauthenticated request was not rejected: " <> show resp.status)
        (resp.status >= 400 && resp.status < 500)
