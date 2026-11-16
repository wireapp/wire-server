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

module Test.TOTP.Routes where

import qualified API.Brig as Brig
import SetupHelpers
import Test.TOTP.Helpers (isRecoveryCodeShaped, someCredentialId)
import Test.TOTP.MockBodies
import Testlib.Prelude

testTotpActivate :: (HasCallStack) => App ()
testTotpActivate = do
  alice <- randomUser OwnDomain def
  cid <- someCredentialId alice
  bindResponse (Brig.activateTotp alice cid mockActivateBody) $ \resp -> do
    resp.status `shouldMatchInt` 200
    codes <- resp.json %. "codes" & asList
    for_ codes $ \c -> do
      code <- asString c
      assertBool
        ("recovery code has the wrong shape: " <> code)
        (isRecoveryCodeShaped code)

testTotpList :: (HasCallStack) => App ()
testTotpList = do
  alice <- randomUser OwnDomain def
  bindResponse (Brig.listTotp alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    creds <- asList resp.json
    for_ creds $ \c -> do
      void $ asString (c %. "id")
      void $ asString (c %. "name")
      void $ asString (c %. "created_at")
      -- the secret and the URI are returned by enrolment and never again
      lookupField c "secret" `shouldMatch` noValue
      lookupField c "uri" `shouldMatch` noValue

testTotpRename :: (HasCallStack) => App ()
testTotpRename = do
  alice <- randomUser OwnDomain def
  cid <- someCredentialId alice
  Brig.renameTotp alice cid mockRenameBody >>= assertStatus 200

testTotpDelete :: (HasCallStack) => App ()
testTotpDelete = do
  alice <- randomUser OwnDomain def
  cid <- someCredentialId alice
  Brig.deleteTotp alice cid mockDeleteBody >>= assertStatus 200
