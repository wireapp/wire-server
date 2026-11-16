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

module Test.TOTP.Helpers where

import qualified API.Brig as Brig
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.CaseInsensitive as CI
import Test.TOTP.MockBodies
import Testlib.Prelude

someCredentialId :: (HasCallStack, MakesValue user) => user -> App String
someCredentialId user = do
  resp <- Brig.enrolTotp user mockEnrolBody >>= getJSON 201
  asString (resp %. "credential_id")

credentialIdPlaceholder :: String
credentialIdPlaceholder = "0f2e1b9c-6c6c-4a0e-9a3f-17a2cf15a001"

responseHeader :: String -> Response -> Maybe ByteString
responseHeader name resp =
  snd <$> find ((CI.mk (BSChar8.pack name) ==) . fst) resp.headers

-- | @xxxx-xxxx-xxxx-xxxx@ in Crockford base32 (no I, L, O, U).
isRecoveryCodeShaped :: String -> Bool
isRecoveryCodeShaped s =
  (length groups == 4)
    && all ((== 4) . length) groups
    && all (all (`elem` crockford)) groups
  where
    crockford :: String
    crockford = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

    groups = splitOnDash s
    splitOnDash t = case break (== '-') t of
      (g, []) -> [g]
      (g, _ : rest) -> g : splitOnDash rest
