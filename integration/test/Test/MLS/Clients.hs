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

module Test.MLS.Clients where

import qualified API.BrigInternal as I
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testGetMLSClients :: (HasCallStack) => App ()
testGetMLSClients = do
  alice <- randomUser OwnDomain def
  alice1 <- createWireClient alice def

  bindResponse (I.getMLSClients alice def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    cs <- resp.json & asList
    c <- assertOne cs
    c
      `shouldMatch` object
        [ "mls" .= False,
          "id" .= alice1.client
        ]

  keys <- initMLSClient def alice1
  ss <- keys %. csSignatureScheme def

  bindResponse (I.getMLSClients alice def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    cs <- resp.json & asList
    c <- assertOne cs
    c
      `shouldMatch` object
        [ "mls" .= False,
          "id" .= alice1.client,
          "mls_signature_key" .= ss
        ]

  void $ uploadNewKeyPackage def alice1

  bindResponse (I.getMLSClients alice def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    cs <- resp.json & asList
    c <- assertOne cs
    c
      `shouldMatch` object
        [ "mls" .= True,
          "id" .= alice1.client,
          "mls_signature_key" .= ss
        ]
