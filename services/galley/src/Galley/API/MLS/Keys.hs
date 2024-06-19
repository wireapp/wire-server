-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.MLS.Keys (getMLSRemovalKey, SomeKeyPair (..)) where

import Control.Error.Util (hush)
import Control.Lens (view)
import Data.Proxy
import Galley.Env
import Imports hiding (getFirst)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Keys

data SomeKeyPair where
  SomeKeyPair :: forall ss. (IsSignatureScheme ss) => Proxy ss -> KeyPair ss -> SomeKeyPair

getMLSRemovalKey ::
  (Member (Input Env) r) =>
  SignatureSchemeTag ->
  Sem r (Maybe SomeKeyPair)
getMLSRemovalKey ss = fmap hush . runError @() $ do
  keysByPurpose <- note () =<< inputs (view mlsKeys)
  let keys = keysByPurpose.removal
  case ss of
    Ed25519 -> pure $ SomeKeyPair (Proxy @Ed25519) (mlsKeyPair_ed25519 keys)
    Ecdsa_secp256r1_sha256 ->
      pure $
        SomeKeyPair
          (Proxy @Ecdsa_secp256r1_sha256)
          (mlsKeyPair_ecdsa_secp256r1_sha256 keys)
    Ecdsa_secp384r1_sha384 ->
      pure $
        SomeKeyPair
          (Proxy @Ecdsa_secp384r1_sha384)
          (mlsKeyPair_ecdsa_secp384r1_sha384 keys)
    Ecdsa_secp521r1_sha512 ->
      pure $
        SomeKeyPair
          (Proxy @Ecdsa_secp521r1_sha512)
          (mlsKeyPair_ecdsa_secp521r1_sha512 keys)
