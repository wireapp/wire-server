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

module Wire.ConversationSubsystem.MLS.Keys
  ( getMLSRemovalKey,
    SomeKeyPair (..),
    RemovalKeyError (..),
  )
where

import Data.Proxy
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Conversation.Config (ConversationSubsystemConfig (..))
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Keys

data SomeKeyPair where
  SomeKeyPair :: forall ss. (IsSignatureScheme ss) => Proxy ss -> KeyPair ss -> SomeKeyPair

-- | Why the backend has no removal key for a signature scheme.
data RemovalKeyError
  = -- | No MLS private keys are configured (see 'mlsPrivateKeyPaths').
    RemovalKeyNotConfigured
  | -- | MLS private keys are configured, but the backend cannot sign with this
    -- scheme until ML-DSA signing is implemented.
    RemovalKeyUnsupported SignatureSchemeTag
  deriving stock (Eq, Show)

getMLSRemovalKey ::
  (Member (Input ConversationSubsystemConfig) r) =>
  SignatureSchemeTag ->
  Sem r (Either RemovalKeyError SomeKeyPair)
getMLSRemovalKey ss = inputs $ \cfg -> case cfg.mlsKeys of
  Nothing -> Left RemovalKeyNotConfigured
  Just keysByPurpose -> removalKeyFor keysByPurpose.removal
  where
    removalKeyFor :: MLSPrivateKeys -> Either RemovalKeyError SomeKeyPair
    removalKeyFor keys = case ss of
      Ed25519 -> Right $ SomeKeyPair (Proxy @Ed25519) (mlsKeyPair_ed25519 keys)
      Ecdsa_secp256r1_sha256 ->
        Right $
          SomeKeyPair
            (Proxy @Ecdsa_secp256r1_sha256)
            (mlsKeyPair_ecdsa_secp256r1_sha256 keys)
      Ecdsa_secp384r1_sha384 ->
        Right $
          SomeKeyPair
            (Proxy @Ecdsa_secp384r1_sha384)
            (mlsKeyPair_ecdsa_secp384r1_sha384 keys)
      Ecdsa_secp521r1_sha512 ->
        Right $
          SomeKeyPair
            (Proxy @Ecdsa_secp521r1_sha512)
            (mlsKeyPair_ecdsa_secp521r1_sha512 keys)
      Mldsa44 -> Left (RemovalKeyUnsupported ss)
      Mldsa65 -> Left (RemovalKeyUnsupported ss)
      Mldsa87 -> Left (RemovalKeyUnsupported ss)
