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

-- | Rotation of the MLS signature key registered for a client.
--
-- The backend pins one signature public key per client and signature scheme,
-- and uses it to authenticate key packages and commits. Replacing that key is
-- therefore only allowed with a proof of possession of both keys:
--
-- * the new key is proven by a key package signed with it, whose x509
--   credential names the client and contains the new key;
--
-- * the old key is proven by a signature, with label 'keyRotationLabel', over
--   an 'MLSKeyRotationTBS' binding the client identity, the old key and the
--   new key.
--
-- The backend does not verify x509 certificate chains (see
-- 'Wire.API.MLS.Validation.validateCredential'), so the credential of the new
-- key is self-asserted: the registered key is the only trust anchor, and
-- whoever holds it can rotate the client to a key of their choice.
module Wire.API.MLS.KeyRotation
  ( MLSKeyRotation (..),
    MLSKeyRotationTBS (..),
    MLSKeyRotationError (..),
    keyRotationLabel,
    validateKeyRotationKeyPackage,
    verifyKeyRotationSignature,
  )
where

import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.Json.Util (base64Schema)
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Validation

-- | Request to replace the signature key registered for a client.
data MLSKeyRotation = MLSKeyRotation
  { -- | A key package signed with the new key, carrying an x509 credential.
    keyPackage :: RawMLS KeyPackage,
    -- | Signature with the currently registered key over an
    -- 'MLSKeyRotationTBS'.
    signature :: ByteString
  }
  deriving stock (Eq, Show)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema MLSKeyRotation)

instance ToSchema MLSKeyRotation where
  schema =
    objectWithDocModifier
      (S.description ?~ "Proof of possession of both the registered and the new MLS signature key of a client")
      $ MLSKeyRotation
        <$> (.keyPackage)
          .= fieldWithDocModifier
            "key_package"
            (S.description ?~ "Key package signed with the new key, with an x509 credential for this client")
            rawKeyPackageSchema
        <*> (.signature)
          .= fieldWithDocModifier
            "signature"
            (S.description ?~ "Signature with the registered key, using the label \"WireMLSKeyRotation\", over the client identity, the registered key and the new key")
            base64Schema

-- | The content signed with the old key:
--
-- > struct {
-- >   opaque client_identity<V>;   // "{user_id}:{client_id}@{domain}"
-- >   opaque old_signature_key<V>;
-- >   opaque new_signature_key<V>;
-- > } MLSKeyRotationTBS;
data MLSKeyRotationTBS = MLSKeyRotationTBS
  { clientIdentity :: ClientIdentity,
    oldKey :: ByteString,
    newKey :: ByteString
  }
  deriving stock (Eq, Show)

instance SerialiseMLS MLSKeyRotationTBS where
  serialiseMLS tbs = do
    serialiseMLSBytes @VarInt (encodeMLS' tbs.clientIdentity)
    serialiseMLSBytes @VarInt tbs.oldKey
    serialiseMLSBytes @VarInt tbs.newKey

-- | Label for the signature with the old key (prefixed with "MLS 1.0 " as for
-- every MLS SignWithLabel signature).
keyRotationLabel :: ByteString
keyRotationLabel = "WireMLSKeyRotation"

data MLSKeyRotationError
  = -- | The key package is not valid for this client.
    KeyRotationInvalidKeyPackage ValidationError
  | -- | Only x509 credentials can be rotated: a basic credential has no
    -- identity other than its key.
    KeyRotationNotX509
  | -- | The new key is the registered key.
    KeyRotationSameKey
  | -- | The signature with the registered key is invalid.
    KeyRotationInvalidSignature
  deriving stock (Eq, Show)

-- | Check the key package of a rotation request, and return its ciphersuite
-- and the new signature key.
validateKeyRotationKeyPackage ::
  ClientIdentity ->
  MLSKeyRotation ->
  Either MLSKeyRotationError (CipherSuiteTag, ByteString)
validateKeyRotationKeyPackage cid rotation = do
  let kp = rotation.keyPackage.value
  (cs, _lifetime) <-
    first KeyRotationInvalidKeyPackage $
      validateKeyPackage (Just (RegularClient cid)) kp
  unless (credentialTag kp.leafNode.credential == X509CredentialTag) $
    Left KeyRotationNotX509
  pure (cs, kp.leafNode.signatureKey)

-- | Check the signature with the registered key over the rotation from the
-- old key to the new key.
verifyKeyRotationSignature ::
  CipherSuiteTag ->
  ClientIdentity ->
  -- | registered (old) key
  ByteString ->
  -- | new key
  ByteString ->
  -- | signature
  ByteString ->
  Either MLSKeyRotationError ()
verifyKeyRotationSignature cs cid old new sig = do
  when (old == new) $ Left KeyRotationSameKey
  let tbs = MLSKeyRotationTBS {clientIdentity = cid, oldKey = old, newKey = new}
  unless (csVerifySignatureWithLabel cs old keyRotationLabel (mkRawMLS tbs) sig) $
    Left KeyRotationInvalidSignature
