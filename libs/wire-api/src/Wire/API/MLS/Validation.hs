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

module Wire.API.MLS.Validation
  ( -- * Main key package validation function
    validateKeyPackage,
    validateLeafNode,
    ValidationError (..),
  )
where

import Control.Applicative
import Control.Error.Util
import Data.ByteArray qualified as BA
import Data.X509 qualified as X509
import Imports
import Wire.API.MLS.Capabilities
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Lifetime
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Validation.Error

validateKeyPackage ::
  Maybe ClientIdentity ->
  KeyPackage ->
  Either ValidationError (CipherSuiteTag, Lifetime)
validateKeyPackage mIdentity kp = do
  -- get ciphersuite
  cs <-
    maybe
      (Left (UnsupportedCipherSuite kp.cipherSuite.cipherSuiteNumber))
      pure
      $ cipherSuiteTag kp.cipherSuite

  -- validate signature
  unless
    ( csVerifySignatureWithLabel
        cs
        kp.leafNode.signatureKey
        "KeyPackageTBS"
        kp.tbs
        kp.signature_
    )
    $ Left InvalidKeyPackageSignature

  -- validate protocol version
  maybe
    (Left UnsupportedProtocolVersion)
    pure
    (pvTag (kp.protocolVersion) >>= guard . (== ProtocolMLS10))

  -- validate credential, lifetime and capabilities
  validateLeafNode cs mIdentity LeafNodeTBSExtraKeyPackage kp.leafNode

  lt <- case kp.leafNode.source of
    LeafNodeSourceKeyPackage lt -> pure lt
    -- unreachable
    _ -> Left UnexpectedLeafNodeSource

  pure (cs, lt)

validateLeafNode ::
  CipherSuiteTag ->
  Maybe ClientIdentity ->
  LeafNodeTBSExtra ->
  LeafNode ->
  Either ValidationError ()
validateLeafNode cs mIdentity extra leafNode = do
  let tbs = LeafNodeTBS leafNode.core extra
  unless
    ( csVerifySignatureWithLabel
        cs
        leafNode.signatureKey
        "LeafNodeTBS"
        (mkRawMLS tbs)
        leafNode.signature_
    )
    $ Left InvalidLeafNodeSignature

  validateCredential cs leafNode.signatureKey mIdentity leafNode.credential
  validateSource extra.tag leafNode.source
  validateCapabilities (credentialTag leafNode.credential) leafNode.capabilities

validateCredential :: CipherSuiteTag -> ByteString -> Maybe ClientIdentity -> Credential -> Either ValidationError ()
validateCredential cs pkey mIdentity cred = do
  -- FUTUREWORK: check signature in the case of an x509 credential
  (identity, mkey) <-
    either credentialError pure $
      credentialIdentityAndKey cred
  traverse_ (validateCredentialKey (csSignatureScheme cs) pkey) mkey
  unless (maybe True (identity ==) mIdentity) $
    Left IdentityMismatch
  where
    credentialError e = Left $ FailedToParseIdentity e

validateCredentialKey :: SignatureSchemeTag -> ByteString -> X509.PubKey -> Either ValidationError ()
validateCredentialKey Ed25519 pk1 (X509.PubKeyEd25519 pk2) = validateCredentialKeyBS pk1 (BA.convert pk2)
validateCredentialKey Ecdsa_secp256r1_sha256 pk1 (X509.PubKeyEC pk2) =
  case pk2.pubkeyEC_pub of
    X509.SerializedPoint bs -> validateCredentialKeyBS pk1 bs
validateCredentialKey Ecdsa_secp384r1_sha384 pk1 (X509.PubKeyEC pk2) =
  case pk2.pubkeyEC_pub of
    X509.SerializedPoint bs -> validateCredentialKeyBS pk1 bs
validateCredentialKey Ecdsa_secp521r1_sha512 pk1 (X509.PubKeyEC pk2) =
  case pk2.pubkeyEC_pub of
    X509.SerializedPoint bs -> validateCredentialKeyBS pk1 bs
validateCredentialKey ss _ _ =
  Left $ SchemeMismatch ss

validateCredentialKeyBS :: ByteString -> ByteString -> Either ValidationError ()
validateCredentialKeyBS pk1 pk2 =
  note PublicKeyMismatch $
    guard (pk1 == pk2)

validateSource :: LeafNodeSourceTag -> LeafNodeSource -> Either ValidationError ()
validateSource t s = do
  let t' = leafNodeSourceTag s
  if t == t'
    then pure ()
    else
      Left $
        LeafNodeSourceTagMisMatch $
          "Expected '"
            <> t.name
            <> "' source, got '"
            <> t'.name
            <> "'"

validateCapabilities :: CredentialTag -> Capabilities -> Either ValidationError ()
validateCapabilities ctag caps =
  unless (fromMLSEnum ctag `elem` caps.credentials) $
    Left BasicCredentialCapabilityMissing
