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
  )
where

import Control.Applicative
import Imports
import Wire.API.MLS.Capabilities
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Lifetime
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation

validateKeyPackage ::
  Maybe ClientIdentity ->
  KeyPackage ->
  Either Text (CipherSuiteTag, Lifetime)
validateKeyPackage mIdentity kp = do
  -- get ciphersuite
  cs <-
    maybe
      (Left "Unsupported ciphersuite")
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
    $ Left "Invalid KeyPackage signature"

  -- validate protocol version
  maybe
    (Left "Unsupported protocol version")
    pure
    (pvTag (kp.protocolVersion) >>= guard . (== ProtocolMLS10))

  -- validate credential, lifetime and capabilities
  validateLeafNode cs mIdentity LeafNodeTBSExtraKeyPackage kp.leafNode

  lt <- case kp.leafNode.source of
    LeafNodeSourceKeyPackage lt -> pure lt
    -- unreachable
    _ -> Left "Unexpected leaf node source"

  pure (cs, lt)

validateLeafNode ::
  CipherSuiteTag ->
  Maybe ClientIdentity ->
  LeafNodeTBSExtra ->
  LeafNode ->
  Either Text ()
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
    $ Left "Invalid LeafNode signature"

  validateCredential mIdentity leafNode.credential
  validateSource extra.tag leafNode.source
  validateCapabilities leafNode.capabilities

validateCredential :: Maybe ClientIdentity -> Credential -> Either Text ()
validateCredential mIdentity (BasicCredential cred) = do
  identity <-
    either credentialError pure $
      decodeMLS' cred
  unless (maybe True (identity ==) mIdentity) $
    Left "client identity does not match credential identity"
  where
    credentialError e =
      Left $
        "Failed to parse identity: " <> e

validateSource :: LeafNodeSourceTag -> LeafNodeSource -> Either Text ()
validateSource t s = do
  let t' = leafNodeSourceTag s
  if t == t'
    then pure ()
    else
      Left $
        "Expected '"
          <> t.name
          <> "' source, got '"
          <> (leafNodeSourceTag s).name
          <> "'"

validateCapabilities :: Capabilities -> Either Text ()
validateCapabilities _ = pure () -- TODO
