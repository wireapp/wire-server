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

module Wire.API.MLS.Validation.Error where

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LT
import Data.Text.Lazy.Builder.Int qualified as LT
import Imports
import Wire.API.MLS.CipherSuite (SignatureSchemeTag)

data ValidationError
  = InvalidKeyPackageSignature
  | UnsupportedCipherSuite Word16
  | InvalidLeafNodeSignature
  | FailedToParseIdentity Text
  | SchemeMismatch SignatureSchemeTag
  | LeafNodeSourceTagMisMatch Text
  | UnsupportedProtocolVersion
  | UnexpectedLeafNodeSource
  | IdentityMismatch
  | PublicKeyMismatch
  | BasicCredentialCapabilityMissing
  deriving (Show, Eq, Generic)

toText :: ValidationError -> Text
toText InvalidKeyPackageSignature = "Invalid KeyPackage signature"
toText (UnsupportedCipherSuite cs) = "Unsupported ciphersuite 0x" <> LT.toStrict (LT.toLazyText (LT.hexadecimal cs))
toText InvalidLeafNodeSignature = "Invalid LeafNode signature"
toText (FailedToParseIdentity err) = "Failed to parse identity: " <> err
toText (SchemeMismatch ss) = "Certificate signature scheme " <> T.pack (show ss) <> " does not match client's public key"
toText (LeafNodeSourceTagMisMatch err) = err
toText UnsupportedProtocolVersion = "Unsupported protocol version"
toText UnexpectedLeafNodeSource = "Unexpected leaf node source"
toText IdentityMismatch = "Client identity does not match credential identity"
toText PublicKeyMismatch = "Certificate public key does not match client's"
toText BasicCredentialCapabilityMissing = "Missing BasicCredential capability"
