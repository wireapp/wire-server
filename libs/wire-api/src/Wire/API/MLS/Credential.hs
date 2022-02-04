{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.MLS.Credential where

import Data.Binary
import Imports

-- | An MLS credential.
--
-- Only the @BasicCredential@ type is supported.
data Credential = BasicCredential
  { bcIdentity :: ByteString,
    bcSignatureScheme :: SignatureScheme,
    bcSignatureKey :: ByteString
  }
  deriving (Generic)

instance Binary Credential

data CredentialType = BasicCredentialType

credentialType :: Credential -> CredentialType
credentialType (BasicCredential _ _ _) = BasicCredentialType

-- | A TLS signature scheme.
--
-- See <https://www.iana.org/assignments/tls-parameters/tls-parameters.xhtml#tls-signaturescheme>.
newtype SignatureScheme = SignatureScheme {signatureSchemeNumber :: Word16}
  deriving newtype (Binary)
