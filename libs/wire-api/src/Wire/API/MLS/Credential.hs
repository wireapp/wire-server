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
import Data.Binary.Get
import Data.Binary.Parser
import Data.Binary.Parser.Char8
import Data.Domain
import Data.Id
import Data.Qualified
import qualified Data.Text as T
import Data.UUID
import Imports
import Wire.API.Arbitrary
import Wire.API.MLS.Serialisation

-- | An MLS credential.
--
-- Only the @BasicCredential@ type is supported.
data Credential = BasicCredential
  { bcIdentity :: ByteString,
    bcSignatureScheme :: SignatureScheme,
    bcSignatureKey :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Credential

data CredentialTag = BasicCredentialTag
  deriving stock (Enum, Bounded, Eq, Show)

instance ParseMLS CredentialTag where
  parseMLS = parseMLSEnum @Word16 "credential type"

instance ParseMLS Credential where
  parseMLS =
    parseMLS >>= \case
      BasicCredentialTag ->
        BasicCredential
          <$> parseMLSBytes @Word16
          <*> parseMLS
          <*> parseMLSBytes @Word16

credentialTag :: Credential -> CredentialTag
credentialTag (BasicCredential _ _ _) = BasicCredentialTag

-- | A TLS signature scheme.
--
-- See <https://www.iana.org/assignments/tls-parameters/tls-parameters.xhtml#tls-signaturescheme>.
newtype SignatureScheme = SignatureScheme {signatureSchemeNumber :: Word16}
  deriving stock (Eq, Show)
  deriving newtype (ParseMLS, Arbitrary)

data ClientIdentity = ClientIdentity
  { ciDomain :: Domain,
    ciUser :: UserId,
    ciClient :: ClientId
  }
  deriving stock (Eq, Show, Generic)

instance ParseMLS ClientIdentity where
  parseMLS = do
    uid <-
      maybe (fail "Invalid UUID") (pure . Id)
        =<< fmap fromASCIIBytes (getByteString 36)
    char ':'
    cid <- newClientId <$> hexadecimal
    char '@'
    dom <-
      either fail pure
        =<< fmap (mkDomain . T.pack) (many' anyChar)
    pure $ ClientIdentity dom uid cid

mkClientIdentity :: Qualified UserId -> ClientId -> ClientIdentity
mkClientIdentity (Qualified uid domain) cid = ClientIdentity domain uid cid

instance Binary ClientIdentity
