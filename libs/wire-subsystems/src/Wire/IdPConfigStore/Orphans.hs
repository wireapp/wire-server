{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Wire.IdPConfigStore.Orphans where

import Cassandra as Cas
import Data.Text.Encoding.Error
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding as LT
import Data.X509 (SignedCertificate)
import Imports
import SAML2.Util (parseURI')
import SAML2.WebSSO qualified as SAML
import Text.XML.DSig (parseKeyInfo, renderKeyInfo)
import URI.ByteString

instance Cql SignedCertificate where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . LT.encodeUtf8 . renderKeyInfo

  fromCql (CqlBlob t) = parseKeyInfo False (LT.decodeUtf8With lenientDecode t)
  fromCql _ = Left "SignedCertificate: expected CqlBlob"

instance Cql (URIRef Absolute) where
  ctype = Tagged TextColumn
  toCql = CqlText . SAML.renderURI

  fromCql (CqlText t) = parseURI' t
  fromCql _ = Left "URI: expected CqlText"

instance Cql SAML.NameID where
  ctype = Tagged TextColumn
  toCql = CqlText . LT.toStrict . SAML.encodeElem

  fromCql (CqlText t) = SAML.decodeElem (LT.fromStrict t)
  fromCql _ = Left "NameID: expected CqlText"

deriving instance Cql SAML.Issuer

deriving instance Cql (SAML.ID SAML.AuthnRequest)
