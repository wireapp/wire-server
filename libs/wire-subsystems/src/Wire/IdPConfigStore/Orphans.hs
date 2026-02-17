{-# OPTIONS_GHC -fno-warn-orphans #-}

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
