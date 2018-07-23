{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}  -- FUTUREWORK: get rid of this

module Spar.Data.Instances where

import Cassandra as Cas
import Data.Aeson (encode, eitherDecode)
import Data.String.Conversions
import Data.X509 (SignedCertificate)
import Text.XML.DSig (renderKeyInfo, parseKeyInfo)
import URI.ByteString
import Text.XML.Util (parseURI')
import Spar.Types

import qualified SAML2.WebSSO as SAML
import qualified Text.XML.Util as SAML

instance Cql (SignedCertificate) where
    ctype = Tagged BlobColumn
    toCql = CqlText . cs . renderKeyInfo

    fromCql (CqlBlob t) = parseKeyInfo (cs t)
    fromCql _           = fail "SignedCertificate: expected CqlBlob"

instance Cql (URIRef Absolute) where
    ctype = Tagged TextColumn
    toCql = CqlText . SAML.renderURI

    fromCql (CqlText t) = parseURI' $ t
    fromCql _           = fail "URI: expected CqlText"

instance Cql SAML.NameID where
    ctype = Tagged TextColumn
    toCql = CqlText . cs . SAML.encodeElem

    fromCql (CqlText t) = SAML.decodeElem . cs $ t
    fromCql _           = fail "NameID: expected CqlText"

deriving instance Cql SAML.Issuer
deriving instance Cql SAML.IdPId
deriving instance Cql (SAML.ID SAML.AuthnRequest)

-- TODO: is encoding VerdictFormat as json in the database a bad idea?
instance Cql (VerdictFormat) where
    ctype = Tagged BlobColumn
    toCql = CqlBlob . encode

    fromCql (CqlBlob t) = eitherDecode t
    fromCql _           = fail "VerdictFormat: expected CqlBlob"
