{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}  -- FUTUREWORK: get rid of this

module Spar.Data.Instances where

import Cassandra as Cas
import Data.String.Conversions
import Data.X509 (SignedCertificate)
import Text.XML.DSig (renderKeyInfo, parseKeyInfo)
import URI.ByteString
import Text.XML.Util (parseURI')

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
    fromCql _           = fail "URI: expected CqlBlob"

deriving instance Cql SAML.Issuer
deriving instance Cql SAML.IdPId
