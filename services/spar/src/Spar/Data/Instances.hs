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

import qualified Data.Binary.Builder as BB
import qualified SAML2.WebSSO as SAML

instance Cql (SignedCertificate) where
    ctype = Tagged TextColumn
    toCql = CqlText . cs . renderKeyInfo

    fromCql (CqlText t) = parseKeyInfo (cs t)
    fromCql _           = fail "URI: expected CqlBlob"

instance Cql (URIRef Absolute) where
    ctype = Tagged TextColumn
    toCql = CqlText . cs. BB.toLazyByteString . serializeURIRef

    fromCql (CqlText t) = parseURI' $ t
    fromCql _           = fail "URI: expected CqlBlob"

deriving instance Cql SAML.Issuer
deriving instance Cql SAML.IdPId
