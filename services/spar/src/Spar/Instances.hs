{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: Should some of these instances be moved elsewhere to avoid Orphans?
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spar.Instances where

import Control.Arrow (left)
import Cassandra as Cas
import Data.X509 (SignedCertificate)
import Text.XML.DSig (renderKeyInfo, parseKeyInfo)
import URI.ByteString
import Data.String.Conversions

import qualified Data.ByteString.Lazy as BL
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

    fromCql (CqlText t) = parseURI' . cs $ t
    fromCql _           = fail "URI: expected CqlBlob"

deriving instance Cql SAML.Issuer

parseURI' :: BL.ByteString -> Either String (URIRef Absolute)
parseURI' t = left (\e -> fail "URI: " <> show e) $ parseURI strictURIParserOptions (BL.toStrict t)
