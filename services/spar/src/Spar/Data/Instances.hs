{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spar.Data.Instances where

import Imports
import Cassandra as Cas
import Data.String.Conversions
import Data.X509 (SignedCertificate)
import SAML2.Util (parseURI')
import Spar.Types
import Text.XML.DSig (renderKeyInfo, parseKeyInfo)
import URI.ByteString

import qualified SAML2.WebSSO as SAML


instance Cql (SignedCertificate) where
    ctype = Tagged BlobColumn
    toCql = CqlBlob . cs . renderKeyInfo

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

type VerdictFormatRow = (VerdictFormatCon, Maybe URI, Maybe URI)
data VerdictFormatCon = VerdictFormatConWeb | VerdictFormatConMobile

instance Cql VerdictFormatCon where
    ctype = Tagged IntColumn

    toCql VerdictFormatConWeb    = CqlInt 0
    toCql VerdictFormatConMobile = CqlInt 1

    fromCql (CqlInt i) = case i of
        0 -> return VerdictFormatConWeb
        1 -> return VerdictFormatConMobile
        n -> fail $ "unexpected VerdictFormatCon: " ++ show n
    fromCql _ = fail "member-status: int expected"

fromVerdictFormat :: VerdictFormat -> VerdictFormatRow
fromVerdictFormat VerdictFormatWeb                         = (VerdictFormatConWeb, Nothing, Nothing)
fromVerdictFormat (VerdictFormatMobile succredir errredir) = (VerdictFormatConMobile, Just succredir, Just errredir)

toVerdictFormat :: VerdictFormatRow -> Maybe VerdictFormat
toVerdictFormat (VerdictFormatConWeb, Nothing, Nothing)                 = Just VerdictFormatWeb
toVerdictFormat (VerdictFormatConMobile, Just succredir, Just errredir) = Just $ VerdictFormatMobile succredir errredir
toVerdictFormat _                                                       = Nothing

deriving instance Cql ScimToken
