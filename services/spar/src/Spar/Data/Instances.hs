{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}  -- FUTUREWORK: get rid of this

module Spar.Data.Instances where

import Cassandra as Cas
import Data.Int (Int8)
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

type VerdictFormatRow = (Int8, Maybe URI, Maybe URI)

fromVerdictFormat :: VerdictFormat -> VerdictFormatRow
fromVerdictFormat VerdictFormatWeb                         = (0, Nothing, Nothing)
fromVerdictFormat (VerdictFormatMobile succredir errredir) = (1, Just succredir, Just errredir)

toVerdictFormat :: VerdictFormatRow -> Maybe VerdictFormat
toVerdictFormat (0, Nothing, Nothing)              = Just VerdictFormatWeb
toVerdictFormat (1, Just succredir, Just errredir) = Just $ VerdictFormatMobile succredir errredir
toVerdictFormat _                                  = Nothing


-- | TODO: why is this not available from @class Cql@?
-- <http://hackage.haskell.org/package/cql-4.0.1/docs/Database-CQL-Protocol.html>
instance Cql Int8 where
    ctype              = Tagged IntColumn
    toCql              = CqlInt . fromIntegral
    fromCql (CqlInt i) = pure $ fromIntegral i
    fromCql _          = fail "Int8: expected CqlInt"
