{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- | 'Cql' instances for Spar types, as well as conversion functions used in "Spar.Data"
-- (which does the actual database work).
module Spar.Data.Instances
  ( -- * Raw database types
    VerdictFormatRow,
    VerdictFormatCon (..),

    -- ** Conversions
    fromVerdictFormat,
    toVerdictFormat,
  )
where

import Cassandra as Cas
import Data.ByteString.Conversion (fromByteString, toByteString)
import Data.X509 (SignedCertificate)
import Imports
import SAML2.Util (parseURI')
import qualified SAML2.WebSSO as SAML
import Spar.Scim.Types (ScimUserCreationStatus (..))
import Text.XML.DSig (parseKeyInfo, renderKeyInfo)
import URI.ByteString
import Wire.API.User.Saml
import Wire.API.User.Scim

instance Cql SAML.XmlText where
  ctype = Tagged TextColumn
  toCql = CqlText . SAML.unsafeFromXmlText

  fromCql (CqlText t) = pure $ SAML.mkXmlText t
  fromCql _ = Left "XmlText: expected CqlText"

instance Cql SignedCertificate where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . cs . renderKeyInfo

  fromCql (CqlBlob t) = parseKeyInfo False (cs t)
  fromCql _ = Left "SignedCertificate: expected CqlBlob"

instance Cql (URIRef Absolute) where
  ctype = Tagged TextColumn
  toCql = CqlText . SAML.renderURI

  fromCql (CqlText t) = parseURI' t
  fromCql _ = Left "URI: expected CqlText"

instance Cql SAML.NameID where
  ctype = Tagged TextColumn
  toCql = CqlText . cs . SAML.encodeElem

  fromCql (CqlText t) = SAML.decodeElem (cs t)
  fromCql _ = Left "NameID: expected CqlText"

deriving instance Cql SAML.Issuer

deriving instance Cql SAML.IdPId

deriving instance Cql (SAML.ID SAML.AuthnRequest)

type VerdictFormatRow = (VerdictFormatCon, Maybe URI, Maybe URI)

data VerdictFormatCon = VerdictFormatConWeb | VerdictFormatConMobile

instance Cql VerdictFormatCon where
  ctype = Tagged IntColumn

  toCql VerdictFormatConWeb = CqlInt 0
  toCql VerdictFormatConMobile = CqlInt 1

  fromCql (CqlInt i) = case i of
    0 -> pure VerdictFormatConWeb
    1 -> pure VerdictFormatConMobile
    n -> Left $ "unexpected VerdictFormatCon: " ++ show n
  fromCql _ = Left "member-status: int expected"

fromVerdictFormat :: VerdictFormat -> VerdictFormatRow
fromVerdictFormat VerdictFormatWeb = (VerdictFormatConWeb, Nothing, Nothing)
fromVerdictFormat (VerdictFormatMobile succredir errredir) = (VerdictFormatConMobile, Just succredir, Just errredir)

toVerdictFormat :: VerdictFormatRow -> Maybe VerdictFormat
toVerdictFormat (VerdictFormatConWeb, Nothing, Nothing) = Just VerdictFormatWeb
toVerdictFormat (VerdictFormatConMobile, Just succredir, Just errredir) = Just $ VerdictFormatMobile succredir errredir
toVerdictFormat _ = Nothing

deriving instance Cql ScimToken

instance Cql ScimTokenHash where
  ctype = Tagged TextColumn
  toCql = CqlText . cs . toByteString
  fromCql (CqlText t) = maybe (Left "ScimTokenHash: parse error") Right (fromByteString . cs $ t)
  fromCql _ = Left "ScimTokenHash: expected CqlText"

instance Cql ScimTokenLookupKey where
  ctype = Tagged TextColumn
  toCql = \case
    ScimTokenLookupKeyHashed h -> toCql h
    ScimTokenLookupKeyPlaintext t -> toCql t
  fromCql s@(CqlText _) =
    ScimTokenLookupKeyHashed <$> fromCql s <|> ScimTokenLookupKeyPlaintext <$> fromCql s
  fromCql _ = Left "ScimTokenLookupKey: expected CqlText"

instance Cql ScimUserCreationStatus where
  ctype = Tagged IntColumn

  toCql ScimUserCreated = CqlInt 0
  toCql ScimUserCreating = CqlInt 1

  fromCql (CqlInt i) = case i of
    0 -> pure ScimUserCreated
    1 -> pure ScimUserCreating
    n -> Left $ "unexpected ScimUserCreationStatus: " ++ show n
  fromCql _ = Left "int expected"
