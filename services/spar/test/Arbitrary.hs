{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module Arbitrary where

import Data.Aeson
import Data.Id (TeamId, UserId)
import Data.OpenApi hiding (Header (..))
import Data.Proxy
import Data.String.Conversions
import Imports
import SAML2.WebSSO.Config
import SAML2.WebSSO.Test.Arbitrary ()
import SAML2.WebSSO.Types
import Servant.API.ContentTypes
import Spar.Scim
import Spar.Scim.Types (ScimUserCreationStatus)
import qualified Spar.Sem.IdPConfigStore as E
import Test.QuickCheck
import URI.ByteString
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml

instance Arbitrary IdPList where
  arbitrary = IdPList <$> arbitrary

instance Arbitrary ScimTokenHash where
  arbitrary = hashScimToken <$> arbitrary

instance Arbitrary ScimTokenList where
  arbitrary = ScimTokenList <$> arbitrary

instance Arbitrary ScimTokenListV7 where
  arbitrary = ScimTokenListV7 <$> arbitrary

instance Arbitrary ScimTokenName where
  arbitrary = ScimTokenName <$> arbitrary

instance Arbitrary NoContent where
  arbitrary = pure NoContent

instance Arbitrary IdPMetadataInfo where
  arbitrary = do
    mdata <- arbitrary
    pure $ IdPMetadataValue (cs $ encode mdata) mdata

instance Arbitrary SsoSettings where
  arbitrary = SsoSettings <$> arbitrary

instance Arbitrary MultiIngressDomainConfig where
  arbitrary = do
    _cfgSPAppURI <- arbitrary
    _cfgSPSsoURI <- arbitrary
    _cfgContacts <- arbitrary
    pure $ MultiIngressDomainConfig {..}

instance Arbitrary ContactPerson where
  arbitrary =
    ContactPerson
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ContactType where
  arbitrary =
    elements
      [minBound ..]

-- This is not required by the servant-server instances, but the swagger
-- tests want it. See https://github.com/haskell-servant/servant-swagger/issues/58

instance ToJSON NoContent where
  toJSON NoContent = String "(no content)"

instance ToSchema NoContent where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance Arbitrary E.Replacing where
  arbitrary = E.Replacing <$> arbitrary

instance Arbitrary E.Replaced where
  arbitrary = E.Replaced <$> arbitrary

-- TODO(sandy): IdPIds are unlikely to collide. Does the size parameter
-- affect them?
instance CoArbitrary IdPId

instance CoArbitrary IdPHandle

instance CoArbitrary WireIdP

instance CoArbitrary WireIdPAPIVersion

instance CoArbitrary TeamId

instance CoArbitrary UserId

instance CoArbitrary Time

instance CoArbitrary Issuer where
  coarbitrary (Issuer ur) = coarbitrary $ show ur

instance (CoArbitrary a) => CoArbitrary (URIRef a) where
  coarbitrary = coarbitrary . show

instance CoArbitrary (IdPConfig WireIdP)

instance CoArbitrary IdPMetadata where
  coarbitrary = coarbitrary . show

instance CoArbitrary ScimUserCreationStatus
