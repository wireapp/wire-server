{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Id ()
import Data.Proxy
import Data.String.Conversions (cs)
import Data.Swagger hiding (Header (..))
import Imports
import SAML2.WebSSO.Test.Arbitrary ()
import Servant.API.ContentTypes
import Spar.Scim
import Spar.Types
import Test.QuickCheck

instance Arbitrary IdPList where
  arbitrary = do
    _idplProviders <- arbitrary
    pure $ IdPList {..}

deriving instance Arbitrary ScimToken

instance Arbitrary ScimTokenInfo where
  arbitrary =
    ScimTokenInfo
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CreateScimToken where
  arbitrary = CreateScimToken <$> arbitrary <*> arbitrary

instance Arbitrary CreateScimTokenResponse where
  arbitrary = CreateScimTokenResponse <$> arbitrary <*> arbitrary

instance Arbitrary ScimTokenList where
  arbitrary = ScimTokenList <$> arbitrary

instance Arbitrary NoContent where
  arbitrary = pure NoContent

instance Arbitrary IdPMetadataInfo where
  arbitrary = do
    mdata <- arbitrary
    pure $ IdPMetadataValue (cs $ encode mdata) mdata

instance Arbitrary SsoSettings where
  arbitrary = SsoSettings <$> arbitrary

-- This is not required by the servant-server instances, but the swagger
-- tests want it. See https://github.com/haskell-servant/servant-swagger/issues/58

instance ToJSON NoContent where
  toJSON NoContent = String "(no content)"

instance ToSchema NoContent where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)
