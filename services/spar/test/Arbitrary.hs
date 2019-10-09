{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary where

import Imports
import Data.Proxy
import "swagger2" Data.Swagger hiding (Header(..))
import Data.Aeson
import Data.Id ()
import SAML2.WebSSO.Test.Arbitrary ()
import Servant.API.ContentTypes
import Spar.Types
import Spar.Scim
import Test.QuickCheck


instance Arbitrary IdPList where
  arbitrary = do
    _idplProviders <- arbitrary
    pure $ IdPList {..}

deriving instance Arbitrary ScimToken

instance Arbitrary ScimTokenInfo where
  arbitrary = ScimTokenInfo
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
  arbitrary = IdPMetadataValue mempty <$> arbitrary

-- This is not required by the servant-server instances, but the swagger
-- tests want it. See https://github.com/haskell-servant/servant-swagger/issues/58

instance ToJSON NoContent where
  toJSON NoContent = String "(no content)"

instance ToSchema NoContent where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)
