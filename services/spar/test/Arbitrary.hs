{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE StandaloneDeriving   #-}
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
import Spar.SCIM
import Test.QuickCheck


instance Arbitrary IdPList where
  arbitrary = do
    _idplProviders <- arbitrary
    pure $ IdPList {..}

deriving instance Arbitrary ScimToken

instance Arbitrary NoContent where
  arbitrary = pure NoContent

instance Arbitrary CreateScimToken where
  arbitrary = CreateScimToken <$> arbitrary

-- This is not required by the servant-server instances, but the swagger
-- tests want it. See https://github.com/haskell-servant/servant-swagger/issues/58

instance ToJSON NoContent where
  toJSON NoContent = String "(no content)"

instance ToSchema NoContent where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)
