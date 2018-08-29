{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary where

import Data.Proxy
import "swagger2" Data.Swagger hiding (Header(..))
import Data.Aeson
import Data.Id ()
import SAML2.WebSSO as SAML
import SAML2.WebSSO.Test.Arbitrary ()
import Servant.API.ContentTypes
import Spar.Types
import Test.QuickCheck


instance Arbitrary SAML.NewIdP where
  arbitrary = do
    _nidpMetadata   <- arbitrary
    _nidpIssuer     <- arbitrary
    _nidpRequestUri <- arbitrary
    _nidpPublicKey  <- arbitrary
    pure $ SAML.NewIdP {..}

instance Arbitrary SPInfo where
  arbitrary = do
    _spiMetaURI  <- arbitrary
    _spiLoginURI <- arbitrary
    pure $ SPInfo {..}

instance Arbitrary IdPExtra where
  arbitrary = do
    _idpeTeam   <- arbitrary
    _idpeSPInfo <- arbitrary
    pure $ IdPExtra {..}

instance Arbitrary IdPList where
  arbitrary = do
    _idplProviders <- arbitrary
    pure $ IdPList {..}

instance Arbitrary NoContent where
  arbitrary = pure NoContent


-- This is not required by the servant-server instances, but the swagger
-- tests want it. See https://github.com/haskell-servant/servant-swagger/issues/58

instance ToJSON NoContent where
  toJSON NoContent = String "(no content)"

instance ToSchema NoContent where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)
