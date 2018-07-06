{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary where

import Data.Misc
import Data.Proxy
import "swagger2" Data.Swagger hiding (Header(..))
import Data.Aeson
import Data.Id ()
import SAML2.WebSSO.Test.Arbitrary ()
import Spar.Types
import Test.QuickCheck
import Servant.API.ContentTypes


instance Arbitrary NewIdP where
  arbitrary = do
    _nidpMetadata   <- arbitrary
    _nidpIssuer     <- arbitrary
    _nidpRequestUri <- arbitrary
    _nidpPublicKey  <- arbitrary
    pure $ NewIdP {..}

instance Arbitrary NoContent where
  arbitrary = pure NoContent

deriving instance Arbitrary HttpsUrl


-- This is not required by the servant-server instances, but the swagger
-- tests want it. See https://github.com/haskell-servant/servant-swagger/issues/58

instance ToJSON NoContent where
  toJSON NoContent = String "(no content)"

instance ToSchema NoContent where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)
