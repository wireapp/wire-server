{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.API.Swagger where

import Data.Id
import Data.Proxy
import Data.UUID (UUID)
import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.
import Lens.Micro
import Servant
import Servant.Swagger
import Spar.API.Instances ()
import Spar.Types
import Web.Cookie (SetCookie)

import qualified Data.Swagger.SchemaOptions as Swagger
import qualified Data.X509 as X509
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Config.TH as SAML
import qualified Servant.Multipart as SM
import qualified URI.ByteString as URI


-- FUTUREWORK: push orphans upstream to saml2-web-sso, servant-multipart

-- TODO: steal from https://github.com/haskell-servant/servant-swagger/blob/master/example/src/Todo.hs

instance ToSchema Swagger where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())
    & mapped . schema . description ?~ "The swagger docs you are looking at (all details hidden)."

instance HasSwagger route => HasSwagger (SM.MultipartForm SM.Mem resp :> route) where
  toSwagger _proxy = toSwagger (Proxy @route)

-- | The options to use for schema generation. Must match the options used
-- for 'ToJSON' instances elsewhere.
samlSchemaOptions :: SchemaOptions
samlSchemaOptions = Swagger.fromAesonOptions SAML.deriveJSONOptions

instance ToParamSchema TeamId where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToParamSchema UserId where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToParamSchema SAML.IdPId where
  toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToSchema TeamId where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)
instance ToSchema UserId where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)
instance ToSchema SAML.IdPId where
  declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance ToSchema SAML.AuthnRequest where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema (SAML.FormRedirect SAML.AuthnRequest) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions
  -- TODO: would be nice to add an example here, but that only works for json?

instance ToSchema SPInfo where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema IdPExtra where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema a => ToSchema (SAML.IdPConfig a) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema IdPList where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema NewIdP where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema (SAML.ID SAML.AuthnRequest) where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.Issuer where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema SAML.Time where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema SAML.Version where
  declareNamedSchema = genericDeclareNamedSchema samlSchemaOptions

instance ToSchema X509.SignedCertificate where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema SAML.SPDesc where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToSchema URI.URI where
  declareNamedSchema _ = declareNamedSchema (Proxy @String)

instance ToParamSchema URI.URI where
  toParamSchema _ = toParamSchema (Proxy @String)

instance ToParamSchema SetCookie where
  toParamSchema _ = toParamSchema (Proxy @Bool)
