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

import Data.Proxy
import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.
import Lens.Micro
import Servant
import Servant.Swagger
import Spar.API.Instances ()
import Spar.Types
import Web.Cookie (SetCookie)

import qualified Data.Aeson as Swagger
import qualified Data.Scientific as Swagger
import qualified Data.Id as Brig
import qualified Data.X509 as X509
import qualified SAML2.WebSSO as SAML
import qualified Servant.Multipart as SM
import qualified URI.ByteString as URI


-- FUTUREWORK: push orphans upstream to saml2-web-sso, servant-multipart

-- TODO: steal from https://github.com/haskell-servant/servant-swagger/blob/master/example/src/Todo.hs

instance ToSchema Swagger where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())
    & mapped . schema . description ?~ "The swagger docs you are looking at (all details hidden)."

instance HasSwagger route => HasSwagger (SM.MultipartForm SM.Mem resp :> route) where
  toSwagger _proxy = toSwagger (Proxy @route)

instance ToParamSchema Brig.TeamId
instance ToParamSchema Brig.UserId
instance ToParamSchema SAML.IdPId
instance ToSchema Brig.TeamId
instance ToSchema Brig.UserId
instance ToSchema NewIdP
instance ToSchema SAML.AuthnRequest
instance ToSchema (SAML.FormRedirect SAML.AuthnRequest)
instance ToSchema (SAML.IdPConfig Brig.TeamId)  -- TODO: would be nice to add an example here, but that only works for json?
instance ToSchema SAML.IdPId
instance ToSchema (SAML.ID SAML.AuthnRequest)
instance ToSchema SAML.Issuer
instance ToSchema SAML.Time
instance ToSchema SAML.Version

instance ToSchema X509.SignedCertificate where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())

instance ToSchema SAML.SPDesc where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())

instance ToSchema URI.URI where
  declareNamedSchema _proxy = genericDeclareNamedSchema defaultSchemaOptions (Proxy @())

instance ToParamSchema SetCookie where
  toParamSchema _proxy = mkEmptyParamSchema SwaggerBoolean

mkEmptyParamSchema :: SwaggerType t -> ParamSchema t
mkEmptyParamSchema _paramSchemaType = ParamSchema {..}
  where
    _paramSchemaDefault = Nothing :: Maybe Swagger.Value
    _paramSchemaFormat = Nothing :: Maybe Format
    _paramSchemaItems = Nothing :: Maybe (SwaggerItems t)
    _paramSchemaMaximum = Nothing :: Maybe Swagger.Scientific
    _paramSchemaExclusiveMaximum = Nothing :: Maybe Bool
    _paramSchemaMinimum = Nothing :: Maybe Swagger.Scientific
    _paramSchemaExclusiveMinimum = Nothing :: Maybe Bool
    _paramSchemaMaxLength = Nothing :: Maybe Integer
    _paramSchemaMinLength = Nothing :: Maybe Integer
    _paramSchemaPattern = Nothing :: Maybe Pattern
    _paramSchemaMaxItems = Nothing :: Maybe Integer
    _paramSchemaMinItems = Nothing :: Maybe Integer
    _paramSchemaUniqueItems = Nothing :: Maybe Bool
    _paramSchemaEnum = Nothing :: Maybe [Swagger.Value]
    _paramSchemaMultipleOf = Nothing :: Maybe Swagger.Scientific
