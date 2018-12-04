{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.SCIM.Swagger where

import Imports
import Control.Lens
import Data.Id
import Data.Proxy
import Data.Time
import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.
import Spar.Orphans ()
import Spar.Types
import Spar.SCIM

import qualified SAML2.WebSSO as SAML


instance ToParamSchema ScimToken where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema ScimToken where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)
    & mapped . schema . description ?~ "Authentication token"

instance ToSchema ScimTokenInfo where
  declareNamedSchema _ = do
    teamSchema      <- declareSchemaRef (Proxy @TeamId)
    idSchema        <- declareSchemaRef (Proxy @ScimTokenId)
    createdAtSchema <- declareSchemaRef (Proxy @UTCTime)
    idpSchema       <- declareSchemaRef (Proxy @SAML.IdPId)
    descrSchema     <- declareSchemaRef (Proxy @Text)
    return $ NamedSchema (Just "ScimTokenInfo") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          [ ("team", teamSchema)
          , ("id", idSchema)
          , ("created_at", createdAtSchema)
          , ("idp", idpSchema)
          , ("description", descrSchema)
          ]
      & required .~ [ "team", "id", "created_at", "description" ]

instance ToSchema CreateScimToken where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    return $ NamedSchema (Just "CreateScimToken") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          [ ("description", textSchema)
          ]
      & required .~ [ "description" ]

instance ToSchema CreateScimTokenResponse where
  declareNamedSchema _ = do
    tokenSchema <- declareSchemaRef (Proxy @ScimToken)
    infoSchema  <- declareSchemaRef (Proxy @ScimTokenInfo)
    return $ NamedSchema (Just "CreateScimTokenResponse") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          [ ("token", tokenSchema)
          , ("info", infoSchema)
          ]
      & required .~ [ "token", "info" ]

instance ToSchema ScimTokenList where
  declareNamedSchema _ = do
    infoListSchema <- declareSchemaRef (Proxy @[ScimTokenInfo])
    return $ NamedSchema (Just "ScimTokenList") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          [ ("tokens", infoListSchema)
          ]
      & required .~ [ "tokens" ]
