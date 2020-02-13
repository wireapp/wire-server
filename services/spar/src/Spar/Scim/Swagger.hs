{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger instances for SCIM-related types that are defined in Spar.
--
-- /Note 2019-02-06:/ SCIM schema types from @hscim@ ('User', 'Group', etc) don't have Swagger
-- instances yet and are unlikely to get them soon. For more details see
-- <https://github.com/wireapp/hscim/pull/16>.
module Spar.Scim.Swagger
  (
  )
where

import Control.Lens
import Data.Id
import Data.Proxy
import "swagger2" Data.Swagger hiding (Header (..))
import Data.Time
import Imports
-- NB: this package depends on both types-common, swagger2, so there is no away around this name
-- clash other than -XPackageImports.

import qualified SAML2.WebSSO as SAML
import Spar.Orphans ()
import Spar.Scim
import Spar.Types

instance ToParamSchema ScimToken where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema ScimToken where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Text)
      & mapped . schema . description ?~ "Authentication token"

instance ToSchema ScimTokenInfo where
  declareNamedSchema _ = do
    teamSchema <- declareSchemaRef (Proxy @TeamId)
    idSchema <- declareSchemaRef (Proxy @ScimTokenId)
    createdAtSchema <- declareSchemaRef (Proxy @UTCTime)
    idpSchema <- declareSchemaRef (Proxy @SAML.IdPId)
    descrSchema <- declareSchemaRef (Proxy @Text)
    return $ NamedSchema (Just "ScimTokenInfo") $
      mempty
        & type_ .~ Just SwaggerObject
        & properties
          .~ [ ("team", teamSchema),
               ("id", idSchema),
               ("created_at", createdAtSchema),
               ("idp", idpSchema),
               ("description", descrSchema)
             ]
        & required .~ ["team", "id", "created_at", "description"]

instance ToSchema CreateScimToken where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    return $ NamedSchema (Just "CreateScimToken") $
      mempty
        & type_ .~ Just SwaggerObject
        & properties
          .~ [ ("description", textSchema),
               ("password", textSchema)
             ]
        & required .~ ["description"]

instance ToSchema CreateScimTokenResponse where
  declareNamedSchema _ = do
    tokenSchema <- declareSchemaRef (Proxy @ScimToken)
    infoSchema <- declareSchemaRef (Proxy @ScimTokenInfo)
    return $ NamedSchema (Just "CreateScimTokenResponse") $
      mempty
        & type_ .~ Just SwaggerObject
        & properties
          .~ [ ("token", tokenSchema),
               ("info", infoSchema)
             ]
        & required .~ ["token", "info"]

instance ToSchema ScimTokenList where
  declareNamedSchema _ = do
    infoListSchema <- declareSchemaRef (Proxy @[ScimTokenInfo])
    return $ NamedSchema (Just "ScimTokenList") $
      mempty
        & type_ .~ Just SwaggerObject
        & properties
          .~ [ ("tokens", infoListSchema)
             ]
        & required .~ ["tokens"]
