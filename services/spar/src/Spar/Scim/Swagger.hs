{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Swagger hiding (Header (..))
import Data.Time
import Imports
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
    return $
      NamedSchema (Just "ScimTokenInfo") $
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
    return $
      NamedSchema (Just "CreateScimToken") $
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
    return $
      NamedSchema (Just "CreateScimTokenResponse") $
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
    return $
      NamedSchema (Just "ScimTokenList") $
        mempty
          & type_ .~ Just SwaggerObject
          & properties
            .~ [ ("tokens", infoListSchema)
               ]
          & required .~ ["tokens"]
