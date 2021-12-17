{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.Cargohold where

import Data.Id
import Data.Metrics.Servant
import Data.Qualified
import Data.SOP
import qualified Data.Swagger as Swagger
import Imports
import Servant
import Servant.Swagger.Internal
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Asset
import Wire.API.ErrorDescription
import Wire.API.Routes.AssetBody
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Public

data PrincipalTag = UserPrincipalTag | BotPrincipalTag | ProviderPrincipalTag
  deriving (Eq, Show)

type family PrincipalId (tag :: PrincipalTag) = (id :: *) | id -> tag where
  PrincipalId 'UserPrincipalTag = Local UserId
  PrincipalId 'BotPrincipalTag = BotId
  PrincipalId 'ProviderPrincipalTag = ProviderId

type family ApplyPrincipalPath (tag :: PrincipalTag) api

type instance ApplyPrincipalPath 'UserPrincipalTag api = ZLocalUser :> "assets" :> "v3" :> api

type instance ApplyPrincipalPath 'BotPrincipalTag api = ZBot :> "bot" :> "assets" :> api

type instance ApplyPrincipalPath 'ProviderPrincipalTag api = ZProvider :> "provider" :> "assets" :> api

instance HasSwagger (ApplyPrincipalPath tag api) => HasSwagger (tag :> api) where
  toSwagger _ = toSwagger (Proxy @(ApplyPrincipalPath tag api))

instance HasServer (ApplyPrincipalPath tag api) ctx => HasServer (tag :> api) ctx where
  type ServerT (tag :> api) m = ServerT (ApplyPrincipalPath tag api) m
  route _ = route (Proxy @(ApplyPrincipalPath tag api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(ApplyPrincipalPath tag api))

instance RoutesToPaths (ApplyPrincipalPath tag api) => RoutesToPaths (tag :> api) where
  getRoutes = getRoutes @(ApplyPrincipalPath tag api)

newtype AssetLocation = AssetLocation {getAssetLocation :: Text}
  deriving newtype
    ( ToHttpApiData,
      FromHttpApiData,
      Swagger.ToParamSchema
    )

instance AsHeaders '[AssetLocation] Asset (Asset, AssetLocation) where
  toHeaders (asset, loc) = (I loc :* Nil, asset)
  fromHeaders (I loc :* Nil, asset) = (asset, loc)

type GetAsset =
  MultiVerb
    'GET
    '[JSON]
    '[ AssetNotFound,
       WithHeaders
         '[DescHeader "Location" "Asset location" AssetLocation]
         AssetLocation
         (RespondEmpty 302 "Asset found")
     ]
    (Maybe AssetLocation)

type ServantAPI =
  ( Summary "Renew an asset token"
      :> CanThrow AssetNotFound
      :> CanThrow Unauthorised
      :> ZLocalUser
      :> "assets"
      :> "v3"
      :> Capture "key" AssetKey
      :> "token"
      :> Post '[JSON] NewAssetToken
  )
    :<|> ( Summary "Delete an asset token"
             :> Description "**Note**: deleting the token makes the asset public."
             :> ZLocalUser
             :> "assets"
             :> "v3"
             :> Capture "key" AssetKey
             :> "token"
             :> MultiVerb
                  'DELETE
                  '[JSON]
                  '[RespondEmpty 200 "Asset token deleted"]
                  ()
         )
    :<|> BaseAPI 'UserPrincipalTag
    :<|> BaseAPI 'BotPrincipalTag
    :<|> BaseAPI 'ProviderPrincipalTag
    :<|> LegacyAPI
    :<|> InternalAPI

type BaseAPI (tag :: PrincipalTag) =
  ( Summary "Upload an asset"
      :> CanThrow AssetTooLarge
      :> CanThrow InvalidLength
      :> tag
      :> AssetBody
      :> MultiVerb
           'POST
           '[JSON]
           '[ WithHeaders
                '[DescHeader "Location" "Asset location" AssetLocation]
                (Asset, AssetLocation)
                (Respond 201 "Asset posted" Asset)
            ]
           (Asset, AssetLocation)
  )
    :<|> ( Summary "Download an asset"
             :> tag
             :> Capture "key" AssetKey
             :> Header "Asset-Token" AssetToken
             :> GetAsset
         )
    :<|> ( Summary "Delete an asset"
             :> CanThrow AssetNotFound
             :> CanThrow Unauthorised
             :> tag
             :> Capture "key" AssetKey
             :> MultiVerb
                  'DELETE
                  '[JSON]
                  '[RespondEmpty 200 "Asset deleted"]
                  ()
         )

type LegacyAPI =
  ( ZLocalUser
      :> "assets"
      :> QueryParam' [Required, Strict] "conv_id" ConvId
      :> Capture "id" AssetId
      :> GetAsset
  )
    :<|> ( ZLocalUser
             :> "conversations"
             :> Capture "cnv" ConvId
             :> "assets"
             :> Capture "id" AssetId
             :> GetAsset
         )
    :<|> ( ZLocalUser
             :> "conversations"
             :> Capture "cnv" ConvId
             :> "otr"
             :> "assets"
             :> Capture "id" AssetId
             :> GetAsset
         )

type InternalAPI =
  "i" :> "status" :> MultiVerb 'GET '[PlainText] '[RespondEmpty 200 "OK"] ()

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
