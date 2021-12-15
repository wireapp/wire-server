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
import Data.SOP
import qualified Data.Swagger as Swagger
import GHC.TypeLits
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

data OptionalSegment = NoSegment | Segment Symbol

type family PrincipalPrefix tag :: OptionalSegment where
  PrincipalPrefix 'UserPrincipalTag = 'NoSegment
  PrincipalPrefix 'BotPrincipalTag = 'Segment "bots"
  PrincipalPrefix 'ProviderPrincipalTag = 'Segment "providers"

instance HasSwagger (x :> api) => HasSwagger ('Segment x :> api) where
  toSwagger _ = toSwagger (Proxy @(x :> api))

instance HasSwagger api => HasSwagger ('NoSegment :> api) where
  toSwagger _ = toSwagger (Proxy @api)

instance HasServer (x :> api) ctx => HasServer ('Segment x :> api) ctx where
  type ServerT ('Segment x :> api) m = ServerT (x :> api) m
  route _ = route (Proxy @(x :> api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(x :> api))

instance HasServer api ctx => HasServer ('NoSegment :> api) ctx where
  type ServerT ('NoSegment :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

type family PrincipalId (tag :: PrincipalTag) :: * where
  PrincipalId 'UserPrincipalTag = UserId
  PrincipalId 'BotPrincipalTag = BotId
  PrincipalId 'ProviderPrincipalTag = ProviderId

newtype AssetLocation = AssetLocation {getAssetLocation :: Text}
  deriving newtype
    ( ToHttpApiData,
      FromHttpApiData,
      Swagger.ToParamSchema
    )

instance AsHeaders '[AssetLocation] Asset (Asset, AssetLocation) where
  toHeaders (asset, loc) = (I loc :* Nil, asset)
  fromHeaders (I loc :* Nil, asset) = (asset, loc)

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

type BaseAPI (tag :: PrincipalTag) =
  ( Summary "Upload an asset"
      :> CanThrow AssetTooLarge
      :> CanThrow InvalidLength
      :> ZLocalUser
      :> PrincipalPrefix tag
      :> "assets"
      :> "v3"
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
             :> ZLocalUser
             :> PrincipalPrefix tag
             :> "assets"
             :> "v3"
             :> Capture "key" AssetKey
             :> Header "Asset-Token" AssetToken
             :> MultiVerb
                  'GET
                  '[JSON]
                  '[ AssetNotFound,
                     WithHeaders
                       '[DescHeader "Location" "Asset location" AssetLocation]
                       AssetLocation
                       (RespondEmpty 302 "Asset found")
                   ]
                  (Maybe AssetLocation)
         )
    :<|> ( Summary "Delete an asset"
             :> CanThrow AssetNotFound
             :> CanThrow Unauthorised
             :> ZLocalUser
             :> PrincipalPrefix tag
             :> "assets"
             :> "v3"
             :> Capture "key" AssetKey
             :> MultiVerb
                  'DELETE
                  '[JSON]
                  '[RespondEmpty 200 "Asset deleted"]
                  ()
         )

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)
