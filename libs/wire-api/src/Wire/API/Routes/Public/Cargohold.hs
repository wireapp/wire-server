-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Kind
import Data.Metrics.Servant
import Data.Qualified
import Data.SOP
import Imports
import Servant
import Servant.OpenApi.Internal.Orphans ()
import URI.ByteString
import Wire.API.Asset
import Wire.API.Error
import Wire.API.Error.Cargohold
import Wire.API.MakesFederatedCall
import Wire.API.Routes.API
import Wire.API.Routes.AssetBody
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.QualifiedCapture
import Wire.API.Routes.Version

data PrincipalTag = UserPrincipalTag | BotPrincipalTag | ProviderPrincipalTag
  deriving (Eq, Show)

instance RenderableSymbol UserPrincipalTag where
  renderSymbol = "user"

instance RenderableSymbol BotPrincipalTag where
  renderSymbol = "bot"

instance RenderableSymbol ProviderPrincipalTag where
  renderSymbol = "provider"

type family PrincipalId (tag :: PrincipalTag) = (id :: Type) | id -> tag where
  PrincipalId 'UserPrincipalTag = Local UserId
  PrincipalId 'BotPrincipalTag = BotId
  PrincipalId 'ProviderPrincipalTag = ProviderId

type family ApplyPrincipalPath (tag :: PrincipalTag) api

type instance
  ApplyPrincipalPath 'UserPrincipalTag api =
    ZLocalUser :> Until 'V2 :> "assets" :> "v3" :> api

type instance ApplyPrincipalPath 'BotPrincipalTag api = ZBot :> "bot" :> "assets" :> api

type instance ApplyPrincipalPath 'ProviderPrincipalTag api = ZProvider :> "provider" :> "assets" :> api

type instance
  SpecialiseToVersion v ((tag :: PrincipalTag) :> api) =
    SpecialiseToVersion v (ApplyPrincipalPath tag api)

instance (HasServer (ApplyPrincipalPath tag api) ctx) => HasServer (tag :> api) ctx where
  type ServerT (tag :> api) m = ServerT (ApplyPrincipalPath tag api) m
  route _ = route (Proxy @(ApplyPrincipalPath tag api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(ApplyPrincipalPath tag api))

instance (RoutesToPaths (ApplyPrincipalPath tag api)) => RoutesToPaths (tag :> api) where
  getRoutes = getRoutes @(ApplyPrincipalPath tag api)

type AssetLocationHeader r =
  '[DescHeader "Location" "Asset location" (AssetLocation r)]

type AssetRedirect =
  WithHeaders
    (AssetLocationHeader Absolute)
    (AssetLocation Absolute)
    (RespondEmpty 302 "Asset found")

type AssetStreaming =
  RespondStreaming
    200
    "Asset returned directly with content type `application/octet-stream`"
    NoFraming
    OctetStream

type GetAsset =
  MultiVerb
    'GET
    '[JSON]
    '[ErrorResponse 'AssetNotFound, AssetRedirect]
    (Maybe (AssetLocation Absolute))

type CargoholdAPI =
  ( Summary "Renew an asset token"
      :> Until 'V2
      :> CanThrow 'AssetNotFound
      :> CanThrow 'Unauthorised
      :> ZLocalUser
      :> "assets"
      :> "v3"
      :> Capture "key" AssetKey
      :> "token"
      :> Post '[JSON] NewAssetToken
  )
    :<|> ( Summary "Delete an asset token"
             :> Until 'V2
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
    :<|> BaseAPIv3 'UserPrincipalTag
    :<|> BaseAPIv3 'BotPrincipalTag
    :<|> BaseAPIv3 'ProviderPrincipalTag
    :<|> QualifiedAPI
    :<|> LegacyAPI
    :<|> MainAPI

-- | Asset API for a given principal (user/bot/provider).
--
-- This was introduced before API versioning, and the user endpoints contain a
-- v3 suffix, which is removed starting from API V2.
type BaseAPIv3 (tag :: PrincipalTag) =
  Named
    '("assets-upload-v3", tag)
    ( Summary "Upload an asset"
        :> CanThrow 'AssetTooLarge
        :> CanThrow 'InvalidLength
        :> tag
        :> AssetBody
        :> MultiVerb
             'POST
             '[JSON]
             '[ WithHeaders
                  (AssetLocationHeader Relative)
                  (Asset, AssetLocation Relative)
                  (Respond 201 "Asset posted" Asset)
              ]
             (Asset, AssetLocation Relative)
    )
    :<|> Named
           '("assets-download-v3", tag)
           ( Summary "Download an asset"
               :> tag
               :> Capture "key" AssetKey
               :> Header "Asset-Token" AssetToken
               :> QueryParam "asset_token" AssetToken
               :> ZHostOpt
               :> GetAsset
           )
    :<|> Named
           '("assets-delete-v3", tag)
           ( Summary "Delete an asset"
               :> CanThrow 'AssetNotFound
               :> CanThrow 'Unauthorised
               :> tag
               :> Capture "key" AssetKey
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    '[RespondEmpty 200 "Asset deleted"]
                    ()
           )

-- | Qualified asset API. Only download and delete endpoints are supported, as
-- upload has stayed unqualified. These endpoints also predate API versioning,
-- and contain a v4 suffix.
type QualifiedAPI =
  Named
    "assets-download-v4"
    ( Summary "Download an asset"
        :> Until 'V2
        :> Description
             "**Note**: local assets result in a redirect, \
             \while remote assets are streamed directly."
        :> MakesFederatedCall 'Cargohold "get-asset"
        :> MakesFederatedCall 'Cargohold "stream-asset"
        :> ZLocalUser
        :> "assets"
        :> "v4"
        :> QualifiedCapture "key" AssetKey
        :> Header "Asset-Token" AssetToken
        :> QueryParam "asset_token" AssetToken
        :> ZHostOpt
        :> MultiVerb
             'GET
             '()
             '[ ErrorResponse 'AssetNotFound,
                AssetRedirect,
                AssetStreaming
              ]
             (Maybe LocalOrRemoteAsset)
    )
    :<|> Named
           "assets-delete-v4"
           ( Summary "Delete an asset"
               :> Until 'V2
               :> Description "**Note**: only local assets can be deleted."
               :> CanThrow 'AssetNotFound
               :> CanThrow 'Unauthorised
               :> ZLocalUser
               :> "assets"
               :> "v4"
               :> QualifiedCapture "key" AssetKey
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    '[RespondEmpty 200 "Asset deleted"]
                    ()
           )

-- Old endpoints, predating BaseAPIv3, and therefore API versioning.
type LegacyAPI =
  Named
    "assets-download-legacy"
    ( ZLocalUser
        :> Until 'V2
        :> "assets"
        :> QueryParam' [Required, Strict] "conv_id" ConvId
        :> Capture "id" AssetId
        :> GetAsset
    )
    :<|> Named
           "assets-conv-download-legacy"
           ( ZLocalUser
               :> Until 'V2
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "assets"
               :> Capture "id" AssetId
               :> GetAsset
           )
    :<|> Named
           "assets-conv-otr-download-legacy"
           ( ZLocalUser
               :> Until 'V2
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "otr"
               :> "assets"
               :> Capture "id" AssetId
               :> GetAsset
           )

-- | With API versioning, the previous ad-hoc v3/v4 versioning is abandoned, and
-- asset endpoints are versioned normally as part of the public API, without any
-- explicit prefix.
type MainAPI =
  Named
    "tokens-renew"
    ( Summary "Renew an asset token"
        :> From 'V2
        :> CanThrow 'AssetNotFound
        :> CanThrow 'Unauthorised
        :> ZLocalUser
        :> "assets"
        :> Capture "key" AssetKey
        :> "token"
        :> Post '[JSON] NewAssetToken
    )
    :<|> Named
           "tokens-delete"
           ( Summary "Delete an asset token"
               :> From 'V2
               :> Description "**Note**: deleting the token makes the asset public."
               :> ZLocalUser
               :> "assets"
               :> Capture "key" AssetKey
               :> "token"
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    '[RespondEmpty 200 "Asset token deleted"]
                    ()
           )
    :<|> Named
           "assets-upload"
           ( Summary "Upload an asset"
               :> From 'V2
               :> CanThrow 'AssetTooLarge
               :> CanThrow 'InvalidLength
               :> ZLocalUser
               :> "assets"
               :> AssetBody
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[ WithHeaders
                         (AssetLocationHeader Relative)
                         (Asset, AssetLocation Relative)
                         (Respond 201 "Asset posted" Asset)
                     ]
                    (Asset, AssetLocation Relative)
           )
    :<|> Named
           "assets-download"
           ( Summary "Download an asset"
               :> From 'V2
               :> Description
                    "**Note**: local assets result in a redirect, \
                    \while remote assets are streamed directly."
               :> MakesFederatedCall 'Cargohold "get-asset"
               :> MakesFederatedCall 'Cargohold "stream-asset"
               :> CanThrow 'NoMatchingAssetEndpoint
               :> ZLocalUser
               :> "assets"
               :> QualifiedCapture "key" AssetKey
               :> Header "Asset-Token" AssetToken
               :> QueryParam "asset_token" AssetToken
               :> ZHostOpt
               :> MultiVerb
                    'GET
                    '()
                    '[ ErrorResponse 'AssetNotFound,
                       AssetRedirect,
                       AssetStreaming
                     ]
                    (Maybe LocalOrRemoteAsset)
           )
    :<|> Named
           "assets-delete"
           ( Summary "Delete an asset"
               :> From 'V2
               :> Description "**Note**: only local assets can be deleted."
               :> CanThrow 'AssetNotFound
               :> CanThrow 'Unauthorised
               :> ZLocalUser
               :> "assets"
               :> QualifiedCapture "key" AssetKey
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    '[RespondEmpty 200 "Asset deleted"]
                    ()
           )

data CargoholdAPITag

instance ServiceAPI CargoholdAPITag v where
  type ServiceAPIRoutes CargoholdAPITag = CargoholdAPI
