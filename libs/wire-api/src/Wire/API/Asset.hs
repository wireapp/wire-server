{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.Asset
  ( module V3,
    AssetLocation (..),
    LocalOrRemoteAsset (..),
  )
where

import Data.SOP
import qualified Data.Swagger as Swagger
import GHC.TypeLits
import Imports
import Servant
import Wire.API.Asset.V3 as V3
import Wire.API.ErrorDescription
import Wire.API.Routes.MultiVerb

newtype AssetLocation = AssetLocation {getAssetLocation :: Text}
  deriving newtype
    ( ToHttpApiData,
      FromHttpApiData,
      Swagger.ToParamSchema
    )

instance AsHeaders '[AssetLocation] Asset (Asset, AssetLocation) where
  toHeaders (asset, loc) = (I loc :* Nil, asset)
  fromHeaders (I loc :* Nil, asset) = (asset, loc)

-- | An asset as returned by the download API: if the asset is local, only a
-- URL is returned, and if it is remote the content of the asset is streamed.
data LocalOrRemoteAsset
  = LocalAsset AssetLocation
  | RemoteAsset (SourceIO ByteString)

instance
  ( ResponseType r0 ~ ErrorDescription code label desc,
    ResponseType r1 ~ AssetLocation,
    ResponseType r2 ~ SourceIO ByteString,
    KnownSymbol desc
  ) =>
  AsUnion '[r0, r1, r2] (Maybe LocalOrRemoteAsset)
  where
  toUnion Nothing = Z (I mkErrorDescription)
  toUnion (Just (LocalAsset loc)) = S (Z (I loc))
  toUnion (Just (RemoteAsset asset)) = S (S (Z (I asset)))

  fromUnion (Z (I _)) = Nothing
  fromUnion (S (Z (I loc))) = Just (LocalAsset loc)
  fromUnion (S (S (Z (I asset)))) = Just (RemoteAsset asset)
  fromUnion (S (S (S x))) = case x of
