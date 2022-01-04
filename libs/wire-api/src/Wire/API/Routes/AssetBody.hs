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

module Wire.API.Routes.AssetBody
  ( AssetBody,
    AssetSource (..),
  )
where

import Conduit
import qualified Data.ByteString.Lazy as LBS
import Data.Swagger
import Data.Swagger.Internal.Schema
import Imports
import Network.HTTP.Media ((//))
import Servant
import Servant.Conduit ()
import Servant.Swagger.Internal.Orphans ()

data MultipartMixed

instance Accept MultipartMixed where
  contentType _ = "multipart" // "mixed"

instance MimeUnrender MultipartMixed ByteString where
  mimeUnrender _ = pure . LBS.toStrict

newtype AssetSource = AssetSource
  { getAssetSource ::
      ConduitT () ByteString (ResourceT IO) ()
  }
  deriving newtype (FromSourceIO ByteString)

instance ToSchema AssetSource where
  declareNamedSchema _ = pure $ named "AssetSource" $ mempty

type AssetBody =
  StreamBody'
    '[ Description
         "A body with content type `multipart/mixed body`. The first section's \
         \content type should be `application/json`. The second section's content \
         \type should be always be `application/octet-stream`. Other content types \
         \will be ignored by the server."
     ]
    NoFraming
    MultipartMixed
    AssetSource
