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

module Wire.API.MLS.Servant (MLS, mimeUnrenderMLSWith, CommitBundleMimeType) where

import Data.Bifunctor
import Data.Binary
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Imports
import Network.HTTP.Media ((//))
import Servant.API hiding (Get)
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.PublicGroupState (OpaquePublicGroupState, unOpaquePublicGroupState)
import Wire.API.MLS.Serialisation

data MLS

instance Accept MLS where
  contentType _ = "message" // "mls"

instance {-# OVERLAPPABLE #-} ParseMLS a => MimeUnrender MLS a where
  mimeUnrender _ = mimeUnrenderMLSWith parseMLS

instance MimeRender MLS OpaquePublicGroupState where
  mimeRender _ = LBS.fromStrict . unOpaquePublicGroupState

mimeUnrenderMLSWith :: Get a -> LByteString -> Either String a
mimeUnrenderMLSWith p = first T.unpack . decodeMLSWith p

data CommitBundleMimeType

instance Accept CommitBundleMimeType where
  contentType _ = "application" // "x-protobuf"

instance MimeUnrender CommitBundleMimeType CommitBundle where
  mimeUnrender _ = first T.unpack . deserializeCommitBundle . LBS.toStrict

instance MimeRender CommitBundleMimeType CommitBundle where
  mimeRender _ = LBS.fromStrict . serializeCommitBundle
