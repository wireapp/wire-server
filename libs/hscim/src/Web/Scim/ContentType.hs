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

-- | SCIM defines its own content type (application/scim+json). It's
-- intended to be used for all requests and responses; see the first
-- paragraph in the SCIM specification at
-- <https://tools.ietf.org/html/rfc7644#section-3.1>.
--
-- This module contains helpers for handling it. Basically, just write
-- 'SCIM' instead of 'JSON' in all Servant routes.
module Web.Scim.ContentType
  ( SCIM,
  )
where

import Data.Aeson
import Data.List.NonEmpty
import Data.Proxy
import Network.HTTP.Media hiding (Accept)
import Servant.API.ContentTypes

data SCIM

instance Accept SCIM where
  contentTypes _ =
    "application" // "scim+json" /: ("charset", "utf-8")
      :| "application" // "scim+json" :
    "application" // "json" /: ("charset", "utf-8") :
    "application" // "json" :
    []

instance ToJSON a => MimeRender SCIM a where
  mimeRender _ = mimeRender (Proxy @JSON)

instance FromJSON a => MimeUnrender SCIM a where
  mimeUnrender _ = mimeUnrender (Proxy @JSON)
