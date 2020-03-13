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
      :| "application" // "scim+json"
      : "application" // "json" /: ("charset", "utf-8")
      : "application" // "json"
      : []

instance ToJSON a => MimeRender SCIM a where
  mimeRender _ = mimeRender (Proxy @JSON)

instance FromJSON a => MimeUnrender SCIM a where
  mimeUnrender _ = mimeUnrender (Proxy @JSON)
