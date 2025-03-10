{-# LANGUAGE OverloadedStrings #-}

module SAML2.WebSSO.Servant
  ( module SAML2.WebSSO.Servant,
    module SAML2.WebSSO.Servant.CPP,
  )
where

import Data.EitherR
import Data.Function
import Data.List (nubBy)
import qualified Data.Map as Map
import Data.Proxy
import Data.String.Conversions
import Network.HTTP.Media ((//))
import Network.HTTP.Types
import qualified Network.HTTP.Types.Header as HttpTypes
import Network.Wai hiding (Response)
import Network.Wai.Internal as Wai
import SAML2.WebSSO.Servant.CPP
import SAML2.WebSSO.XML
import Servant.API as Servant hiding (MkLink, URI (..))
import Text.Hamlet.XML
import Text.XML

type GetRedir = Verb 'GET 307

type PostRedir = Verb 'POST 303

-- | There is a tiny package `servant-xml`, which does essentially what this type and its
-- 'Mime{,Un}Render' instances do, but inlining this package seems easier.
data XML

instance Accept XML where
  contentType Proxy = "application" // "xml"

instance {-# OVERLAPPABLE #-} HasXMLRoot a => MimeRender XML a where
  mimeRender Proxy = cs . encode

instance {-# OVERLAPPABLE #-} HasXMLRoot a => MimeUnrender XML a where
  mimeUnrender Proxy = fmapL show . decode . cs

data HTML

instance Accept HTML where
  contentType Proxy = "text" // "html"

instance MimeRender HTML ST where
  mimeRender Proxy msg =
    mkHtml
      [xml|
      <body>
        <p>
          #{msg}
    |]

mkHtml :: [Node] -> LBS
mkHtml nodes = renderLBS def doc
  where
    doc = Document (Prologue [] (Just doctyp) []) root []
    doctyp = Doctype "html" (Just $ PublicID "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd")
    root = Element "html" rootattr nodes
    rootattr = Map.fromList [("xmlns", "http://www.w3.org/1999/xhtml"), ("xml:lang", "en")]

-- | [3.5.5.1] Caching
setHttpCachePolicy :: Middleware
setHttpCachePolicy ap rq respond = ap rq $ respond . addHeadersToResponse httpCachePolicy
  where
    httpCachePolicy :: HttpTypes.ResponseHeaders
    httpCachePolicy = [("Cache-Control", "no-cache, no-store"), ("Pragma", "no-cache")]
    addHeadersToResponse :: HttpTypes.ResponseHeaders -> Wai.Response -> Wai.Response
    addHeadersToResponse extraHeaders resp = case resp of
      ResponseFile status hdrs filepath part -> ResponseFile status (updH hdrs) filepath part
      ResponseBuilder status hdrs builder -> ResponseBuilder status (updH hdrs) builder
      ResponseStream status hdrs body -> ResponseStream status (updH hdrs) body
      ResponseRaw action resp' ->
        ResponseRaw action $
          addHeadersToResponse extraHeaders resp'
      where
        updH hdrs = nubBy ((==) `on` fst) $ extraHeaders ++ hdrs
