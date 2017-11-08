{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Utilities.Response where

import Data.Aeson hiding (Error, json)
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal
import Network.Wai.Utilities.Error

import qualified Data.ByteString.Lazy as Lazy

empty :: Response
empty = plain ""

plain :: Lazy.ByteString -> Response
plain = responseLBS status200 [plainContent]

plainContent :: Header
plainContent = (hContentType, "text/plain; charset=UTF-8")

json :: ToJSON a => a -> Response
json = responseLBS status200 [jsonContent] . encode

jsonContent :: Header
jsonContent = (hContentType, "application/json")

errorRs :: Status -> Text -> Text -> Response
errorRs s l m = errorRs' (Error s l m)

errorRs' :: Error -> Response
errorRs' e = setStatus (code e) (json e)

addHeader :: HeaderName -> ByteString -> Response -> Response
addHeader k v (ResponseFile s h f ff) = ResponseFile s ((k, v):h) f ff
addHeader k v (ResponseBuilder s h b) = ResponseBuilder s ((k, v):h) b
addHeader k v (ResponseStream s h x)  = ResponseStream s ((k, v):h) x
addHeader k v (ResponseRaw s r)       = ResponseRaw s (addHeader k v r)

setStatus :: Status -> Response -> Response
setStatus s (ResponseBuilder _ h b) = ResponseBuilder s h b
setStatus s (ResponseStream _ h x)  = ResponseStream s h x
setStatus s (ResponseFile _ h f ff) = ResponseFile s h f ff
setStatus s (ResponseRaw x r)       = ResponseRaw x (setStatus s r)
