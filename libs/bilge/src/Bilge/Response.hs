{-# LANGUAGE OverloadedStrings #-}

module Bilge.Response
    ( -- * Helpers
      statusCode
    , statusMessage
    , getHeader
    , getHeader'
    , getCookie
    , getCookieValue
    , showResponse

      -- * Re-exports
    , Response
    , responseStatus
    , responseHeaders
    , responseVersion
    , responseBody
    , responseJson
    ) where

import Imports
import Control.Lens
import Data.Aeson (FromJSON, eitherDecode)
import Data.CaseInsensitive (original)
import Network.HTTP.Client
import Network.HTTP.Types (HeaderName, httpMajor, httpMinor)
import Web.Cookie

import qualified Data.ByteString.Char8 as C
import qualified Network.HTTP.Types    as HTTP

statusCode :: Response a -> Int
statusCode = HTTP.statusCode . responseStatus

statusMessage :: Response a -> ByteString
statusMessage = HTTP.statusMessage . responseStatus

getHeader :: HeaderName -> Response a -> Maybe ByteString
getHeader h = lookup h . responseHeaders

-- | Like 'getHeader', but if no value exists for the given key, return the
-- static ByteString \"NO_HEADER_VALUE\".
getHeader' :: HeaderName -> Response a -> ByteString
getHeader' h = fromMaybe "NO_HEADER_VALUE" . getHeader h

getCookie :: ByteString -> Response a -> Maybe Cookie
getCookie n r = find ((n ==) . cookie_name) (destroyCookieJar $ responseCookieJar r)

-- | Retrieve the value of a given cookie name from a "Set-Cookie" header on the response
getCookieValue :: ByteString -> Response a -> Maybe ByteString
getCookieValue cookieName resp =
        resp
        ^? to responseHeaders
        . traversed -- Over each header
        . filtered ((== "Set-Cookie") . fst) -- Select the cookie headers by name
        . _2 -- Select Set-Cookie values
        . to parseSetCookie
        . filtered ((== cookieName) . setCookieName) -- Select only the cookie we want
        . to setCookieValue -- extract the cookie value


showResponse :: Show a => Response a -> String
showResponse r = showString "HTTP/"
    . shows (httpMajor . responseVersion $ r)
    . showString "."
    . shows (httpMinor . responseVersion $ r)
    . showString " "
    . shows (statusCode r)
    . showString " "
    . showString (C.unpack $ statusMessage r)
    . showString "\n"
    . showHeaders
    . showString "\n\n"
    . shows (responseBody r)
    $ ""
  where
    showHeaders = foldl' (.) (showString "") (map showHdr (responseHeaders r))
    showHdr (k, v) = showString . C.unpack $ original k <> ": " <> v <> "\n"


responseJson :: FromJSON a => Response (Maybe LByteString) -> Either String a
responseJson resp = case responseBody resp of
    Nothing  -> Left "no body"
    Just raw -> eitherDecode raw
