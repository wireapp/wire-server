{-# LANGUAGE OverloadedStrings #-}

module Bilge.Response
    ( -- * Helpers
      statusCode
    , statusMessage
    , getHeader
    , getHeader'
    , getCookie
    , showResponse

      -- * Re-exports
    , Response
    , responseStatus
    , responseHeaders
    , responseVersion
    , responseBody
    ) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (original)
import Data.List (foldl', find)
import Data.Maybe
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Types (HeaderName, httpMajor, httpMinor)

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
