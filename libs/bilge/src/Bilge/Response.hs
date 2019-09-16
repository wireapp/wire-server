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
    , ResponseLBS
    , responseStatus
    , responseHeaders
    , responseVersion
    , responseBody
    , responseJsonEither
    , responseJsonMaybe
    , responseJsonThrow
    , responseJsonError
    , responseJsonUnsafe
    , responseJsonUnsafeWithMsg
    ) where

import Imports
import Control.Exception (ErrorCall(ErrorCall))
import Control.Lens
import Control.Monad.Catch
import Data.Aeson (FromJSON, eitherDecode)
import Data.CaseInsensitive (original)
import Data.EitherR (fmapL)
import Data.Typeable (typeRep)
import Network.HTTP.Client
import Network.HTTP.Types (HeaderName, httpMajor, httpMinor)
import Web.Cookie

import qualified Data.Proxy
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


type ResponseLBS = Response (Maybe LByteString)

{-# INLINE responseJsonEither #-}
responseJsonEither
  :: forall a. (HasCallStack, Typeable a, FromJSON a)
  => ResponseLBS -> Either String a
responseJsonEither = fmapL addTypeInfo . eitherDecode <=< maybe err pure . responseBody
  where
    err :: Either String void
    err = Left "Missing response body."

    addTypeInfo :: String -> String
    addTypeInfo = ((show (typeRep (Data.Proxy.Proxy @a)) <> " ") <>)

{-# INLINE responseJsonMaybe #-}
responseJsonMaybe
  :: (HasCallStack, Typeable a, FromJSON a)
  => ResponseLBS -> Maybe a
responseJsonMaybe = either (const Nothing) Just . responseJsonEither

{-# INLINE responseJsonThrow #-}
responseJsonThrow
  :: (HasCallStack, MonadThrow m, Typeable a, FromJSON a, Exception e)
  => (String -> e) -> ResponseLBS -> m a
responseJsonThrow mkErr = either (throwM . mkErr) pure . responseJsonEither

{-# INLINE responseJsonError #-}
responseJsonError
  :: (HasCallStack, MonadThrow m, Typeable a, FromJSON a)
  => ResponseLBS -> m a
responseJsonError = responseJsonThrow ErrorCall

{-# INLINE responseJsonUnsafe #-}
responseJsonUnsafe
  :: (HasCallStack, Typeable a, FromJSON a)
  => ResponseLBS -> a
responseJsonUnsafe = responseJsonUnsafeWithMsg ""

{-# INLINE responseJsonUnsafeWithMsg #-}
responseJsonUnsafeWithMsg
  :: (HasCallStack, Typeable a, FromJSON a)
  => String -> ResponseLBS -> a
responseJsonUnsafeWithMsg userErr = either err id . responseJsonEither
  where
    err parserErr = error . intercalate " " $
        [ "responseJsonUnsafeWithMsg:" ] <>
        [ userErr | not $ null userErr ] <>
        [ parserErr ]

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
