{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Network.Wire.Client.API.Auth
    ( login
    , refreshAuth
    , addAuth
    , Auth (..)
    , AuthCookie
    , module Auth
    ) where

import Bilge
import Brig.Types.User.Auth as Auth hiding (Cookie, user)
import Control.Monad.IO.Class
import Data.List.NonEmpty
import Data.Monoid
import Data.Id (UserId)
import Data.Time (getCurrentTime)
import Data.ByteString.Conversion (toByteString')
import Network.HTTP.Client (generateCookie)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Monad
import Web.Cookie (parseSetCookie)

import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.Char8 as C
import qualified Data.Text.Encoding    as T

newtype AuthCookie = AuthCookie Cookie

-- | Authentication details sent during an API call.
data Auth
    -- | No authentication
    = NoAuth

    -- | Authentication for calls that pass nginz (i.e. all official instances)
    | PublicAuth
        { authCookie :: !AuthCookie
        , authToken  :: !AccessToken
        }

    -- | Authentication by providing a Z-User header (for e.g. integration tests)
    | ZUserAuth
        { authUser :: !UserId
        }

-------------------------------------------------------------------------------
-- Unauthenticated

login :: MonadClient m => Login -> m (Maybe Auth)
login l = do
    token <- clientRequest Brig req rsc consumeBody
    server <- getServer Brig
    liftIO $ tokenResponse (setServer server req) token Nothing
  where
    req = method POST
        . path "/login"
        . acceptJson
        . json l
        $ empty
    rsc = status200 :| [status403]

-------------------------------------------------------------------------------
-- Authenticated

refreshAuth :: MonadClient m => Auth -> m (Maybe Auth)
refreshAuth (PublicAuth ac@(AuthCookie c) t) = do
    server <- getServer Brig
    rs <- clientRequest Brig (req server) rsc consumeBody
    liftIO $ tokenResponse (setServer server $ req server) rs (Just ac)
  where
    req s = method POST
          . path "/access"
          . acceptJson
          . setCookie s
          . addAuth (PublicAuth ac t)
          $ empty
    rsc = status200 :| [status403]
    setCookie sv
        | serverSSL sv = cookie c
        | otherwise    = header "Cookie" (cookie_name c <> "=" <> cookie_value c)
refreshAuth auth@(ZUserAuth _) = pure (Just auth)
refreshAuth auth@NoAuth = pure (Just auth)

-------------------------------------------------------------------------------
-- Utilities

-- | Add authentication headers to a request.
addAuth :: Auth -> Request -> Request
addAuth (PublicAuth _ t) =
    header "Authorization" (tokType <> " " <> tokValue)
  where
    tokType  = C.pack (show (tokenType t))
    tokValue = Lazy.toStrict (access t)
addAuth (ZUserAuth uid) =
    header "Z-User" (toByteString' uid)
addAuth NoAuth = id

-- | Construct an 'Auth'orisation out of an access token response.
tokenResponse :: Request
                  -- ^ The associated request (for cookie verification).
              -> Response (Maybe Lazy.ByteString)
                  -- ^ The access token response.
              -> Maybe AuthCookie
                  -- ^ The cookie to use if none is provided in the response.
              -> IO (Maybe Auth)
tokenResponse rq rs ck
    | statusCode rs == 200 = mkAuth
    | statusCode rs == 403 = return Nothing
    | otherwise            = unexpected rs "tokenResponse: status code"
  where
    mkAuth = do
        cok <- mkCookie $ parseSetCookie <$> getHeader "Set-Cookie" rs
        tok <- fromBody rs
        return . Just $ PublicAuth cok tok

    mkCookie Nothing    = maybe (unexpected rs "missing set-cookie") return ck
    mkCookie (Just hdr) = do
        now <- getCurrentTime
        case generateCookie hdr rq now True of
            Just cok | cookie_name cok == "zuid" -> return $ AuthCookie cok
            Just (cookie_name -> cok)            -> unexpected rs $ "unknown cookie: " <> T.decodeLatin1 cok
            Nothing                              -> unexpected rs "invalid cookie"
