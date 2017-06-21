{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Network.Wire.Client.API.Auth
    ( login
    , refreshAuth
    , Auth (..)
    , AuthCookie
    , AuthToken (..)
    , TokenType
    , token
    , module Auth
    ) where

import Bilge
import Brig.Types.User.Auth as Auth (Login)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.ByteString (ByteString)
import Data.List.NonEmpty
import Data.Monoid
import Data.Time (getCurrentTime)
import Data.Time.Clock (NominalDiffTime)
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

data AuthToken = AuthToken
    { accessToken :: !ByteString
    , tokenType   :: !TokenType
    , expiresIn   :: !NominalDiffTime
    }

data Auth = Auth
    { authCookie :: !AuthCookie
    , authToken  :: !AuthToken
    }

instance FromJSON AuthToken where
    parseJSON = withObject "auth-token" $ \o ->
        AuthToken <$> (T.encodeUtf8 <$> o .: "access_token")
                  <*> o .: "token_type"
                  <*> (fromInteger <$> o .: "expires_in")

data TokenType = Bearer deriving (Show)

instance FromJSON TokenType where
    parseJSON (String "Bearer") = return Bearer
    parseJSON _                 = mzero

-------------------------------------------------------------------------------
-- Unauthenticated

login :: MonadClient m => Login -> m (Maybe Auth)
login l = do
    rs <- clientRequest req rsc consumeBody
    sv <- getServer
    liftIO $ tokenResponse (setServer sv req) rs Nothing
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
refreshAuth (Auth ac@(AuthCookie c) t) = do
    sv <- getServer
    rs <- clientRequest (req sv) rsc consumeBody
    liftIO $ tokenResponse (setServer sv $ req sv) rs (Just ac)
  where
    req s = method POST
          . path "/access"
          . acceptJson
          . setCookie s
          . token t
          $ empty
    rsc = status200 :| [status403]

    setCookie sv
        | serverSSL sv = cookie c
        | otherwise    = header "Cookie" (cookie_name c <> "=" <> cookie_value c)

-------------------------------------------------------------------------------
-- Utilities

token :: AuthToken -> Request -> Request
token t = header "Authorization" (tokType <> " " <> tokValue)
  where
    tokType  = C.pack . show $ tokenType t
    tokValue = accessToken t

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
        return . Just $ Auth cok tok

    mkCookie Nothing    = maybe (unexpected rs "missing set-cookie") return ck
    mkCookie (Just hdr) = do
        now <- getCurrentTime
        case generateCookie hdr rq now True of
            Just cok | cookie_name cok == "zuid" -> return $ AuthCookie cok
            Just (cookie_name -> cok)            -> unexpected rs $ "unknown cookie: " <> T.decodeLatin1 cok
            Nothing                              -> unexpected rs "invalid cookie"
