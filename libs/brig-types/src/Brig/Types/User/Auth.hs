{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Brig.Types.User.Auth where

import Brig.Types.Code
import Brig.Types.Common
import Control.Applicative
import Data.Aeson
import Data.ByteString.Conversion
import Data.ByteString.Lazy (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import Data.Id (UserId)
import Data.Misc (PlainTextPassword (..))
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word

-----------------------------------------------------------------------------
-- Login / Authentication

data PendingLoginCode = PendingLoginCode
    { pendingLoginCode    :: !LoginCode
    , pendingLoginTimeout :: !Timeout
    } deriving (Eq)

-- | A single-use login code.
newtype LoginCode = LoginCode
    { fromLoginCode :: Text }
    deriving (Eq, FromJSON, ToJSON)

-- | A request for sending a 'LoginCode'
data SendLoginCode = SendLoginCode
    { lcPhone :: !Phone
    , lcCall  :: !Bool
    , lcForce :: !Bool
    }

-- | A timeout for a new or pending login code.
newtype LoginCodeTimeout = LoginCodeTimeout
    { fromLoginCodeTimeout :: Timeout }
    deriving (Eq, Show)

data Login
    = PasswordLogin !LoginId !PlainTextPassword !(Maybe CookieLabel)
    | SmsLogin !Phone !LoginCode !(Maybe CookieLabel)

loginLabel :: Login -> Maybe CookieLabel
loginLabel (PasswordLogin _ _ l) = l
loginLabel (SmsLogin      _ _ l) = l

data LoginId
    = LoginByEmail  !Email
    | LoginByPhone  !Phone
    | LoginByHandle !Handle

instance FromJSON Login where
    parseJSON = withObject "Login" $ \o -> do
        passw <- o .:? "password"
        case passw of
            Nothing -> SmsLogin <$> o .: "phone" <*> o .: "code" <*> o .:? "label"
            Just pw -> PasswordLogin <$> pwLoginId o <*> pure pw <*> o .:? "label"
      where
        pwLoginId o = do
            email  <- fmap LoginByEmail  <$> (o .:? "email")
            phone  <- fmap LoginByPhone  <$> (o .:? "phone")
            handle <- fmap LoginByHandle <$> (o .:? "handle")
            maybe (fail "'email', 'phone' or 'handle' required")
                  pure
                  (email <|> phone <|> handle)

instance ToJSON Login where
    toJSON (PasswordLogin (LoginByEmail e) pw l) =
        object [ "email" .= e, "password" .= pw, "label" .= l ]

    toJSON (PasswordLogin (LoginByPhone p) pw l) =
        object [ "phone" .= p, "password" .= pw, "label" .= l ]

    toJSON (PasswordLogin (LoginByHandle h) pw l) =
        object [ "handle" .= h, "password" .= pw, "label" .= l ]

    toJSON (SmsLogin p c l) =
        object [ "phone" .= p, "code"  .= c, "label" .= l ]

instance FromJSON PendingLoginCode where
    parseJSON = withObject "PendingLoginCode" $ \o ->
        PendingLoginCode <$> o .: "code"
                         <*> o .: "expires_in"

instance ToJSON PendingLoginCode where
    toJSON (PendingLoginCode c t) = object
        [ "code" .= c, "expires_in" .= t ]

instance FromJSON SendLoginCode where
    parseJSON = withObject "SendLoginCode" $ \o ->
        SendLoginCode <$> o .: "phone"
                      <*> o .:? "voice_call" .!= False
                      <*> o .:? "force" .!= True

instance ToJSON SendLoginCode where
    toJSON (SendLoginCode p c f) = object
        [ "phone"      .= p
        , "voice_call" .= c
        , "force"      .= f
        ]

instance FromJSON LoginCodeTimeout where
    parseJSON = withObject "LoginCodeTimeout" $ \o ->
        LoginCodeTimeout <$> o .: "expires_in"

instance ToJSON LoginCodeTimeout where
    toJSON (LoginCodeTimeout t) = object [ "expires_in" .= t ]

--------------------------------------------------------------------------------
-- Cookies & Access Tokens

-- | A temporary API access token.
data AccessToken = AccessToken
    { user      :: !UserId
    , access    :: !ByteString -- accessTokenValue
    , tokenType :: !TokenType  -- accessTokenType
    , expiresIn :: !Integer    -- accessTokenExpiresIn
    }

data TokenType = Bearer deriving Show

bearerToken :: UserId -> ByteString -> Integer -> AccessToken
bearerToken u a = AccessToken u a Bearer

data RemoveCookies = RemoveCookies
    { rmCookiesPassword :: !PlainTextPassword
    , rmCookiesLabels   :: [CookieLabel]
    , rmCookiesIdents   :: [CookieId]
    }

-- | A device-specific identifying label for one or more cookies.
-- Cookies can be listed and deleted based on their labels.
newtype CookieLabel = CookieLabel
    { cookieLabelText :: Text }
    deriving (Eq, Show, Ord, FromJSON, ToJSON, FromByteString, ToByteString, IsString)

newtype CookieId = CookieId
    { cookieIdNum :: Word32 }
    deriving (Eq, Show, FromJSON, ToJSON)

-- | A (long-lived) cookie scoped to a specific user for obtaining new
-- 'AccessToken's.
data Cookie a = Cookie
    { cookieId      :: !CookieId
    , cookieType    :: !CookieType
    , cookieCreated :: !UTCTime
    , cookieExpires :: !UTCTime
    , cookieLabel   :: !(Maybe CookieLabel)
    , cookieSucc    :: !(Maybe CookieId)
    , cookieValue   :: !a
    } deriving (Eq, Show)

data CookieList = CookieList
    { cookieList :: [Cookie ()]
    }

data CookieType
    = SessionCookie
        -- ^ A session cookie. These are mainly intended for clients
        -- that are web browsers. For other clients, session cookies
        -- behave like regular persistent cookies except for the fact
        -- that they are never renewed during a token refresh and that
        -- they have a shorter lifetime.
    | PersistentCookie
        -- ^ A regular persistent cookie that expires at a specific date.
        -- These cookies are regularly renewed as part of an access token
        -- refresh.
    deriving (Eq, Show)

instance ToJSON AccessToken where
    toJSON (AccessToken u t tt e) =
        object [ "user"         .= u
               , "access_token" .= decodeUtf8 t
               , "token_type"   .= tt
               , "expires_in"   .= e
               ]

instance FromJSON AccessToken where
    parseJSON = withObject "AccessToken" $ \o ->
        AccessToken <$> o .: "user"
                    <*> (encodeUtf8 <$> o .: "access_token")
                    <*> o .: "token_type"
                    <*> o .: "expires_in"

instance ToJSON TokenType where
    toJSON Bearer = toJSON ("Bearer" :: Text)

instance FromJSON TokenType where
    parseJSON (String "Bearer") = return Bearer
    parseJSON _                 = fail "Invalid token type"

instance FromJSON RemoveCookies where
    parseJSON = withObject "remove" $ \o ->
        RemoveCookies <$> o .:  "password"
                      <*> o .:? "labels" .!= []
                      <*> o .:? "ids"    .!= []

instance ToJSON (Cookie ()) where
    toJSON c = object [ "id"        .= cookieId c
                      , "created"   .= cookieCreated c
                      , "expires"   .= cookieExpires c
                      , "label"     .= cookieLabel c
                      , "type"      .= cookieType c
                      , "successor" .= cookieSucc c
                      ]

instance FromJSON (Cookie ()) where
    parseJSON = withObject "cookie" $ \o ->
        Cookie <$> o .:  "id"
               <*> o .:  "type"
               <*> o .:  "created"
               <*> o .:  "expires"
               <*> o .:? "label"
               <*> o .:? "successor"
               <*> pure ()

instance ToJSON CookieList where
    toJSON c = object [ "cookies" .= cookieList c ]

instance FromJSON CookieList where
    parseJSON = withObject "CookieList" $ \o ->
        CookieList <$> o .: "cookies"

instance ToJSON CookieType where
    toJSON SessionCookie    = "session"
    toJSON PersistentCookie = "persistent"

instance FromJSON CookieType where
    parseJSON (String "session")    = return SessionCookie
    parseJSON (String "persistent") = return PersistentCookie
    parseJSON _                     = fail "Invalid cookie type"
