{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.User.Auth
  ( -- * Login
    Login (..),
    loginLabel,
    LoginCode (..),
    LoginId (..),
    PendingLoginCode (..),
    SendLoginCode (..),
    LoginCodeTimeout (..),

    -- * Cookies
    CookieList (..),
    CookieId (..),
    CookieType (..),
    Cookie (..),
    CookieLabel (..),
    RemoveCookies (..),

    -- * Token
    AccessToken (..),
    bearerToken,
    TokenType (..),

    -- * Swagger
    modelSendLoginCode,
    modelLoginCodeResponse,
    modelLogin,
    modelRemoveCookies,
    modelCookie,
    modelCookieList,
    modelAccessToken,
  )
where

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Conversion
import Data.Code as Code
import Data.Id (UserId)
import Data.Misc (PlainTextPassword (..))
import Data.Schema (ToSchema)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Imports
import Wire.API.User.Auth2
import Wire.API.User.Identity (Phone)
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- Login

-- | Different kinds of logins.
data Login
  = PasswordLogin LoginId PlainTextPassword (Maybe CookieLabel) (Maybe Code.Value)
  | SmsLogin Phone LoginCode (Maybe CookieLabel)
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Login)

modelLogin :: Doc.Model
modelLogin = Doc.defineModel "Login" $ do
  Doc.description "Payload for performing a login."
  Doc.property "email" Doc.string' $ do
    Doc.description "The email address for a password login."
    Doc.optional
  Doc.property "phone" Doc.string' $ do
    Doc.description "The phone number for a password or SMS login."
    Doc.optional
  Doc.property "handle" Doc.string' $ do
    Doc.description "The handle for a password login."
    Doc.optional
  Doc.property "password" Doc.string' $ do
    Doc.description "The password for a password login."
    Doc.optional
  Doc.property "code" Doc.string' $ do
    Doc.description "The login code for an SMS login."
    Doc.optional
  Doc.property "label" Doc.string' $ do
    Doc.description
      "A label to associate with the returned cookie. \
      \Every client should have a unique and stable (persistent) label \
      \to allow targeted revocation of all cookies granted to that \
      \specific client."
    Doc.optional
  Doc.property "verification_code" Doc.string' $ do
    Doc.description "The login verification code for 2nd factor authentication. Required only if SndFactorPasswordChallenge is enabled for the team/server."
    Doc.optional

instance ToJSON Login where
  toJSON (SmsLogin p c l) = object ["phone" .= p, "code" .= c, "label" .= l]
  toJSON (PasswordLogin login password label mbCode) =
    object
      [ "password" .= password,
        "label" .= label,
        loginIdPair login,
        "verification_code" .= mbCode
      ]

instance FromJSON Login where
  parseJSON = withObject "Login" $ \o -> do
    passw <- o .:? "password"
    case passw of
      Nothing ->
        SmsLogin <$> o .: "phone" <*> o .: "code" <*> o .:? "label"
      Just pw -> do
        loginId <- parseJSON (Object o)
        PasswordLogin loginId pw <$> (o .:? "label") <*> (o .:? "verification_code")

loginLabel :: Login -> Maybe CookieLabel
loginLabel (PasswordLogin _ _ l _) = l
loginLabel (SmsLogin _ _ l) = l

loginIdPair :: LoginId -> Aeson.Pair
loginIdPair = \case
  LoginByEmail s -> "email" .= s
  LoginByPhone s -> "phone" .= s
  LoginByHandle s -> "handle" .= s

--------------------------------------------------------------------------------
-- LoginCode

-- | A single-use login code.
newtype LoginCode = LoginCode
  {fromLoginCode :: Text}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON, Arbitrary)

-- | Used for internal endpoint only.
data PendingLoginCode = PendingLoginCode
  { pendingLoginCode :: LoginCode,
    pendingLoginTimeout :: Code.Timeout
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PendingLoginCode)

instance ToJSON PendingLoginCode where
  toJSON (PendingLoginCode c t) =
    object
      ["code" .= c, "expires_in" .= t]

instance FromJSON PendingLoginCode where
  parseJSON = withObject "PendingLoginCode" $ \o ->
    PendingLoginCode
      <$> o .: "code"
      <*> o .: "expires_in"

--------------------------------------------------------------------------------
-- SendLoginCode

-- | A request for sending a 'LoginCode'
data SendLoginCode = SendLoginCode
  { lcPhone :: Phone,
    lcCall :: Bool,
    lcForce :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SendLoginCode)

modelSendLoginCode :: Doc.Model
modelSendLoginCode = Doc.defineModel "SendLoginCode" $ do
  Doc.description "Payload for requesting a login code to be sent."
  Doc.property "phone" Doc.string' $
    Doc.description "E.164 phone number to send the code to."
  Doc.property "voice_call" Doc.bool' $ do
    Doc.description "Request the code with a call instead (default is SMS)."
    Doc.optional

instance ToJSON SendLoginCode where
  toJSON (SendLoginCode p c f) =
    object
      [ "phone" .= p,
        "voice_call" .= c,
        "force" .= f
      ]

instance FromJSON SendLoginCode where
  parseJSON = withObject "SendLoginCode" $ \o ->
    SendLoginCode
      <$> o .: "phone"
      <*> o .:? "voice_call" .!= False
      <*> o .:? "force" .!= True

--------------------------------------------------------------------------------
-- LoginCodeTimeout

-- | A timeout for a new or pending login code.
newtype LoginCodeTimeout = LoginCodeTimeout
  {fromLoginCodeTimeout :: Code.Timeout}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)

modelLoginCodeResponse :: Doc.Model
modelLoginCodeResponse = Doc.defineModel "LoginCodeResponse" $ do
  Doc.description "A response for a successfully sent login code."
  Doc.property "expires_in" Doc.int32' $
    Doc.description "Number of seconds before the login code expires."

instance ToJSON LoginCodeTimeout where
  toJSON (LoginCodeTimeout t) = object ["expires_in" .= t]

instance FromJSON LoginCodeTimeout where
  parseJSON = withObject "LoginCodeTimeout" $ \o ->
    LoginCodeTimeout <$> o .: "expires_in"

--------------------------------------------------------------------------------
-- Cookie

data CookieList = CookieList
  { cookieList :: [Cookie ()]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CookieList)

modelCookieList :: Doc.Model
modelCookieList = Doc.defineModel "CookieList" $ do
  Doc.description "List of cookie information"
  Doc.property "cookies" (Doc.array (Doc.ref modelCookie)) Doc.end

instance ToJSON CookieList where
  toJSON c = object ["cookies" .= cookieList c]

instance FromJSON CookieList where
  parseJSON = withObject "CookieList" $ \o ->
    CookieList <$> o .: "cookies"

-- | A (long-lived) cookie scoped to a specific user for obtaining new
-- 'AccessToken's.
data Cookie a = Cookie
  { cookieId :: CookieId,
    cookieType :: CookieType,
    cookieCreated :: UTCTime,
    cookieExpires :: UTCTime,
    cookieLabel :: Maybe CookieLabel,
    cookieSucc :: Maybe CookieId,
    cookieValue :: a
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform (Cookie a))

modelCookie :: Doc.Model
modelCookie = Doc.defineModel "Cookie" $ do
  Doc.description "Cookie information"
  Doc.property "id" Doc.int32' $
    Doc.description "The primary cookie identifier"
  Doc.property "type" modelTypeCookieType $
    Doc.description "The cookie's type"
  Doc.property "created" Doc.dateTime' $
    Doc.description "The cookie's creation time"
  Doc.property "expires" Doc.dateTime' $
    Doc.description "The cookie's expiration time"
  Doc.property "label" Doc.bytes' $
    Doc.description "The cookie's label"

instance ToJSON (Cookie ()) where
  toJSON c =
    object
      [ "id" .= cookieId c,
        "created" .= cookieCreated c,
        "expires" .= cookieExpires c,
        "label" .= cookieLabel c,
        "type" .= cookieType c,
        "successor" .= cookieSucc c
      ]

instance FromJSON (Cookie ()) where
  parseJSON = withObject "cookie" $ \o ->
    Cookie
      <$> o .: "id"
      <*> o .: "type"
      <*> o .: "created"
      <*> o .: "expires"
      <*> o .:? "label"
      <*> o .:? "successor"
      <*> pure ()

-- | A device-specific identifying label for one or more cookies.
-- Cookies can be listed and deleted based on their labels.
newtype CookieLabel = CookieLabel
  {cookieLabelText :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype
    ( FromJSON,
      ToJSON,
      FromByteString,
      ToByteString,
      IsString,
      Arbitrary,
      S.ToSchema,
      ToSchema
    )

newtype CookieId = CookieId
  {cookieIdNum :: Word32}
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Arbitrary)

data CookieType
  = -- | A session cookie. These are mainly intended for clients
    -- that are web browsers. For other clients, session cookies
    -- behave like regular persistent cookies except for the fact
    -- that they are never renewed during a token refresh and that
    -- they have a shorter lifetime.
    SessionCookie
  | -- | A regular persistent cookie that expires at a specific date.
    -- These cookies are regularly renewed as part of an access token
    -- refresh.
    PersistentCookie
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CookieType)

modelTypeCookieType :: Doc.DataType
modelTypeCookieType =
  Doc.string $
    Doc.enum
      [ "session",
        "persistent"
      ]

instance ToJSON CookieType where
  toJSON SessionCookie = "session"
  toJSON PersistentCookie = "persistent"

instance FromJSON CookieType where
  parseJSON (String "session") = pure SessionCookie
  parseJSON (String "persistent") = pure PersistentCookie
  parseJSON _ = fail "Invalid cookie type"

--------------------------------------------------------------------------------
-- RemoveCookies

data RemoveCookies = RemoveCookies
  { rmCookiesPassword :: PlainTextPassword,
    rmCookiesLabels :: [CookieLabel],
    rmCookiesIdents :: [CookieId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoveCookies)

modelRemoveCookies :: Doc.Model
modelRemoveCookies = Doc.defineModel "RemoveCookies" $ do
  Doc.description "Data required to remove cookies"
  Doc.property "password" Doc.bytes' $
    Doc.description "The user's password"
  Doc.property "labels" (Doc.array Doc.bytes') $ do
    Doc.description "A list of cookie labels for which to revoke the cookies."
    Doc.optional
  Doc.property "ids" (Doc.array Doc.int32') $ do
    Doc.description "A list of cookie IDs to revoke."
    Doc.optional

instance ToJSON RemoveCookies where
  toJSON (RemoveCookies password labels ids) =
    object
      [ "password" .= password,
        "labels" .= labels,
        "ids" .= ids
      ]

instance FromJSON RemoveCookies where
  parseJSON = withObject "remove" $ \o ->
    RemoveCookies
      <$> o .: "password"
      <*> o .:? "labels" .!= []
      <*> o .:? "ids" .!= []

--------------------------------------------------------------------------------
-- Cookies & Access Tokens

-- | A temporary API access token.
data AccessToken = AccessToken
  { user :: UserId,
    -- | FUTUREWORK: must be valid UTF-8 (see ToJSON), encode that in the type!
    access :: LByteString,
    tokenType :: TokenType,
    expiresIn :: Integer
  }
  deriving stock (Eq, Show, Generic)

bearerToken :: UserId -> LByteString -> Integer -> AccessToken
bearerToken u a = AccessToken u a Bearer

modelAccessToken :: Doc.Model
modelAccessToken = Doc.defineModel "AccessToken" $ do
  Doc.description "An API access token."
  Doc.property "access_token" Doc.bytes' $
    Doc.description "The opaque access token string."
  Doc.property "token_type" (Doc.string $ Doc.enum ["Bearer"]) $
    Doc.description "The type of the access token."
  Doc.property "expires_in" Doc.int64' $
    Doc.description "The number of seconds this token is valid."

instance ToJSON AccessToken where
  toJSON (AccessToken u t tt e) =
    object
      [ "user" .= u,
        -- FUTUREWORK: if we assume it's valid UTF-8, why not make it 'Text'?
        "access_token" .= decodeUtf8 t,
        "token_type" .= tt,
        "expires_in" .= e
      ]

instance FromJSON AccessToken where
  parseJSON = withObject "AccessToken" $ \o ->
    AccessToken
      <$> o .: "user"
      <*> (encodeUtf8 <$> o .: "access_token")
      <*> o .: "token_type"
      <*> o .: "expires_in"

instance Arbitrary AccessToken where
  arbitrary =
    AccessToken
      <$> arbitrary
      <*> (encodeUtf8 <$> arbitrary)
      <*> arbitrary
      <*> arbitrary

data TokenType = Bearer
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TokenType)

instance ToJSON TokenType where
  toJSON Bearer = toJSON ("Bearer" :: Text)

instance FromJSON TokenType where
  parseJSON (String "Bearer") = pure Bearer
  parseJSON _ = fail "Invalid token type"
