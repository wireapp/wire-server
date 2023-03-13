{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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
    PasswordLoginData (..),
    SmsLoginData (..),
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
    SomeUserToken (..),
    SomeAccessToken (..),
    UserTokenCookie (..),

    -- * Access
    AccessWithCookie (..),
    Access,
    SomeAccess,

    -- * Servant
    TokenResponse,
  )
where

import Control.Applicative
import Control.Lens ((?~))
import Control.Lens.TH
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson.Types as A
import Data.Bifunctor
import Data.ByteString.Builder
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import Data.Code as Code
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util
import Data.Misc (PlainTextPassword6)
import Data.SOP
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import Data.Time.Clock (UTCTime)
import Data.Tuple.Extra hiding (first)
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Servant
import Web.Cookie
import Wire.API.Routes.MultiVerb
import Wire.API.User.Identity (Email, Phone)
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- LoginId

data LoginId
  = LoginByEmail Email
  | LoginByPhone Phone
  | LoginByHandle Handle
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LoginId)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema LoginId

-- NB. this should fail if (e.g.) the email is present but unparseable even if the JSON contains a valid phone number or handle.
-- See tests in `Test.Wire.API.User.Auth`.
instance ToSchema LoginId where
  schema = object "LoginId" $ loginObjectSchema

loginObjectSchema :: ObjectSchema SwaggerDoc LoginId
loginObjectSchema =
  fromLoginId .= tupleSchema `withParser` validate
  where
    fromLoginId :: LoginId -> (Maybe Email, Maybe Phone, Maybe Handle)
    fromLoginId = \case
      LoginByEmail e -> (Just e, Nothing, Nothing)
      LoginByPhone p -> (Nothing, Just p, Nothing)
      LoginByHandle h -> (Nothing, Nothing, Just h)
    tupleSchema :: ObjectSchema SwaggerDoc (Maybe Email, Maybe Phone, Maybe Handle)
    tupleSchema =
      (,,)
        <$> fst3 .= maybe_ (optField "email" schema)
        <*> snd3 .= maybe_ (optField "phone" schema)
        <*> thd3 .= maybe_ (optField "handle" schema)
    validate :: (Maybe Email, Maybe Phone, Maybe Handle) -> A.Parser LoginId
    validate (mEmail, mPhone, mHandle) =
      maybe (fail "'email', 'phone' or 'handle' required") pure $
        (LoginByEmail <$> mEmail) <|> (LoginByPhone <$> mPhone) <|> (LoginByHandle <$> mHandle)

--------------------------------------------------------------------------------
-- LoginCode

-- | A single-use login code.
newtype LoginCode = LoginCode
  {fromLoginCode :: Text}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema LoginCode

instance ToSchema LoginCode where
  schema = LoginCode <$> fromLoginCode .= text "LoginCode"

-- | Used for internal endpoint only.
data PendingLoginCode = PendingLoginCode
  { pendingLoginCode :: LoginCode,
    pendingLoginTimeout :: Code.Timeout
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PendingLoginCode)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema PendingLoginCode

instance ToSchema PendingLoginCode where
  schema =
    object "PendingLoginCode" $
      PendingLoginCode
        <$> pendingLoginCode .= field "code" schema
        <*> pendingLoginTimeout .= field "expires_in" schema

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
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema SendLoginCode

instance ToSchema SendLoginCode where
  schema =
    objectWithDocModifier
      "SendLoginCode"
      (description ?~ "Payload for requesting a login code to be sent")
      $ SendLoginCode
        <$> lcPhone
          .= fieldWithDocModifier
            "phone"
            (description ?~ "E.164 phone number to send the code to")
            (unnamed schema)
        <*> lcCall
          .= fmap
            (fromMaybe False)
            ( optFieldWithDocModifier
                "voice_call"
                (description ?~ "Request the code with a call instead (default is SMS)")
                schema
            )
        <*> lcForce .= fmap (fromMaybe True) (optField "force" schema)

--------------------------------------------------------------------------------
-- LoginCodeTimeout

-- | A timeout for a new or pending login code.
newtype LoginCodeTimeout = LoginCodeTimeout
  {fromLoginCodeTimeout :: Code.Timeout}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema LoginCodeTimeout

instance ToSchema LoginCodeTimeout where
  schema =
    objectWithDocModifier
      "LoginCodeTimeout"
      (description ?~ "A response for a successfully sent login code")
      $ LoginCodeTimeout
        <$> fromLoginCodeTimeout
          .= fieldWithDocModifier
            "expires_in"
            (description ?~ "Number of seconds before the login code expires")
            (unnamed schema)

--------------------------------------------------------------------------------
-- Cookie

data CookieList = CookieList
  { cookieList :: [Cookie ()]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CookieList)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema CookieList

instance ToSchema CookieList where
  schema =
    objectWithDocModifier
      "CookieList"
      (description ?~ "List of cookie information")
      $ CookieList
        <$> cookieList .= field "cookies" (array schema)

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

instance ToSchema (Cookie ()) where
  schema =
    object "Cookie" $
      Cookie
        <$> cookieId .= field "id" schema
        <*> cookieType .= field "type" schema
        <*> cookieCreated .= field "created" utcTimeSchema
        <*> cookieExpires .= field "expires" utcTimeSchema
        <*> cookieLabel .= optField "label" (maybeWithDefault A.Null schema)
        <*> cookieSucc .= optField "successor" (maybeWithDefault A.Null schema)
        <*> cookieValue .= pure ()

deriving via Schema (Cookie ()) instance FromJSON (Cookie ())

deriving via Schema (Cookie ()) instance ToJSON (Cookie ())

deriving via Schema (Cookie ()) instance S.ToSchema (Cookie ())

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
  deriving newtype (ToSchema, FromJSON, ToJSON, Arbitrary)

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
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema CookieType

instance ToSchema CookieType where
  schema =
    enum @Text "CookieType" $
      element "session" SessionCookie
        <> element "persistent" PersistentCookie

--------------------------------------------------------------------------------
-- Login

-- | Different kinds of logins.
data Login
  = PasswordLogin PasswordLoginData
  | SmsLogin SmsLoginData
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Login)

data PasswordLoginData = PasswordLoginData
  { plId :: LoginId,
    plPassword :: PlainTextPassword6,
    plLabel :: Maybe CookieLabel,
    plCode :: Maybe Code.Value
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PasswordLoginData)

passwordLoginSchema :: ObjectSchema SwaggerDoc PasswordLoginData
passwordLoginSchema =
  PasswordLoginData
    <$> plId .= loginObjectSchema
    <*> plPassword .= field "password" schema
    <*> plLabel .= optField "label" (maybeWithDefault A.Null schema)
    <*> plCode .= optField "verification_code" (maybeWithDefault A.Null schema)

data SmsLoginData = SmsLoginData
  { slPhone :: Phone,
    slCode :: LoginCode,
    slLabel :: Maybe CookieLabel
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SmsLoginData)

smsLoginSchema :: ObjectSchema SwaggerDoc SmsLoginData
smsLoginSchema =
  SmsLoginData
    <$> slPhone .= field "phone" schema
    <*> slCode .= field "code" schema
    <*> slLabel
      .= optFieldWithDocModifier
        "label"
        ( description
            ?~ "This label can be used to delete all cookies matching it\
               \ (cf. /cookies/remove)"
        )
        (maybeWithDefault A.Null schema)

$(makePrisms ''Login)

instance ToSchema Login where
  schema =
    object "Login" $
      tag _PasswordLogin passwordLoginSchema
        <> tag _SmsLogin smsLoginSchema

deriving via Schema Login instance FromJSON Login

deriving via Schema Login instance ToJSON Login

deriving via Schema Login instance S.ToSchema Login

loginLabel :: Login -> Maybe CookieLabel
loginLabel (PasswordLogin pl) = plLabel pl
loginLabel (SmsLogin sl) = slLabel sl

--------------------------------------------------------------------------------
-- RemoveCookies

data RemoveCookies = RemoveCookies
  { rmCookiesPassword :: PlainTextPassword6,
    rmCookiesLabels :: [CookieLabel],
    rmCookiesIdents :: [CookieId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoveCookies)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema RemoveCookies

instance ToSchema RemoveCookies where
  schema =
    objectWithDocModifier
      "RemoveCookies"
      (description ?~ "Data required to remove cookies")
      $ RemoveCookies
        <$> rmCookiesPassword
          .= fieldWithDocModifier
            "password"
            (description ?~ "The user's password")
            schema
        <*> rmCookiesLabels
          .= fmap
            fold
            ( optFieldWithDocModifier
                "labels"
                (description ?~ "A list of cookie labels for which to revoke the cookies")
                (array schema)
            )
        <*> rmCookiesIdents
          .= fmap
            fold
            ( optFieldWithDocModifier
                "ids"
                (description ?~ "A list of cookie IDs to revoke")
                (array schema)
            )

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
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema AccessToken

instance ToSchema AccessToken where
  schema =
    object "AccessToken" $
      AccessToken
        <$> user .= field "user" schema
        <*>
        -- FUTUREWORK: if we assume it's valid UTF-8, why not make it 'Text'?
        access
          .= fieldWithDocModifier
            "access_token"
            (description ?~ "The opaque access token string")
            ( LBS.fromStrict . T.encodeUtf8
                <$> (T.decodeUtf8 . LBS.toStrict)
                  .= schema
            )
        <*> tokenType .= field "token_type" schema
        <*> expiresIn
          .= fieldWithDocModifier
            "expires_in"
            (description ?~ "The number of seconds this token is valid")
            schema

bearerToken :: UserId -> LByteString -> Integer -> AccessToken
bearerToken u a = AccessToken u a Bearer

instance Arbitrary AccessToken where
  arbitrary =
    AccessToken
      <$> arbitrary
      <*> (LT.encodeUtf8 <$> arbitrary)
      <*> arbitrary
      <*> arbitrary

data TokenType = Bearer
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TokenType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema TokenType

instance ToSchema TokenType where
  schema = enum @Text "TokenType" $ element "Bearer" Bearer

--------------------------------------------------------------------------------
-- Access

-- summary of types involved:
--
-- user tokens     SomeUserToken = Token User + Token LHUser
-- access tokens   SomeAccessToken = Token Access + Token LHAccess

-- session: Cookie (Token u) (used in DB)

-- cookie: UserTokenCookie

data AccessWithCookie c = Access
  { accessToken :: !AccessToken,
    accessCookie :: !(Maybe c)
  }
  deriving (Functor, Foldable, Traversable)

type Access u = AccessWithCookie (Cookie (ZAuth.Token u))

type SomeAccess = AccessWithCookie UserTokenCookie

instance AsHeaders '[Maybe UserTokenCookie] AccessToken SomeAccess where
  toHeaders (Access at c) = (I c :* Nil, at)
  fromHeaders (I c :* Nil, at) = Access at c

--------------------------------------------------------------------------------
-- Token sum types

data SomeUserToken
  = PlainUserToken (ZAuth.Token ZAuth.User)
  | LHUserToken (ZAuth.Token ZAuth.LegalHoldUser)
  deriving (Show)

instance FromHttpApiData SomeUserToken where
  parseHeader h =
    first T.pack $
      fmap PlainUserToken (runParser parser h)
        <|> fmap LHUserToken (runParser parser h)
  parseUrlPiece = parseHeader . T.encodeUtf8

instance FromByteString SomeUserToken where
  parser =
    PlainUserToken <$> parser
      <|> LHUserToken <$> parser

instance ToByteString SomeUserToken where
  builder (PlainUserToken t) = builder t
  builder (LHUserToken t) = builder t

data SomeAccessToken
  = PlainAccessToken (ZAuth.Token ZAuth.Access)
  | LHAccessToken (ZAuth.Token ZAuth.LegalHoldAccess)
  deriving (Show)

instance FromHttpApiData SomeAccessToken where
  parseHeader h =
    first T.pack $
      fmap PlainAccessToken (runParser parser h)
        <|> fmap LHAccessToken (runParser parser h)
  parseUrlPiece = parseHeader . T.encodeUtf8

-- | Data that is returned to the client in the form of a cookie containing a
-- user token.
data UserTokenCookie = UserTokenCookie
  { utcExpires :: Maybe UTCTime,
    utcToken :: SomeUserToken,
    utcSecure :: Bool
  }

utcFromSetCookie :: SetCookie -> Either Text UserTokenCookie
utcFromSetCookie c = do
  v <- first T.pack $ runParser parser (setCookieValue c)
  pure
    UserTokenCookie
      { utcToken = v,
        utcExpires = setCookieExpires c,
        utcSecure = setCookieSecure c
      }

utcToSetCookie :: UserTokenCookie -> SetCookie
utcToSetCookie c =
  def
    { setCookieName = "zuid",
      setCookieValue = toByteString' (utcToken c),
      setCookiePath = Just "/access",
      setCookieExpires = utcExpires c,
      setCookieSecure = utcSecure c,
      setCookieHttpOnly = True
    }

instance S.ToParamSchema UserTokenCookie where
  toParamSchema _ = mempty & S.type_ ?~ S.SwaggerString

instance FromHttpApiData UserTokenCookie where
  parseHeader = utcFromSetCookie . parseSetCookie
  parseUrlPiece = parseHeader . T.encodeUtf8

instance ToHttpApiData UserTokenCookie where
  toHeader =
    LBS.toStrict
      . toLazyByteString
      . renderSetCookie
      . utcToSetCookie
  toUrlPiece = T.decodeUtf8 . toHeader

--------------------------------------------------------------------------------
-- Servant

type TokenResponse =
  WithHeaders
    '[OptHeader (Header "Set-Cookie" UserTokenCookie)]
    SomeAccess
    (Respond 200 "OK" AccessToken)
