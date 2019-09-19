{-# OPTIONS_GHC -Wno-orphans #-}
-- FUTUREWORK: move the 'ToSchema' instances to their home modules (where the data types
-- live), and turn warning about orphans back on.

module Brig.Types.Servant.Orphans where

import Imports

import qualified "swagger" Data.Swagger.Build.Api as Swagger1
import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.

import Brig.Types.Activation
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth (CookieLabel)
import Control.Lens
import Data.Aeson as Aeson
import Data.ByteString.Conversion (List(..))
import Data.ByteString.Conversion as BSC
import Data.Currency (Alpha)
import Data.Id
import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.Misc
import Data.Proxy
import Data.Range
import Data.String.Conversions (cs)
import Data.Text.Ascii
import Data.Typeable (typeOf)
import Data.UUID (UUID)
import Galley.Types
import Galley.Types.Bot.Service
import Galley.Types.Teams
import qualified Data.Json.Util
import qualified Data.Metrics as Metrics
import qualified Servant
import Servant hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam', URI)
import Servant.Swagger
import URI.ByteString (URI)


----------------------------------------------------------------------
-- * more stuff we need to move to other places

newtype AccountStatusObject = AccountStatusObject AccountStatus
    deriving (Eq, Show, Generic)

instance FromJSON AccountStatusObject where
    parseJSON = withObject "account-status object" $
        \o -> AccountStatusObject <$> o .: "status"

instance ToJSON AccountStatusObject where
    toJSON (AccountStatusObject status) = object [ "status" Aeson..= status ]

data ActivationCodeObject = ActivationCodeObject ActivationKey ActivationCode
    deriving (Eq, Show, Generic)

instance FromJSON ActivationCodeObject where
    parseJSON = withObject "activation code object" $
        \o -> ActivationCodeObject <$> o .: "key" <*> o .: "code"

instance ToJSON ActivationCodeObject where
    toJSON (ActivationCodeObject key code) = object [ "key" Aeson..= key, "code" Aeson..= code ]


----------------------------------------------------------------------
-- * generic servant helpers

type Head = Verb 'HEAD 204 '[JSON]  -- TODO: which status code is this?
type Get = Verb 'GET 200 '[JSON]
type Post = Verb 'POST 201 '[JSON]
type Put204 = Verb 'PUT 204 '[JSON]
type Put200 = Verb 'PUT 200 '[JSON]
type Delete200 = Verb 'DELETE 200 '[JSON]
type Delete202 = Verb 'DELETE 202 '[JSON]

type ReqBody = Servant.ReqBody '[JSON]

type QueryParamStrict = Servant.QueryParam  -- TODO: which one?
type QueryParamOptional = Servant.QueryParam  -- TODO: which one?


----------------------------------------------------------------------
-- * wire auth combinators

type InternalZUser = Header "Z-User" UserId
type InternalZConn = Header "Z-Connection" ConnId

data AuthZUser
data AuthZConn

-- this is fun because we want to generate swagger docs for what this looks like from the
-- other side of nginz, but we still want the handler types for 'HasServer' to pan out.  and
-- it turns out it's quite simple to achieve this!

-- TODO: the 'HasSwagger' instances have the (small?) flaw that functions who require both
-- z-user and z-conn will require the authorization header twice.  perhaps we can write a type
-- family that eliminates all auth headers of type 'Text', and call it here before we inject a
-- new one?

instance HasSwagger api => HasSwagger (AuthZUser :> api) where
    toSwagger _ = toSwagger (Proxy @(Header "Authorization" Text :> api))

instance HasSwagger api => HasSwagger (AuthZConn :> api) where
    toSwagger _ = toSwagger (Proxy @(Header "Authorization" Text :> api))

instance HasServer api ctx => HasServer (AuthZUser :> api) ctx where
    type ServerT (AuthZUser :> api) m =
        ServerT (Header "Z-User" UserId :> api) m

    route _ =
        route (Proxy @(Header "Z-User" UserId :> api))

    hoistServerWithContext _ ctx nt srv =
        hoistServerWithContext (Proxy @(Header "Z-User" UserId :> api)) ctx nt srv

instance HasServer api ctx => HasServer (AuthZConn :> api) ctx where
    type ServerT (AuthZConn :> api) m =
        ServerT (Header "Z-Connection" UserId :> api) m

    route _ =
        route (Proxy @(Header "Z-Connection" UserId :> api))

    hoistServerWithContext _ ctx nt srv =
        hoistServerWithContext (Proxy @(Header "Z-Connection" UserId :> api)) ctx nt srv


----------------------------------------------------------------------
-- * swagger helpers

camelToUnderscore :: String -> String
camelToUnderscore = concatMap go . (ix 0 %~ toLower)
  where go x = if isUpper x then "_" <> [toLower x] else [x]


----------------------------------------------------------------------
-- * orphans

instance {-# OVERLAPPABLE #-} (Typeable a, FromByteString a) => FromHttpApiData a where
  parseUrlPiece = maybe (Left err) Right . BSC.fromByteString . cs
    where
      err = "a ~ " <> showtype  <> ". (Typeable a, FromByteString a) => FromHttpApiData a"
      showtype = cs . show . typeOf $ (undefined :: a)

instance ToParamSchema (Id a) where
    toParamSchema _ = toParamSchema (Proxy @UUID)
    -- FUTUREWORK: @& description .~ Just (... :: Text)@ would be nice here, but will require
    -- a patch to swagger2, I think.  and we need to think of a clever way to get from "any"
    -- in the instance head back to "AnyId".  (the dumb way would also work, just writing 5
    -- instances.)
    -- Same for all ToParamSchema instances below.

instance ToSchema (Id a) where
    declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance ToSchema Metrics.Metrics where
    declareNamedSchema = undefined

instance ToSchema UserSet
instance ToSchema UserConnection
instance ToSchema Relation
instance ToParamSchema Relation where
instance ToSchema Message
instance ToSchema Data.Json.Util.UTCTimeMillis
instance ToSchema NewUser
instance ToSchema NewUserOrigin

instance ToSchema (Range from to typ) where
    declareNamedSchema = undefined

instance ToSchema User
instance ToSchema UserIdentity
instance ToSchema Email
instance ToSchema Phone
instance ToSchema Name
instance ToSchema UserSSOId
instance ToSchema Pict
instance ToSchema Asset
instance ToSchema AssetSize
instance ToSchema ColourId
instance ToSchema Locale
instance ToSchema Language
instance ToSchema Country
instance ToSchema SelfProfile
instance ToSchema ActivationCode
instance ToSchema CookieLabel
instance ToSchema PlainTextPassword
instance ToSchema ManagedBy
instance ToSchema InvitationCode
instance ToSchema (NewTeam ())
instance ToSchema NewTeamUser
instance ToSchema ServiceRef
instance ToSchema BindingNewTeamUser
instance ToSchema BindingNewTeam
instance ToSchema Alpha
instance ToSchema (AsciiText Base64Url)
instance ToSchema CountryCode

instance ToSchema Value where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema Handle

deriving instance Generic ISO639_1
instance ToSchema ISO639_1

instance ToSchema Brig.Types.User.EmailUpdate
instance ToSchema ConnectionsStatusRequest
instance ToSchema ConnectionStatus
instance ToSchema UserAccount
instance ToSchema AccountStatus

instance ToParamSchema (List a) where
    toParamSchema _ = toParamSchema (Proxy @Text)
-- alternative:
-- instance (Generic a, ToParamSchema a, ToParamSchema a) => ToParamSchema (List a)
-- deriving instance Generic a => Generic (List a)

instance ToParamSchema Email where
    toParamSchema _ = toParamSchema (Proxy @Text)

instance ToParamSchema Phone
instance ToParamSchema Handle
instance ToParamSchema AccountStatus
instance ToParamSchema PhonePrefix

instance ToParamSchema URI where
    toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema AccountStatusUpdate
instance ToSchema AccountStatusObject
instance ToSchema ActivationCodeObject
instance ToSchema UserIds
instance ToSchema ActivationKey
instance ToSchema ExcludedPrefix
instance ToSchema PhonePrefix
instance ToSchema UserClients
instance ToSchema ClientId
instance ToSchema ManagedByUpdate
instance ToSchema RichInfoUpdate
instance ToSchema RichInfo
instance ToSchema RichField

-- FUTUREWORK: once we have https://github.com/haskell-servant/servant-swagger/pull/107 pulled
-- in, we can send empty bodies again and this would still work.
instance ToSchema NoContent

instance ToParamSchema ConnId where
    toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema Swagger1.ApiDecl where
    declareNamedSchema _ = declareNamedSchema (Proxy @Value)
