{-# LANGUAGE RoleAnnotations #-}
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
import Brig.Types.Client
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Properties
import Brig.Types.Search as Search
import Brig.Types.User
import Brig.Types.User.Auth
import Brig.Types.User.Auth (CookieLabel)
import Control.Lens
import Data.Aeson as Aeson
import Data.Aeson                 (Value)
import Data.ByteString.Conversion as BSC
import Data.ByteString.Conversion (List(..))
import Data.Currency (Alpha)
import Data.Id
import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.LegalHold
import Data.Misc
import Data.Proxy
import Data.Range
import Data.Singletons.Bool (reflectBool)
import Data.String.Conversions (cs)
import Data.Text.Ascii
import Data.Text (Text)
import Data.Typeable (typeOf)
import Data.UUID (UUID)
import Galley.Types
import Galley.Types.Bot.Service
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import GHC.TypeLits
import Gundeck.Types.Notification
import Servant.API
import Servant.API.Description (FoldDescription, reflectDescription)
import Servant.API.Modifiers (FoldRequired)
import Servant hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam', URI)
import Servant.Swagger
import Servant.Swagger.Internal (addDefaultResponse400, addParam)
import Servant.Swagger.UI
import Servant.Swagger.UI.Core
import URI.ByteString (URI)

import qualified Data.Json.Util
import qualified Data.Metrics         as Metrics
import qualified Data.Metrics.Servant as Metrics
import qualified Data.Text            as Text
import qualified Servant


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

type Head = Verb 'HEAD 204 '[JSON]
type Get = Verb 'GET 200 '[JSON]
type Post = Verb 'POST 201 '[JSON]
type Put = Verb 'PUT 200 '[JSON]
type Delete = Verb 'DELETE 200 '[JSON]

type ReqBody = Servant.ReqBody '[JSON]

type QueryParamStrict = Servant.QueryParam' '[Required, Servant.Strict]
type QueryParamOptional = Servant.QueryParam' '[Optional, Servant.Strict]


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
    declareNamedSchema _ = declareNamedSchema (Proxy @())  -- TODO

instance ToSchema UserSet
instance ToSchema UserConnection
instance ToSchema Relation
instance ToParamSchema Relation where
instance ToSchema Message
instance ToSchema Data.Json.Util.UTCTimeMillis
instance ToSchema NewUser
instance ToSchema NewUserOrigin

instance ToSchema (Range from to typ) where
    declareNamedSchema _ = declareNamedSchema (Proxy @Int)  -- TODO

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

instance ToParamSchema URI.ByteString.URI where
    toParamSchema _ = toParamSchema (Proxy @Text)

instance ToParamSchema Servant.API.URI where
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


instance ToSchema (SwaggerSchemaUI' dir api) where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)

instance ToSchema (SwaggerUiHtml dir any) where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)

instance ToSchema Swagger where
  declareNamedSchema _ = declareNamedSchema (Proxy @NoContent)


instance ToSchema Perm
instance ToSchema Permissions
instance ToSchema PhoneUpdate
instance ToSchema Team
instance ToSchema TeamBinding
instance ToSchema TeamData
instance ToSchema TeamMember
instance ToSchema TeamStatus
instance ToSchema UserLegalHoldStatus
instance ToSchema PropertyValue
instance ToSchema (AsciiText Printable)
instance ToSchema PropertyKey


instance ToParamSchema typ => ToParamSchema (Range lower upper typ)

deriving instance Generic Search.Contact
instance ToSchema Search.Contact

deriving instance Generic (SearchResult Search.Contact)
instance ToSchema (SearchResult Search.Contact)


instance ToSchema QueuedNotification where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)  -- TODO

instance ToSchema Conversation where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)  -- TODO

instance ToSchema Client where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)  -- TODO

instance ToSchema CookieList where
  declareNamedSchema _ = declareNamedSchema (Proxy @Value)  -- TODO


data NoSwagger
  deriving (Generic)

instance HasSwagger (NoSwagger :> api) where
  toSwagger _ = mempty


data SwaggerDesc (notes :: Symbol) (val :: k)
type role SwaggerDesc phantom phantom

-- Copied and mutated from "Servant.Swagger.Internal".  To make this more composable, we'd
-- have to touch the package, I think.  (We should probably do that now, no?  write a function
-- that does what the instance does, plus takes an extra descr string, then writing the two
-- instances is trivial.  then also find a better name for SwaggerDesc.  WithDescription?)
instance ( KnownSymbol desc
         , KnownSymbol sym
         , ToParamSchema a
         , HasSwagger sub
         , SBoolI (FoldRequired mods)
         , KnownSymbol (FoldDescription mods)
         ) => HasSwagger (SwaggerDesc desc (QueryParam' mods sym a) :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      descText :: [Text]
        = [ cs $ symbolVal (Proxy :: Proxy desc)
          , cs $ reflectDescription (Proxy :: Proxy mods)
          ]
      transDesc ""   = Nothing
      transDesc desc = Just desc
      param = mempty
        & name .~ tname
        & description .~ (transDesc . Text.strip . Text.unlines $ descText)
        & required ?~ reflectBool (Proxy :: Proxy (FoldRequired mods))
        & schema .~ ParamOther sch
      sch = mempty
        & in_ .~ ParamQuery
        & paramSchema .~ toParamSchema (Proxy :: Proxy a)


instance HasServer api ctx => HasServer (NoSwagger :> api) ctx where
  type ServerT (NoSwagger :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance HasServer (something :> api) ctx => HasServer (SwaggerDesc (sym :: Symbol) something :> api) ctx where
  type ServerT (SwaggerDesc sym something :> api) m = ServerT (something :> api) m
  route _ = route (Proxy @(something :> api))
  hoistServerWithContext _ = hoistServerWithContext (Proxy @(something :> api))


instance Metrics.RoutesToPaths api => Metrics.RoutesToPaths (NoSwagger :> api) where
  getRoutes = mempty

instance Metrics.RoutesToPaths Raw where
  getRoutes = mempty
