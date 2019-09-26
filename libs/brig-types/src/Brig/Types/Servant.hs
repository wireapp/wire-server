{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}
-- FUTUREWORK: move the 'ToSchema' instances to their home modules (where the data types
-- live), and turn warning about orphans back on.

-- TODO: change some 'Inline' properites into 'Ref'.  (not sure i remember why i didn't do
-- that to begin with.  it may cause problems with the test function that crawles the API, but
-- then we just need to fix those problems.)

module Brig.Types.Servant where

import Imports

import qualified "swagger" Data.Swagger.Build.Api as Swagger1
import "swagger2" Data.Swagger hiding (Header(..))
import "swagger2" Data.Swagger.Declare
import "swagger2" Data.Swagger.Internal.Schema
import "swagger2" Data.Swagger.Internal.TypeShape
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.

import GHC.Generics (Rep)
import Brig.Types.Activation
import Brig.Types.Client
import Brig.Types.Client.Prekey (PrekeyId, Prekey, LastPrekey)
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Properties
import Brig.Types.Provider hiding (PasswordChange, CompletePasswordReset)
import Brig.Types.Search as Search
import Brig.Types.Team.Invitation
import Brig.Types.Team.LegalHold
import Brig.Types.User
import Brig.Types.User.Auth
import Brig.Types.User.Auth (CookieLabel)
import Control.Lens
import Data.Aeson as Aeson hiding (fieldLabelModifier, constructorTagModifier)
import Data.Aeson (toJSON)
import Data.Aeson (Value(..))
import Data.ByteString.Conversion as BSC
import Data.ByteString.Conversion (fromByteString)
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
import Data.Text as Text (unlines)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import Data.Typeable (typeOf)
import Data.UUID (fromText)
import Data.UUID (UUID)
import Galley.Types
import Galley.Types.Bot.Service
import Galley.Types.Teams
import Galley.Types.Teams.Internal ()
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SSO
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
import URI.ByteString.QQ (uri)

import qualified Data.Code
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Json.Util
import qualified Data.Metrics         as Metrics
import qualified Data.Metrics.Servant as Metrics
import qualified Data.Text            as Text
import qualified Servant
import qualified URI.ByteString


----------------------------------------------------------------------
-- * more stuff we need to move to other places

-- | newtype for 'Text' that has a prettier 'Arbitrary' instance (just for testing).
newtype SchematicText = SchematicText Text
  deriving (Eq, Show, Generic)

instance ToSchema SchematicText where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToParamSchema SchematicText where
  toParamSchema _ = toParamSchema (Proxy @Text)

newtype AccountStatusObject = AccountStatusObject { _fromAccountStatusObject :: AccountStatus }
    deriving (Eq, Show, Generic)

instance FromJSON AccountStatusObject where
    parseJSON = withObject "account-status object" $
        \o -> AccountStatusObject <$> o .: "status"

instance ToJSON AccountStatusObject where
    toJSON (AccountStatusObject status) = object [ "status" Aeson..= status ]

data ActivationCodeObject = ActivationCodeObject
    { _acoKey  :: ActivationKey
    , _acoCode :: ActivationCode
    }
    deriving (Eq, Show, Generic)

instance FromJSON ActivationCodeObject where
    parseJSON = withObject "activation code object" $
        \o -> ActivationCodeObject <$> o .: "key" <*> o .: "code"

instance ToJSON ActivationCodeObject where
    toJSON (ActivationCodeObject key code) = object [ "key" Aeson..= key, "code" Aeson..= code ]

instance ToSchema ActivationCodeObject where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "_aco"

instance ToSchema ActivationCode where
  declareNamedSchema _ = declareNamedSchema (Proxy @SchematicText)

instance ToSchema ActivationKey where
  declareNamedSchema _ = declareNamedSchema (Proxy @SchematicText)


----------------------------------------------------------------------
-- * generic servant helpers

type Head = Verb 'HEAD 204 '[JSON] NoContent
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

-- | errors should be caught by the test suite, so we don't need to be subtle.
unsafeStripPrefix :: HasCallStack => String -> String -> String
unsafeStripPrefix pref txt
  = fromMaybe (error $ "internal error: " <> show (pref, txt))
  $ stripPrefix pref txt

withFieldLabelMod
  :: forall a (proxy :: * -> *).
     ( Generic a
     , GToSchema (Rep a)
     , TypeHasSimpleShape a "genericDeclareNamedSchemaUnrestricted"
     )
  => (String -> String)
  -> proxy a
  -> Declare (Definitions Schema) NamedSchema
withFieldLabelMod fun = genericDeclareNamedSchema opts
  where opts = defaultSchemaOptions { fieldLabelModifier = fun }

withConstructorTagMod
  :: forall a (proxy :: * -> *).
     ( Generic a
     , GToSchema (Rep a)
     , TypeHasSimpleShape a "genericDeclareNamedSchemaUnrestricted"
     )
  => (String -> String)
  -> proxy a
  -> Declare (Definitions Schema) NamedSchema
withConstructorTagMod fun = genericDeclareNamedSchema opts
  where opts = defaultSchemaOptions { constructorTagModifier = fun }

mkEnumSchema :: [Text] -> Schema
mkEnumSchema vals = mempty & enum_ .~ Just (String <$> vals)


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

instance ToSchema UserSet where
  declareNamedSchema = withFieldLabelMod $ \"usUsrs" -> "users"

instance ToSchema UserConnection
instance ToSchema Relation
instance ToParamSchema Relation where
instance ToSchema Message
instance ToSchema Data.Json.Util.UTCTimeMillis
instance ToSchema (NewUser "visible")
instance ToSchema NewUserOrigin

instance ToSchema (Range from to typ) where
    declareNamedSchema _ = declareNamedSchema (Proxy @typ)  -- TODO: at least add a
                                                            -- description showing the range.
                                                            -- or perhaps swagger2 can do
                                                            -- this?

instance ToSchema User
instance ToSchema UserIdentity
instance ToSchema Email
instance ToSchema Phone
instance ToSchema Name

instance ToSchema UserSSOId where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "userSSOId"

instance ToSchema Pict

instance ToSchema Asset where
  declareNamedSchema _ = pure $ NamedSchema (Just "Asset") $ mempty
        & properties .~ properties_
        & example .~ example_
        & required .~ ["type", "key"]
        & type_ .~ SwaggerObject
      where
        properties_ :: HashMap.InsOrdHashMap Text (Referenced Schema)
        properties_ = HashMap.fromList
          [ ("type", Inline (mkEnumSchema ["image"]))
          , ("key", Inline (toSchema (Proxy @SchematicText)))
          , ("size", Inline (toSchema (Proxy @AssetSize)))
          ]

        example_ :: Maybe Value
        example_ = Just "{\"size\":\"complete\",\"key\":\"879\",\"type\":\"image\"}"

instance ToSchema AssetSize where
  declareNamedSchema = withConstructorTagMod $ camelToUnderscore . unsafeStripPrefix "Asset"

instance ToSchema ColourId where
  declareNamedSchema _ = declareNamedSchema (Proxy @Int)

instance ToSchema Locale
instance ToSchema Language
instance ToSchema Country
instance ToSchema SelfProfile

instance ToSchema CookieLabel

instance ToSchema (PlainTextPassword "visible") where
  declareNamedSchema _ = declareNamedSchema (Proxy @SchematicText)

instance ToSchema ManagedBy
instance ToSchema InvitationCode

instance ToSchema ServiceRef where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "_serviceRef"

instance ToSchema (NewTeam ()) where
  declareNamedSchema _ = pure $ NamedSchema (Just "NewTeam ()") $ mempty
    & properties .~ properties_
    & example .~ example_
    & required .~ ["name", "icon"]
    & type_ .~ SwaggerObject
    where
      properties_ :: HashMap.InsOrdHashMap Text (Referenced Schema)
      properties_ = HashMap.fromList
        [ ( "name",     Inline (toSchema (Proxy @(Range 1 256 Text))))
        , ( "icon",     Inline (toSchema (Proxy @(Range 1 256 Text))))
        , ( "icon_key", Inline (toSchema (Proxy @(Maybe (Range 1 256 Text)))))
        ]

      example_ :: Maybe Value
      example_ = toJSON <$> (newNewTeam @() <$> checked "name" <*> checked "icon")

instance ToSchema NewTeamUser

instance ToSchema BindingNewTeam where
  declareNamedSchema _ = declareNamedSchema (Proxy @(NewTeam ()))

instance ToSchema BindingNewTeamUser where
  declareNamedSchema _ = declareNamedSchema (Proxy @(NewTeam ()))
    <&> schema . properties %~ (<> properties_)
    where
      properties_ :: HashMap.InsOrdHashMap Text (Referenced Schema)
      properties_ = HashMap.fromList
        [ ("currency", Inline (toSchema (Proxy @Alpha)))
        ]

instance ToSchema Alpha
instance ToSchema CountryCode

instance ToSchema Value where
    declareNamedSchema _ = pure $ NamedSchema (Just "Value") mempty  -- TODO: the test suite
                                                                     -- error is almost
                                                                     -- helpful, but not
                                                                     -- quite.  generate the
                                                                     -- swagger json and
                                                                     -- figure out what it
                                                                     -- should look like.
                                                                     -- then make it look like
                                                                     -- that.

instance ToSchema Handle

deriving instance Generic ISO639_1
instance ToSchema ISO639_1

instance ToSchema Brig.Types.User.EmailUpdate
instance ToSchema ConnectionsStatusRequest
instance ToSchema ConnectionStatus
instance ToSchema UserAccount

instance ToSchema AccountStatus where
  declareNamedSchema = withConstructorTagMod camelToUnderscore

instance ToParamSchema (List a) where
    toParamSchema _ = toParamSchema (Proxy @SchematicText)
-- alternative:
-- instance (Generic a, ToParamSchema a, ToParamSchema a) => ToParamSchema (List a)
-- deriving instance Generic a => Generic (List a)

instance ToParamSchema Email where
    toParamSchema _ = toParamSchema (Proxy @SchematicText)

instance ToParamSchema Phone
instance ToParamSchema Handle
instance ToParamSchema AccountStatus
instance ToParamSchema PhonePrefix

instance ToParamSchema URI.ByteString.URI where
    toParamSchema _ = toParamSchema (Proxy @SchematicText)

instance ToSchema URI.ByteString.URI where
    declareNamedSchema _ = declareNamedSchema (Proxy @SchematicText)

instance ToJSON URI.ByteString.URI where
    toJSON = String . cs . URI.ByteString.serializeURIRef'

instance FromJSON URI.ByteString.URI where
    parseJSON = withText "URI.ByteString.URI"
        $ either (fail . show) pure
        . URI.ByteString.parseURI URI.ByteString.laxURIParserOptions
        . cs

instance ToParamSchema Servant.API.URI where
    toParamSchema _ = toParamSchema (Proxy @SchematicText)

instance ToSchema HttpsUrl where
    declareNamedSchema _ = declareNamedSchema (Proxy @URI.ByteString.URI)


instance ToSchema AccountStatusUpdate where
  declareNamedSchema = withFieldLabelMod $ \"suStatus" -> "status"

instance ToSchema AccountStatusObject where
  declareNamedSchema = withFieldLabelMod $ \"_fromAccountStatusObject" -> "status"

instance ToSchema UserIds where
  declareNamedSchema = withFieldLabelMod $ \"cUsers" -> "ids"

instance ToSchema ExcludedPrefix
instance ToSchema PhonePrefix
instance ToSchema UserClients
instance ToSchema ManagedByUpdate

instance ToSchema RichInfoUpdate where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "riu"

instance ToSchema RichInfoVersion where
  declareNamedSchema _ = declareNamedSchema (Proxy @Int)

instance ToSchema RichInfo where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "richInfo"

instance ToSchema RichField where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "richField"

-- FUTUREWORK: once we have https://github.com/haskell-servant/servant-swagger/pull/107 pulled
-- in, we can send empty bodies again and this would still work.
instance ToSchema NoContent

instance ToParamSchema ConnId where
    toParamSchema _ = toParamSchema (Proxy @SchematicText)

-- deprecated (where is this used?)
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

instance ToSchema TeamBinding where
  declareNamedSchema _ = declareNamedSchema (Proxy @Bool)

instance ToSchema TeamData
instance ToSchema TeamMember

instance ToSchema TeamStatus where
  declareNamedSchema = withConstructorTagMod camelToUnderscore

instance ToSchema PropertyValue
instance ToSchema PropertyKey

instance ToSchema (AsciiText r) where
  declareNamedSchema _ = declareNamedSchema (Proxy @SchematicText)

instance ToParamSchema typ => ToParamSchema (Range lower upper typ)

instance ToSchema Search.Contact where
  declareNamedSchema = withFieldLabelMod $ \case
    "concatUserId"   -> "id"
    "contactName"    -> "name"
    "contactColorId" -> "accent_id"
    "contactHandle"  -> "handle"

instance ToSchema (SearchResult Search.Contact) where
  declareNamedSchema = withFieldLabelMod $ \case
    "searchFound"    -> "found"
    "searchReturned" -> "returned"
    "searchTook"     -> "took"
    "searchResults"  -> "documents"

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


----------------------------------------------------------------------
-- more orphans

instance ToSchema Milliseconds where
  declareNamedSchema _ = declareNamedSchema (Proxy @Word64)

instance ToSchema (ApproveLegalHoldForUserRequest "visible") where
  declareNamedSchema = withFieldLabelMod $ \"alhfuPassword" -> "password"

instance ToSchema CheckHandles
instance ToSchema PasswordResetIdentity
instance ToSchema (CompletePasswordReset "visible")
instance ToSchema PasswordResetKey
instance ToSchema PasswordResetCode
instance ToSchema (DeleteUser "visible")

instance ToSchema Data.Code.Timeout where
  declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @SchematicText)
    where
      tweak = fmap $ schema . description ?~ descr
      descr = "A string containing a 'NominalDiffTime' value (in integer seconds)."


instance ToSchema DeletionCodeTimeout
instance ToSchema (DisableLegalHoldForUserRequest "visible")
instance ToSchema EmailRemove
instance ToSchema FeatureFlags
instance ToSchema FeatureLegalHold
instance ToSchema FeatureSSO
instance ToSchema HandleUpdate
instance ToSchema Invitation
instance ToSchema Role

instance ToSchema InvitationList where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "il"

instance ToSchema InvitationRequest where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "ir"

instance ToSchema LegalHoldClientRequest

instance ToSchema ServiceKeyType where
  declareNamedSchema = withConstructorTagMod $ \case "RsaServiceKey" -> "rsa"

instance ToSchema ServiceKey where
  declareNamedSchema = withFieldLabelMod $ \case
    "serviceKeyType" -> "type"
    "serviceKeySize" -> "size"
    "serviceKeyPEM"  -> "pem"

instance ToSchema LegalHoldService where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "legalHoldService"

instance ToSchema LegalHoldServiceConfirm
instance ToSchema LocaleUpdate

instance ToSchema NewPasswordReset
instance ToSchema (PasswordChange "visible")
instance ToSchema PhoneRemove
instance ToSchema (ReAuthUser "visible")
instance ToSchema (RemoveLegalHoldSettingsRequest "visible")

instance ToSchema SSOStatus where
  declareNamedSchema = withConstructorTagMod $ camelToUnderscore . unsafeStripPrefix "SSO"

instance ToSchema SSOTeamConfig

-- this is reasonably correct, but the ToJSON instance of PlainTextPassword hides the
-- password, which will break roundtrip tests as well as client functions.
instance ToSchema (TeamMemberDeleteData "visible") where
  declareNamedSchema _ = pure $ NamedSchema (Just "TeamMemberDeleteData") $ mempty
        & properties .~ properties_
        & example .~ example_
        & required .~ ["password"]
        & type_ .~ SwaggerObject
      where
        properties_ :: HashMap.InsOrdHashMap Text (Referenced Schema)
        properties_ = HashMap.fromList
          [ ("password", Inline (toSchema (Proxy @(Maybe Text))))
          ]

        example_ :: Maybe Value
        example_ = Just "{\"password\":null}"  -- or "{\"password\":\"wef\"}"
                                               -- FUTUREWORK: can there be more than one example?

instance ToSchema UpdateServiceWhitelist where
  declareNamedSchema = withFieldLabelMod $ \case
    "updateServiceWhitelistProvider" -> "provider"
    "updateServiceWhitelistService" -> "id"
    "updateServiceWhitelistStatus" -> "whitelisted"

instance ToSchema UserHandleInfo
instance ToSchema UserProfile

instance ToSchema UserUpdate where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "uup"

instance ToSchema Data.Code.Key where
  declareNamedSchema _ = declareNamedSchema (Proxy @SchematicText)

instance ToSchema Data.Code.Value where
  declareNamedSchema _ = declareNamedSchema (Proxy @SchematicText)

instance ToSchema VerifyDeleteUser where
  declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "verifyDeleteUser"

instance ToSchema ServiceKeyPEM where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @SchematicText)
      where
        tweak = fmap $ schema . example ?~ pem
        pem = String . Text.unlines $
            [ "-----BEGIN PUBLIC KEY-----"
            , "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0"
            , "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH"
            , "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV"
            , "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS"
            , "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8"
            , "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la"
            , "nQIDAQAB"
            , "-----END PUBLIC KEY-----"
            ]

instance ToSchema (Fingerprint Rsa) where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @SchematicText)
      where
        tweak = fmap $ schema . example ?~ fpr
        fpr = "ioy3GeIjgQRsobf2EKGO3O8mq/FofFxHRqy0T4ERIZ8="

instance ToSchema ServiceToken where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @SchematicText)
      where
        tweak = fmap $ schema . example ?~ tok
        tok = "sometoken"

instance ToSchema NewLegalHoldService where
  declareNamedSchema = withFieldLabelMod $ \case
    "newLegalHoldServiceKey"   -> "public_key"
    "newLegalHoldServiceUrl"   -> "base_url"
    "newLegalHoldServiceToken" -> "auth_token"

instance ToSchema ViewLegalHoldService where
    declareNamedSchema _ = pure $ NamedSchema (Just "ViewLegalHoldService") $ mempty
        & properties .~ properties_
        & example .~ example_
        & required .~ ["status"]
        & minProperties .~ Just 1
        & maxProperties .~ Just 2
        & type_ .~ SwaggerObject
      where
        properties_ :: HashMap.InsOrdHashMap Text (Referenced Schema)
        properties_ = HashMap.fromList
          [ ("status", Inline (toSchema (Proxy @MockViewLegalHoldServiceStatus)))
          , ("settings", Inline (toSchema (Proxy @ViewLegalHoldServiceInfo)))
          ]

        example_ :: Maybe Value
        example_ = Just . toJSON
                 $ ViewLegalHoldService (ViewLegalHoldServiceInfo (Id tid) lhuri fpr tok key)
          where
            tok = ServiceToken "sometoken"
            Just key = fromByteString . encodeUtf8 $ Text.unlines $
                [ "-----BEGIN PUBLIC KEY-----"
                , "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0"
                , "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH"
                , "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV"
                , "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS"
                , "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8"
                , "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la"
                , "nQIDAQAB"
                , "-----END PUBLIC KEY-----"
                ]
            Just tid = fromText "7fff70c6-7b9c-11e9-9fbd-f3cc32e6bbec"
            Right lhuri = mkHttpsUrl [uri|https://example.com/|]
            fpr = Fingerprint "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"

-- | this type is only introduce locally here to generate the schema for 'ViewLegalHoldService'.
data MockViewLegalHoldServiceStatus = Configured | NotConfigured | Disabled
  deriving (Eq, Show, Generic)

instance ToSchema MockViewLegalHoldServiceStatus where
    declareNamedSchema = withConstructorTagMod camelToUnderscore

instance ToSchema ViewLegalHoldServiceInfo where
    {-

    -- FUTUREWORK: The generic instance uses a reference to the UUID type in TeamId.  This
    -- leads to perfectly valid swagger output, but 'validateEveryToJSON' chokes on it
    -- (unknown schema "UUID").  In order to be able to run those tests, we construct the
    -- 'ToSchema' instance manually.
    -- See also: https://github.com/haskell-servant/servant-swagger/pull/104

    declareNamedSchema = withFieldLabelMod $ \case
        "viewLegalHoldServiceFingerprint" -> "fingerprint"
        "viewLegalHoldServiceUrl"         -> "base_url"
        "viewLegalHoldServiceTeam"        -> "team_id"
        "viewLegalHoldServiceAuthToken"   -> "auth_token"
        "viewLegalHoldServiceKey"         -> "public_key"

    -}
    declareNamedSchema _ = pure $ NamedSchema (Just "ViewLegalHoldServiceInfo") $ mempty
        & properties .~ properties_
        & example .~ example_
        & required .~ ["team_id", "base_url", "fingerprint", "auth_token", "public_key"]
        & type_ .~ SwaggerObject
      where
        properties_ :: HashMap.InsOrdHashMap Text (Referenced Schema)
        properties_ = HashMap.fromList
          [ ("team_id", Inline (toSchema (Proxy @UUID)))
          , ("base_url", Inline (toSchema (Proxy @HttpsUrl)))
          , ("fingerprint", Inline (toSchema (Proxy @(Fingerprint Rsa))))
          , ("auth_token", Inline (toSchema (Proxy @(ServiceToken))))
          , ("public_key", Inline (toSchema (Proxy @(ServiceKeyPEM))))
          ]

        example_ :: Maybe Value
        example_ = Just . toJSON
                 $ ViewLegalHoldServiceInfo (Id tid) lhuri fpr tok key
          where
            tok = ServiceToken "sometoken"
            Just key = fromByteString . encodeUtf8 $ Text.unlines $
                [ "-----BEGIN PUBLIC KEY-----"
                , "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0"
                , "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH"
                , "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV"
                , "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS"
                , "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8"
                , "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la"
                , "nQIDAQAB"
                , "-----END PUBLIC KEY-----"
                ]
            Just tid = fromText "7fff70c6-7b9c-11e9-9fbd-f3cc32e6bbec"
            Right lhuri = mkHttpsUrl [uri|https://example.com/|]
            fpr = Fingerprint "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"

instance ToSchema LegalHoldTeamConfig where
    declareNamedSchema = withFieldLabelMod $ \case "legalHoldTeamConfigStatus" -> "status"

instance ToSchema LegalHoldStatus where
    declareNamedSchema = tweak . withConstructorTagMod ctmod
      where
        ctmod = \case
          "LegalHoldDisabled" -> "disabled"
          "LegalHoldEnabled"  -> "enabled"

        tweak = fmap $ schema . description ?~ descr
          where
            descr = "determines whether admins of a team " <>
                    "are allowed to enable LH for their users."

instance ToSchema RequestNewLegalHoldClient where
    declareNamedSchema = withFieldLabelMod camelToUnderscore

instance ToSchema NewLegalHoldClient where
  declareNamedSchema = withFieldLabelMod $ \case
    "newLegalHoldClientPrekeys"     -> "prekeys"
    "newLegalHoldClientLastKey"     -> "last_prekey"

instance ToSchema UserLegalHoldStatusResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "UserLegalHoldStatusResponse") $ mempty
        & properties .~ properties_
        & required .~ ["status"]
        & minProperties .~ Just 1
        & maxProperties .~ Just 3
        & type_ .~ SwaggerObject
      where
        properties_ :: HashMap.InsOrdHashMap Text (Referenced Schema)
        properties_ = HashMap.fromList
          [ ("status", Inline (toSchema (Proxy @UserLegalHoldStatus)))
          , ("last_prekey", Inline (toSchema (Proxy @LastPrekey)))
          , ("client", Inline (toSchema (Proxy @(IdObject ClientId))))
          ]

instance ToSchema a => ToSchema (IdObject a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "IdObject a") $ mempty
        & properties .~ properties_
        & required .~ ["id"]
        & type_ .~ SwaggerObject
      where
        properties_ :: HashMap.InsOrdHashMap Text (Referenced Schema)
        properties_ = HashMap.fromList
          [ ("id", Inline (toSchema (Proxy @a)))
          ]

instance ToSchema UserLegalHoldStatus where
    declareNamedSchema = tweak . withConstructorTagMod ctmod
      where
        ctmod = \case
          "UserLegalHoldEnabled"  -> "enabled"
          "UserLegalHoldPending"  -> "pending"
          "UserLegalHoldDisabled" -> "disabled"

        tweak = fmap $ schema . description ?~ descr
          where
            descr = "states whether a user is under legal hold, " <>
                    "or whether legal hold is pending approval."

instance ToSchema ClientId where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @SchematicText)
      where
        tweak = fmap $ schema . description ?~ descr
          where
            descr = "A Client Id"

instance ToSchema PrekeyId where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Int)
      where
        tweak = fmap $ schema . description ?~ descr
          where
            descr = "in the range [0..65535]."
              -- FUTUREWORK: can this be also expressed in swagger, not just in the description?

instance ToSchema Prekey where
    declareNamedSchema = withFieldLabelMod $ camelToUnderscore . unsafeStripPrefix "prekey"

instance ToSchema LastPrekey where
    declareNamedSchema _ = declareNamedSchema (Proxy @Prekey)
