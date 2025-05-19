{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.API.EnterpriseLogin where

import Cassandra qualified as C
import Control.Arrow
import Control.Lens (makePrisms, (?~))
import Crypto.Hash qualified as Crypto
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteArray (convert)
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as BL
import Data.Default (Default, def)
import Data.Domain
import Data.Id
import Data.Json.Util (base64URLSchema)
import Data.Misc
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Ascii (AsciiBase64Url, AsciiText (toText))
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding qualified as Text
import Imports
import SAML2.WebSSO qualified as SAML
import SAML2.WebSSO.Test.Arbitrary ()
import Test.QuickCheck (suchThat)
import Web.HttpApiData
import Wire.API.Routes.Bearer
import Wire.Arbitrary

data DomainRedirect
  = None
  | Locked
  | SSO SAML.IdPId
  | Backend HttpsUrl (Maybe HttpsUrl)
  | NoRegistration
  | PreAuthorized
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform DomainRedirect

instance Default DomainRedirect where
  def = None

makePrisms ''DomainRedirect

data DomainRedirectTag
  = NoneTag
  | LockedTag
  | SSOTag
  | BackendTag
  | NoRegistrationTag
  | PreAuthorizedTag
  deriving (Show, Ord, Eq, Enum, Bounded)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema DomainRedirectTag

domainRedirectTag :: DomainRedirect -> DomainRedirectTag
domainRedirectTag None = NoneTag
domainRedirectTag Locked = LockedTag
domainRedirectTag (SSO _) = SSOTag
domainRedirectTag (Backend _ _) = BackendTag
domainRedirectTag NoRegistration = NoRegistrationTag
domainRedirectTag PreAuthorized = PreAuthorizedTag

instance ToSchema DomainRedirectTag where
  schema =
    enum @Text "DomainRedirect Tag" $
      mconcat
        [ element "none" NoneTag,
          element "locked" LockedTag,
          element "sso" SSOTag,
          element "backend" BackendTag,
          element "no-registration" NoRegistrationTag,
          element "pre-authorized" PreAuthorizedTag
        ]

domainRedirectTagSchema :: ObjectSchema SwaggerDoc DomainRedirectTag
domainRedirectTagSchema = field "domain_redirect" schema

domainRedirectSchemaV8 :: ObjectSchema SwaggerDoc DomainRedirect
domainRedirectSchemaV8 =
  snd
    <$> (domainRedirectTag &&& id)
      .= bind
        (fst .= domainRedirectTagSchema)
        (snd .= dispatch domainRedirectObjectSchema)
  where
    domainRedirectObjectSchema :: DomainRedirectTag -> ObjectSchema SwaggerDoc DomainRedirect
    domainRedirectObjectSchema = \case
      NoneTag -> tag _None (pure ())
      LockedTag -> tag _Locked (pure ())
      SSOTag -> tag _SSO samlIdPIdObjectSchema
      BackendTag -> tag (_Backend) backendConfigSchemaV8
      NoRegistrationTag -> tag _NoRegistration (pure ())
      PreAuthorizedTag -> tag _PreAuthorized (pure ())

backendConfigSchemaV8 :: ObjectSchema SwaggerDoc (HttpsUrl, Maybe HttpsUrl)
backendConfigSchemaV8 =
  (,)
    <$> fst .= field "backend_url" schema
    -- API versions <= V8 ignore the WebApp URL
    <*> const Nothing .= pure Nothing

domainRedirectSchema :: ObjectSchema SwaggerDoc DomainRedirect
domainRedirectSchema =
  snd
    <$> (domainRedirectTag &&& id)
      .= bind
        (fst .= domainRedirectTagSchema)
        (snd .= dispatch domainRedirectObjectSchema)
  where
    domainRedirectObjectSchema :: DomainRedirectTag -> ObjectSchema SwaggerDoc DomainRedirect
    domainRedirectObjectSchema = \case
      NoneTag -> tag _None (pure ())
      LockedTag -> tag _Locked (pure ())
      SSOTag -> tag _SSO samlIdPIdObjectSchema
      BackendTag -> tag (_Backend) backendConfigFieldSchema
      NoRegistrationTag -> tag _NoRegistration (pure ())
      PreAuthorizedTag -> tag _PreAuthorized (pure ())

    backendConfigFieldSchema :: ObjectSchema SwaggerDoc (HttpsUrl, Maybe HttpsUrl)
    backendConfigFieldSchema = field "backend" backendConfigObjectSchema

    backendConfigObjectSchema :: ValueSchema NamedSwaggerDoc (HttpsUrl, Maybe HttpsUrl)
    backendConfigObjectSchema =
      object "BackendConfig" $
        (,)
          <$> fst .= field "config_url" schema
          <*> snd .= maybe_ (optField "webapp_url" schema)

samlIdPIdObjectSchema :: ObjectSchema SwaggerDoc SAML.IdPId
samlIdPIdObjectSchema = SAML.IdPId <$> SAML.fromIdPId .= field "sso_code" uuidSchema

instance ToSchema DomainRedirect where
  schema = object "DomainRedirect " domainRedirectSchema

deriving via (Schema DomainRedirect) instance FromJSON DomainRedirect

deriving via (Schema DomainRedirect) instance ToJSON DomainRedirect

deriving via (Schema DomainRedirect) instance S.ToSchema DomainRedirect

data TeamInvite
  = Allowed
  | NotAllowed
  | Team TeamId
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform TeamInvite

instance Default TeamInvite where
  def = Allowed

makePrisms ''TeamInvite

data TeamInviteTag
  = AllowedTag
  | NotAllowedTag
  | TeamTag
  deriving (Show, Ord, Eq, Enum, Bounded)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema TeamInviteTag

instance ToSchema TeamInviteTag where
  schema =
    enum @Text "TeamInvite Tag" $
      mconcat
        [ element "allowed" AllowedTag,
          element "not-allowed" NotAllowedTag,
          element "team" TeamTag
        ]

teamInviteTagSchema :: ObjectSchema SwaggerDoc TeamInviteTag
teamInviteTagSchema = field "team_invite" schema

teamInviteObjectSchema :: ObjectSchema SwaggerDoc TeamInvite
teamInviteObjectSchema =
  snd
    <$> (toTagged &&& id)
      .= bind
        (fst .= teamInviteTagSchema)
        (snd .= dispatch teamInviteDataSchema)
  where
    toTagged :: TeamInvite -> TeamInviteTag
    toTagged Allowed = AllowedTag
    toTagged NotAllowed = NotAllowedTag
    toTagged (Team _) = TeamTag

    teamInviteDataSchema :: TeamInviteTag -> ObjectSchema SwaggerDoc TeamInvite
    teamInviteDataSchema = \case
      AllowedTag -> tag _Allowed (pure ())
      NotAllowedTag -> tag _NotAllowed (pure ())
      TeamTag -> tag _Team (field "team" schema)

instance ToSchema TeamInvite where
  schema = object "TeamInvite" teamInviteObjectSchema

deriving via (Schema TeamInvite) instance FromJSON TeamInvite

deriving via (Schema TeamInvite) instance ToJSON TeamInvite

deriving via (Schema TeamInvite) instance S.ToSchema TeamInvite

-- | The challenge to be presented in a TXT DNS record by the owner of the domain.
newtype DnsVerificationToken = DnsVerificationToken {unDnsVerificationToken :: AsciiBase64Url}
  deriving stock (Ord, Eq, Show)
  deriving newtype (FromHttpApiData, ToByteString)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema DnsVerificationToken

instance ToSchema DnsVerificationToken where
  schema = DnsVerificationToken <$> unDnsVerificationToken .= schema

data DomainRegistrationUpdate = DomainRegistrationUpdate
  { domainRedirect :: DomainRedirect,
    teamInvite :: TeamInvite
  }
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema DomainRegistrationUpdate

instance Arbitrary DomainRegistrationUpdate where
  arbitrary = do
    ( DomainRegistrationUpdate
        <$> arbitrary
        <*> arbitrary
      )
      `suchThat` validate
    where
      validate :: DomainRegistrationUpdate -> Bool
      validate dr =
        case dr.domainRedirect of
          Locked -> dr.teamInvite == Allowed
          Backend _ _ -> dr.teamInvite == NotAllowed
          _ -> True

instance ToSchema DomainRegistrationUpdate where
  schema =
    object "DomainRegistrationUpdate" $
      DomainRegistrationUpdate
        <$> (.domainRedirect) .= domainRedirectSchema
        <*> (.teamInvite) .= teamInviteObjectSchema

data DomainRegistrationResponse = DomainRegistrationResponse
  { domain :: Domain,
    authorizedTeam :: Maybe TeamId,
    domainRedirect :: DomainRedirect,
    teamInvite :: TeamInvite,
    dnsVerificationToken :: Maybe DnsVerificationToken
  }
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema DomainRegistrationResponse

mkDomainRegistrationResponse :: DomainRegistration -> DomainRegistrationResponse
mkDomainRegistrationResponse DomainRegistration {..} = DomainRegistrationResponse {..}

instance ToSchema DomainRegistrationResponse where
  schema =
    object "DomainRegistrationResponse" $
      DomainRegistrationResponse
        <$> (.domain) .= field "domain" schema
        <*> (.authorizedTeam) .= maybe_ (optField "authorized_team" schema)
        <*> (.domainRedirect) .= domainRedirectSchema
        <*> (.teamInvite) .= teamInviteObjectSchema
        <*> (.dnsVerificationToken) .= optField "dns_verification_token" (maybeWithDefault Aeson.Null schema)

data DomainRegistration = DomainRegistration
  { domain :: Domain,
    authorizedTeam :: Maybe TeamId,
    domainRedirect :: DomainRedirect,
    teamInvite :: TeamInvite,
    dnsVerificationToken :: Maybe DnsVerificationToken,
    authTokenHash :: Maybe Token
  }
  deriving stock (Eq, Show)

instance Arbitrary DomainRegistration where
  arbitrary = do
    dom :: Domain <- arbitrary
    upd :: DomainRegistrationUpdate <- arbitrary
    mbteam :: Maybe TeamId <- arbitrary
    pure
      DomainRegistration
        { domain = dom,
          authorizedTeam = mbteam,
          domainRedirect = upd.domainRedirect,
          teamInvite = upd.teamInvite,
          dnsVerificationToken = Nothing,
          authTokenHash = Nothing
        }

mkDomainRegistration :: Domain -> DomainRegistration
mkDomainRegistration domain =
  DomainRegistration
    { domain,
      authorizedTeam = Nothing,
      domainRedirect = def,
      teamInvite = def,
      dnsVerificationToken = Nothing,
      authTokenHash = Nothing
    }

newtype Token = Token {unToken :: ByteString}
  deriving newtype (Eq, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON, S.ToSchema) via (Schema Token)

instance ToSchema Token where
  schema = Token <$> unToken .= named "Token" base64URLSchema

hashToken :: Token -> Token
hashToken = Token . convert . Crypto.hash @ByteString @Crypto.SHA256 . unToken

instance S.ToParamSchema (Bearer Token) where
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString

instance FromHttpApiData Token where
  parseUrlPiece =
    mapLeft Text.pack
      . fmap Token
      . B64U.decodeUnpadded
      . Text.encodeUtf8

instance ToByteString Token where
  builder = builder . B64U.encodeUnpadded . unToken

--------------------------------------------------------------------------------
-- CQL instances

instance C.Cql DomainRedirectTag where
  ctype = C.Tagged C.IntColumn

  toCql NoneTag = C.CqlInt 1
  toCql LockedTag = C.CqlInt 2
  toCql SSOTag = C.CqlInt 3
  toCql BackendTag = C.CqlInt 4
  toCql NoRegistrationTag = C.CqlInt 5
  toCql PreAuthorizedTag = C.CqlInt 6

  fromCql (C.CqlInt i) = case i of
    1 -> pure NoneTag
    2 -> pure LockedTag
    3 -> pure SSOTag
    4 -> pure BackendTag
    5 -> pure NoRegistrationTag
    6 -> pure PreAuthorizedTag
    n -> Left $ "Unexpected DomainRedirectTag value: " ++ show n
  fromCql _ = Left "DomainRedirectTag value: int expected"

instance C.Cql TeamInviteTag where
  ctype = C.Tagged C.IntColumn

  toCql AllowedTag = C.CqlInt 1
  toCql NotAllowedTag = C.CqlInt 2
  toCql TeamTag = C.CqlInt 3

  fromCql (C.CqlInt i) = case i of
    1 -> pure AllowedTag
    2 -> pure NotAllowedTag
    3 -> pure TeamTag
    n -> Left $ "Unexpected TeamInviteTag value: " ++ show n
  fromCql _ = Left "TeamInviteTag value: int expected"

instance C.Cql DnsVerificationToken where
  ctype = C.Tagged C.AsciiColumn
  toCql = C.toCql . toText . unDnsVerificationToken
  fromCql (C.CqlAscii t) = DnsVerificationToken <$> Ascii.validate t
  fromCql _ = Left "DnsVerificationToken value: text expected"

instance C.Cql Token where
  ctype = C.Tagged C.BlobColumn

  toCql = C.CqlBlob . BL.fromStrict . unToken

  fromCql (C.CqlBlob b) = pure $ Token $ BL.toStrict b
  fromCql _ = Left "Token value: blob expected"
