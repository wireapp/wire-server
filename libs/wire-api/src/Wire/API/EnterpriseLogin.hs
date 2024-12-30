{-# LANGUAGE TemplateHaskell #-}

module Wire.API.EnterpriseLogin where

import Cassandra qualified as C
import Control.Arrow
import Control.Lens (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Domain
import Data.Id
import Data.Misc
import Data.OpenApi qualified as OpenApi
import Data.Schema
import Data.Text.Ascii (Ascii, AsciiText (toText))
import Data.Text.Ascii qualified as Ascii
import Imports
import SAML2.WebSSO qualified as SAML

data DomainRedirect
  = None
  | Locked
  | SSO SAML.IdPId
  | Backend HttpsUrl
  | NoRegistration
  | PreAuthorized
  deriving stock (Eq, Show)

makePrisms ''DomainRedirect

data DomainRedirectTag
  = NoneTag
  | LockedTag
  | SSOTag
  | BackendTag
  | NoRegistrationTag
  | PreAuthorizedTag
  deriving (Show, Ord, Eq, Enum, Bounded)
  deriving (ToJSON, FromJSON, OpenApi.ToSchema) via Schema DomainRedirectTag

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

domainRedirectSchema :: ObjectSchema SwaggerDoc DomainRedirect
domainRedirectSchema =
  snd
    <$> (toTagged &&& id)
      .= bind
        (fst .= domainRedirectTagSchema)
        (snd .= dispatch domainRedirectObjectSchema)
  where
    toTagged :: DomainRedirect -> DomainRedirectTag
    toTagged None = NoneTag
    toTagged Locked = LockedTag
    toTagged (SSO _) = SSOTag
    toTagged (Backend _) = BackendTag
    toTagged NoRegistration = NoRegistrationTag
    toTagged PreAuthorized = PreAuthorizedTag

    domainRedirectObjectSchema :: DomainRedirectTag -> ObjectSchema SwaggerDoc DomainRedirect
    domainRedirectObjectSchema = \case
      NoneTag -> tag _None (pure ())
      LockedTag -> tag _Locked (pure ())
      SSOTag -> tag _SSO samlIdPIdObjectSchema
      BackendTag -> tag _Backend backendUrlSchema
      NoRegistrationTag -> tag _NoRegistration (pure ())
      PreAuthorizedTag -> tag _PreAuthorized (pure ())

samlIdPIdObjectSchema :: ObjectSchema SwaggerDoc SAML.IdPId
samlIdPIdObjectSchema = SAML.IdPId <$> SAML.fromIdPId .= field "sso_idp_id" uuidSchema

backendUrlSchema :: ObjectSchema SwaggerDoc HttpsUrl
backendUrlSchema = field "backend_url" schema

instance ToSchema DomainRedirect where
  schema = object "DomainRedirect " domainRedirectSchema

deriving via (Schema DomainRedirect) instance FromJSON DomainRedirect

deriving via (Schema DomainRedirect) instance ToJSON DomainRedirect

deriving via (Schema DomainRedirect) instance OpenApi.ToSchema DomainRedirect

data TeamInvite
  = Allowed
  | NotAllowed
  | Team TeamId
  deriving stock (Eq, Show)

makePrisms ''TeamInvite

data TeamInviteTag
  = AllowedTag
  | NotAllowedTag
  | TeamTag
  deriving (Show, Ord, Eq, Enum, Bounded)
  deriving (ToJSON, FromJSON, OpenApi.ToSchema) via Schema TeamInviteTag

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

deriving via (Schema TeamInvite) instance OpenApi.ToSchema TeamInvite

newtype DnsVerificationToken = DnsVerificationToken {unDnsVerificationToken :: Ascii}
  deriving stock (Ord, Eq, Show)
  deriving (ToJSON, FromJSON, OpenApi.ToSchema) via Schema DnsVerificationToken

instance ToSchema DnsVerificationToken where
  schema = DnsVerificationToken <$> unDnsVerificationToken .= schema

data DomainRegistrationUpdate = DomainRegistrationUpdate
  { domainRedirect :: DomainRedirect,
    teamInvite :: TeamInvite
  }
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, OpenApi.ToSchema) via Schema DomainRegistrationUpdate

instance ToSchema DomainRegistrationUpdate where
  schema =
    object "DomainRegistrationUpdate" $
      DomainRegistrationUpdate
        <$> (.domainRedirect) .= domainRedirectSchema
        <*> (.teamInvite) .= teamInviteObjectSchema

data DomainRegistration = DomainRegistration
  { domain :: Domain,
    domainRedirect :: DomainRedirect,
    teamInvite :: TeamInvite,
    dnsVerificationToken :: Maybe DnsVerificationToken
  }
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, OpenApi.ToSchema) via Schema DomainRegistration

instance ToSchema DomainRegistration where
  schema =
    object "DomainRegistration" $
      DomainRegistration
        <$> (.domain) .= field "domain" schema
        <*> (.domainRedirect) .= domainRedirectSchema
        <*> (.teamInvite) .= teamInviteObjectSchema
        <*> (.dnsVerificationToken) .= optField "dns_verification_token" (maybeWithDefault Aeson.Null schema)

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
