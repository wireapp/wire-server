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
  | Backend HttpsUrl
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
domainRedirectTag (Backend _) = BackendTag
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
      BackendTag -> tag _Backend backendUrlSchema
      NoRegistrationTag -> tag _NoRegistration (pure ())
      PreAuthorizedTag -> tag _PreAuthorized (pure ())

samlIdPIdObjectSchema :: ObjectSchema SwaggerDoc SAML.IdPId
samlIdPIdObjectSchema = SAML.IdPId <$> SAML.fromIdPId .= field "sso_code" uuidSchema

backendUrlSchema :: ObjectSchema SwaggerDoc HttpsUrl
backendUrlSchema = field "backend_url" schema

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
  deriving stock (Ord, Eq, Show, Generic)
  deriving newtype (FromHttpApiData, ToByteString, Arbitrary)
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
          Backend _ -> dr.teamInvite == NotAllowed
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

----------------------------------------------------------------------

data DomainRegistration' = DomainRegistration'
  { settings :: Maybe DomainRegistrationSettings',
    dnsVerificationToken :: Maybe DnsVerificationToken,
    authTokenHash :: Maybe Token
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform DomainRegistration')

data DomainRegistrationSettings'
  = Locked'
  | PreAuthorized'
  | NoRegistration'
  | DomainForBackend HttpsUrl
  | DomainForLocalTeam TeamId (Maybe SAML.IdPId)
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform DomainRegistrationSettings')

newToOld :: Domain -> DomainRegistration' -> DomainRegistration
newToOld domain DomainRegistration' {..} = DomainRegistration {..}
  where
    domainRedirect = case settings of
      Nothing -> None
      Just Locked' -> Locked
      Just PreAuthorized' -> PreAuthorized
      Just NoRegistration' -> NoRegistration
      Just (DomainForBackend url) -> Backend url
      Just (DomainForLocalTeam _tid Nothing) -> None
      Just (DomainForLocalTeam _tid (Just idpid)) -> SSO idpid

    teamInvite = case settings of
      Nothing -> Allowed
      Just Locked' -> Allowed
      Just PreAuthorized' -> Allowed
      Just NoRegistration' -> NotAllowed -- TODO: where does the spec cover this case?
      Just (DomainForBackend _) -> NotAllowed
      Just (DomainForLocalTeam tid _) -> Team tid

    authorizedTeam = case settings of
      Just (DomainForLocalTeam tid _) -> Just tid
      _ -> Nothing

oldToNew :: DomainRegistration -> Either String (Domain, DomainRegistration')
oldToNew DomainRegistration {..} = do
  settings :: Maybe DomainRegistrationSettings' <- do
    case (domainRedirect, teamInvite, authorizedTeam) of
      (None, Allowed, Nothing) -> Right Nothing
      (Locked, Allowed, Nothing) -> Right (Just Locked')
      (PreAuthorized, Allowed, Nothing) -> Right (Just PreAuthorized')
      (NoRegistration, NotAllowed, Nothing) -> Right (Just NoRegistration')
      (Backend url, NotAllowed, Nothing) -> Right (Just (DomainForBackend url))
      (None, Team tid, Just tid') | tid == tid' -> Right (Just (DomainForLocalTeam tid Nothing))
      (SSO idpid, Team tid, Just tid') | tid == tid' -> Right (Just (DomainForLocalTeam tid (Just idpid)))
      _ -> Left ("domainRedirect, teamInvite, authorizedTeam mismatch: " <> show (domainRedirect, teamInvite, authorizedTeam))

  Right (domain, DomainRegistration' {..})

----------------------------------------------------------------------

data DomainRegistration = DomainRegistration
  { domain :: Domain,
    authorizedTeam :: Maybe TeamId,
    domainRedirect :: DomainRedirect,
    teamInvite :: TeamInvite,
    dnsVerificationToken :: Maybe DnsVerificationToken,
    authTokenHash :: Maybe Token
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform DomainRegistration)

instance {-# OVERLAPPING #-} Default (Domain -> DomainRegistration) where
  def domain =
    DomainRegistration
      { domain,
        authorizedTeam = Nothing,
        domainRedirect = def,
        teamInvite = def,
        dnsVerificationToken = Nothing,
        authTokenHash = Nothing
      }

newtype Token = Token {unToken :: ByteString}
  deriving newtype (Eq, Ord, Show, Arbitrary)
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
