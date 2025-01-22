{-# LANGUAGE TemplateHaskell #-}

module Wire.API.Routes.Public.Brig.DomainVerification where

import Control.Arrow
import Control.Lens (makePrisms)
import Data.Aeson.Types qualified as A
import Data.Domain
import Data.Id
import Data.Misc
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import SAML2.WebSSO qualified as SAML
import Servant
import Wire.API.EnterpriseLogin
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.Routes.Bearer
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public (ZLocalUser)
import Wire.API.User.Identity (EmailAddress)

data DomainRedirectConfig
  = DomainRedirectConfigRemove
  | DomainRedirectConfigBackend HttpsUrl
  | DomainRedirectConfigNoRegistration
  deriving stock (Eq, Show)

makePrisms ''DomainRedirectConfig

data DomainVerificationTokenResponse = DomainVerificationTokenResponse
  { authToken :: Maybe DomainVerificationAuthToken,
    dnsToken :: DomainVerificationToken
  }
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema DomainVerificationTokenResponse)

instance ToSchema DomainVerificationTokenResponse where
  schema :: ValueSchema NamedSwaggerDoc DomainVerificationTokenResponse
  schema =
    object "DomainVerificationTokenResponse" $
      DomainVerificationTokenResponse
        <$> (.authToken) .= maybe_ (optField "auth_token" schema)
        <*> (.dnsToken) .= field "dns_verification_token" schema

deriving via (Schema DomainRedirectConfig) instance A.ToJSON DomainRedirectConfig

deriving via (Schema DomainRedirectConfig) instance A.FromJSON DomainRedirectConfig

deriving via (Schema DomainRedirectConfig) instance S.ToSchema DomainRedirectConfig

data DomainRedirectConfigTag
  = DomainRedirectConfigRemoveTag
  | DomainRedirectConfigBackendTag
  | DomainRedirectConfigNoRegistrationTag
  deriving (Show, Ord, Eq, Enum, Bounded)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema DomainRedirectConfigTag

instance ToSchema DomainRedirectConfigTag where
  schema =
    enum @Text
      "DomainRedirectConfigTag"
      $ mconcat
        [ element "remove" DomainRedirectConfigRemoveTag,
          element "backend" DomainRedirectConfigBackendTag,
          element "no-registration" DomainRedirectConfigNoRegistrationTag
        ]

domainRedirectConfigTagObjectSchema :: ObjectSchema SwaggerDoc DomainRedirectConfigTag
domainRedirectConfigTagObjectSchema =
  field "domain_redirect" schema

domainRedirectConfigToTag :: DomainRedirectConfig -> DomainRedirectConfigTag
domainRedirectConfigToTag = \case
  DomainRedirectConfigRemove -> DomainRedirectConfigRemoveTag
  DomainRedirectConfigBackend _ -> DomainRedirectConfigBackendTag
  DomainRedirectConfigNoRegistration -> DomainRedirectConfigNoRegistrationTag

domainRedirectConfigSchema :: ObjectSchema SwaggerDoc DomainRedirectConfig
domainRedirectConfigSchema =
  snd
    <$> (domainRedirectConfigToTag &&& id)
      .= bind
        (fst .= domainRedirectConfigTagObjectSchema)
        (snd .= dispatch domainRedirectConfigObjectSchema)
  where
    domainRedirectConfigObjectSchema :: DomainRedirectConfigTag -> ObjectSchema SwaggerDoc DomainRedirectConfig
    domainRedirectConfigObjectSchema = \case
      DomainRedirectConfigBackendTag -> tag _DomainRedirectConfigBackend backendUrlSchema
      DomainRedirectConfigNoRegistrationTag -> tag _DomainRedirectConfigNoRegistration (pure ())
      DomainRedirectConfigRemoveTag -> tag _DomainRedirectConfigRemove (pure ())

instance ToSchema DomainRedirectConfig where
  schema = object "DomainRedirectConfig" domainRedirectConfigSchema

newtype GetDomainRegistrationRequest = GetDomainRegistrationRequest {domainRegistrationRequestEmail :: EmailAddress}
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema GetDomainRegistrationRequest)

instance ToSchema GetDomainRegistrationRequest where
  schema =
    object "GetDomainRegistrationRequest" $
      GetDomainRegistrationRequest
        <$> domainRegistrationRequestEmail
          .= field "email" schema

data TeamInviteConfig = TeamInviteConfig
  { teamInvite :: TeamInvite,
    code :: Maybe SAML.IdPId
  }
  deriving (Show, Eq)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema TeamInviteConfig)

instance ToSchema TeamInviteConfig where
  schema =
    object "TeamInviteConfig" $
      TeamInviteConfig
        <$> (.teamInvite) .= teamInviteObjectSchema
        <*> code .= maybe_ (optField "sso" samlIdpIdSchema)

samlIdpIdSchema :: ValueSchema NamedSwaggerDoc SAML.IdPId
samlIdpIdSchema = SAML.fromIdPId .= fmap SAML.IdPId uuidSchema

type DomainVerificationAPI =
  Named
    "domain-verification-token"
    ( Summary "Get a DNS verification token"
        :> Header "Authorization" (Bearer DomainVerificationAuthToken)
        :> "domain-verification"
        :> Capture "domain" Domain
        :> "token"
        :> Post '[JSON] DomainVerificationTokenResponse
    )
    :<|> Named
           "domain-verification-token-team"
           ( Summary "Get a DNS verification token"
               :> CanThrow DomainVerificationAuthFailure
               :> CanThrow DomainVerificationPaymentRequired
               :> ZLocalUser
               :> "domain-verification"
               :> Capture "domain" Domain
               :> "team-token"
               :> Post '[JSON] DomainVerificationTokenResponse
           )
    :<|> Named
           "update-domain-redirect"
           ( Summary "Verify DNS record and save domain redirect configuration"
               :> CanThrow DomainVerificationOperationForbidden
               :> CanThrow DomainVerificationDomainVerificationFailed
               :> Header' '[Required, Strict] "Authorization" (Bearer DomainVerificationAuthToken)
               :> "domain-verification"
               :> Capture "domain" Domain
               :> "backend"
               :> ReqBody '[JSON] DomainRedirectConfig
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Updated")
           )
    :<|> Named
           "update-team-invite"
           ( Summary "Verify DNS record and save team-invite configuration"
               :> CanThrow DomainVerificationAuthFailure
               :> CanThrow DomainVerificationPaymentRequired
               :> CanThrow DomainVerificationOperationForbidden
               :> CanThrow DomainVerificationDomainVerificationFailed
               :> ZLocalUser
               :> "domain-verification"
               :> Capture "domain" Domain
               :> "team"
               :> ReqBody '[JSON] TeamInviteConfig
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Updated")
           )
    :<|> Named
           "get-domain-registration"
           ( Summary "Get domain registration configuration by email"
               :> CanThrow DomainVerificationInvalidDomain
               :> "get-domain-registration"
               :> ReqBody '[JSON] GetDomainRegistrationRequest
               :> Post '[JSON] DomainRedirect
           )
