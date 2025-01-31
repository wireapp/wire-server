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

data DomainVerificationChallenge = DomainVerificationChallenge
  { challengeId :: ChallengeId,
    -- | unhashed/plaintext short lived challenge auth token
    token :: Token,
    dnsVerificationToken :: DnsVerificationToken
  }
  deriving (Eq, Show)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema DomainVerificationChallenge)

instance ToSchema DomainVerificationChallenge where
  schema =
    object "DomainVerificationChallenge" $
      DomainVerificationChallenge
        <$> challengeId .= field "id" schema
        <*> token .= field "token" schema
        <*> (.dnsVerificationToken) .= field "dns_verification_token" schema

newtype ChallengeToken = ChallengeToken {unChallengeToken :: Token}
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema ChallengeToken)

instance ToSchema ChallengeToken where
  schema =
    object "ChallengeToken" $
      ChallengeToken
        <$> unChallengeToken .= field "challenge_token" schema

newtype DomainOwnershipToken = DomainOwnershipToken {unDomainOwnershipToken :: Token}
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema DomainOwnershipToken)

instance ToSchema DomainOwnershipToken where
  schema =
    object "DomainOwnershipToken" $
      DomainOwnershipToken
        <$> unDomainOwnershipToken .= field "domain_ownership_token" schema

newtype RegisteredDomains = RegisteredDomains {unRegisteredDomains :: [DomainRegistrationResponse]}
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema RegisteredDomains)

instance ToSchema RegisteredDomains where
  schema =
    object "RegisteredDomains" $
      RegisteredDomains
        <$> unRegisteredDomains .= field "registered_domains" (array schema)

type DomainVerificationChallengeAPI =
  Named
    "domain-verification-challenge"
    ( Summary "Get a DNS verification challenge"
        :> "domain-verification"
        :> Capture "domain" Domain
        :> "challenges"
        :> Post '[JSON] DomainVerificationChallenge
    )
    :<|> Named
           "verify-challenge"
           ( Summary "Verify a DNS verification challenge"
               :> CanThrow DomainVerificationChallengeNotFound
               :> CanThrow DomainVerificationAuthFailure
               :> CanThrow DomainVerificationDomainVerificationFailed
               :> "domain-verification"
               :> Capture "domain" Domain
               :> "challenges"
               :> Capture "challengeId" ChallengeId
               :> ReqBody '[JSON] ChallengeToken
               :> Post '[JSON] DomainOwnershipToken
           )

type DomainVerificationTeamAPI =
  Named
    "domain-verification-authorize-team"
    ( Summary "Authorize a team to operate on a verified domain"
        :> CanThrow DomainVerificationAuthFailure
        :> CanThrow DomainVerificationPaymentRequired
        :> CanThrow DomainVerificationOperationForbidden
        :> ZLocalUser
        :> "domain-verification"
        :> Capture "domain" Domain
        :> "authorize-team"
        :> ReqBody '[JSON] DomainOwnershipToken
        :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Authorized")
    )
    :<|> Named
           "update-team-invite"
           ( Summary "Update the team-invite configuration"
               :> CanThrow DomainVerificationAuthFailure
               :> CanThrow DomainVerificationPaymentRequired
               :> CanThrow DomainVerificationOperationForbidden
               :> ZLocalUser
               :> "domain-verification"
               :> Capture "domain" Domain
               :> "team"
               :> ReqBody '[JSON] TeamInviteConfig
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Updated")
           )
    :<|> Named
           "get-all-registered-domains"
           ( Summary "Get all registered domains"
               :> ZLocalUser
               :> "teams"
               :> Capture "teamId" TeamId
               :> "registered-domains"
               :> Get '[JSON] RegisteredDomains
           )
    :<|> Named
           "delete-registered-domain"
           ( Summary "Delete a registered domain"
               :> CanThrow DomainVerificationAuthFailure
               :> CanThrow DomainVerificationPaymentRequired
               :> CanThrow DomainVerificationOperationForbidden
               :> ZLocalUser
               :> "teams"
               :> Capture "teamId" TeamId
               :> "registered-domains"
               :> Capture "domain" Domain
               :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 204 "Deleted")
           )

type DomainVerificationAPI =
  Named
    "update-domain-redirect"
    ( Summary "Update the domain redirect configuration"
        :> CanThrow DomainVerificationAuthFailure
        :> CanThrow DomainVerificationOperationForbidden
        :> Header' '[Required, Strict] "Authorization" (Bearer Token)
        :> "domain-verification"
        :> Capture "domain" Domain
        :> "backend"
        :> ReqBody '[JSON] DomainRedirectConfig
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
