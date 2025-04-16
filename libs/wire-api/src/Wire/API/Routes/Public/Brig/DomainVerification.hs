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
import Wire.API.Routes.Version
import Wire.API.User.Identity (EmailAddress)
import Wire.Arbitrary

data DomainRedirectConfig (v :: Version)
  = DomainRedirectConfigRemove
  | DomainRedirectConfigBackend
      -- | Backend URL
      HttpsUrl
      -- | WebApp URL
      (Maybe HttpsUrl)
  | DomainRedirectConfigNoRegistration
  deriving stock (Eq, Show)

makePrisms ''DomainRedirectConfig

type DomainRedirectConfigV8 = DomainRedirectConfig V8

deriving via (Schema DomainRedirectConfigV8) instance A.ToJSON DomainRedirectConfigV8

deriving via (Schema DomainRedirectConfigV8) instance A.FromJSON DomainRedirectConfigV8

deriving via (Schema DomainRedirectConfigV8) instance S.ToSchema DomainRedirectConfigV8

-- TODO: Keep the name unversioned until we know it has to be versioned
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

domainRedirectConfigToTag :: DomainRedirectConfig a -> DomainRedirectConfigTag
domainRedirectConfigToTag = \case
  DomainRedirectConfigRemove -> DomainRedirectConfigRemoveTag
  DomainRedirectConfigBackend _ _ -> DomainRedirectConfigBackendTag
  DomainRedirectConfigNoRegistration -> DomainRedirectConfigNoRegistrationTag

domainRedirectConfigV8Schema :: ObjectSchema SwaggerDoc DomainRedirectConfigV8
domainRedirectConfigV8Schema =
  snd
    <$> (domainRedirectConfigToTag &&& id)
      .= bind
        (fst .= domainRedirectConfigTagObjectSchema)
        (snd .= dispatch domainRedirectConfigObjectSchema)
  where
    domainRedirectConfigObjectSchema :: DomainRedirectConfigTag -> ObjectSchema SwaggerDoc DomainRedirectConfigV8
    domainRedirectConfigObjectSchema = \case
      DomainRedirectConfigBackendTag -> tag _DomainRedirectConfigBackend backendConfigSchema
      DomainRedirectConfigNoRegistrationTag -> tag _DomainRedirectConfigNoRegistration (pure ())
      DomainRedirectConfigRemoveTag -> tag _DomainRedirectConfigRemove (pure ())

    backendConfigSchema :: ObjectSchema SwaggerDoc (HttpsUrl, Maybe HttpsUrl)
    backendConfigSchema =
      (,)
        <$> fst .= backendUrlSchema
        <*> snd .= pure Nothing

instance ToSchema DomainRedirectConfigV8 where
  schema = object "DomainRedirectConfigV8" domainRedirectConfigV8Schema

type DomainRedirectConfigV9 = DomainRedirectConfig V9

deriving via (Schema DomainRedirectConfigV9) instance A.ToJSON DomainRedirectConfigV9

deriving via (Schema DomainRedirectConfigV9) instance A.FromJSON DomainRedirectConfigV9

deriving via (Schema DomainRedirectConfigV9) instance S.ToSchema DomainRedirectConfigV9

domainRedirectConfigV9Schema :: ObjectSchema SwaggerDoc DomainRedirectConfigV9
domainRedirectConfigV9Schema =
  snd
    <$> (domainRedirectConfigToTag &&& id)
      .= bind
        (fst .= domainRedirectConfigTagObjectSchema)
        (snd .= dispatch domainRedirectConfigObjectSchema)
  where
    -- TODO: Duplication with V8: Probably can be toplevel.
    domainRedirectConfigObjectSchema :: DomainRedirectConfigTag -> ObjectSchema SwaggerDoc DomainRedirectConfigV9
    domainRedirectConfigObjectSchema = \case
      DomainRedirectConfigBackendTag -> tag _DomainRedirectConfigBackend backendObjectSchema
      DomainRedirectConfigNoRegistrationTag -> tag _DomainRedirectConfigNoRegistration (pure ())
      DomainRedirectConfigRemoveTag -> tag _DomainRedirectConfigRemove (pure ())

    backendObjectSchema :: ObjectSchema SwaggerDoc (HttpsUrl, Maybe HttpsUrl)
    backendObjectSchema = field "backend" backendConfigSchema

    backendConfigSchema :: ValueSchema NamedSwaggerDoc (HttpsUrl, Maybe HttpsUrl)
    backendConfigSchema =
      object "backend_config" $
        (,)
          <$> fst .= field "config" schema
          -- TODO: Should be discussed if this could really be optinal. However, to me it makes sense.
          <*> snd .= maybe_ (optField "webapp" schema)

instance ToSchema DomainRedirectConfigV9 where
  schema = object "DomainRedirectConfigV9" domainRedirectConfigV9Schema

newtype GetDomainRegistrationRequest = GetDomainRegistrationRequest {domainRegistrationRequestEmail :: EmailAddress}
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema GetDomainRegistrationRequest)

instance ToSchema GetDomainRegistrationRequest where
  schema =
    object "GetDomainRegistrationRequest" $
      GetDomainRegistrationRequest
        <$> domainRegistrationRequestEmail
          .= field "email" schema

data TeamDomainRedirect
  = TeamSso SAML.IdPId
  | TeamNoRegistration
  | TeamNone
  deriving (Show, Eq)

data TeamDomainRedirectTag = TeamNoRegistrationTag | TeamNoneTag
  deriving (Show, Ord, Eq, Enum, Bounded)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema TeamDomainRedirectTag

instance ToSchema TeamDomainRedirectTag where
  schema =
    enum @Text
      "TeamDomainRedirectTag"
      $ mconcat
        [ element "no-registration" TeamNoRegistrationTag,
          element "none" TeamNoneTag
        ]

idpIdValueSchema :: ValueSchema SwaggerDoc SAML.IdPId
idpIdValueSchema = SAML.fromIdPId .= fmap SAML.IdPId (Data.Schema.unnamed uuidSchema)

maybeTeamDomainRedirectTargetObjectSchema :: ObjectSchemaP SwaggerDoc (Maybe TeamDomainRedirectTag, Maybe SAML.IdPId) (Maybe TeamDomainRedirect)
maybeTeamDomainRedirectTargetObjectSchema =
  withParser teamDomainRedirectTargetTupleObjectSchema maybeTeamDomainRedirectTargetTargetFromTuple
  where
    teamDomainRedirectTargetTupleObjectSchema :: ObjectSchema SwaggerDoc (Maybe TeamDomainRedirectTag, Maybe SAML.IdPId)
    teamDomainRedirectTargetTupleObjectSchema =
      (,)
        <$> fst .= maybe_ (optField "domain_redirect" schema)
        <*> snd .= maybe_ (optField "sso" idpIdValueSchema)

    fromTag :: TeamDomainRedirectTag -> TeamDomainRedirect
    fromTag = \case
      TeamNoRegistrationTag -> TeamNoRegistration
      TeamNoneTag -> TeamNone

    maybeTeamDomainRedirectTargetTargetFromTuple :: (Maybe TeamDomainRedirectTag, Maybe SAML.IdPId) -> A.Parser (Maybe TeamDomainRedirect)
    maybeTeamDomainRedirectTargetTargetFromTuple = \case
      (Just _, Just _) -> fail "only one of domain_redirect or sso must be present"
      (Just redirect, _) -> pure $ Just (fromTag redirect)
      (_, Just sso) -> pure $ Just (TeamSso sso)
      (Nothing, Nothing) -> pure Nothing

maybeTeamDomainRedirectToTuple :: Maybe TeamDomainRedirect -> (Maybe TeamDomainRedirectTag, Maybe SAML.IdPId)
maybeTeamDomainRedirectToTuple = \case
  (Just TeamNone) -> (Just TeamNoneTag, Nothing)
  (Just TeamNoRegistration) -> (Just TeamNoRegistrationTag, Nothing)
  (Just (TeamSso sso)) -> (Nothing, Just sso)
  Nothing -> (Nothing, Nothing)

data TeamInviteConfig = TeamInviteConfig
  { teamInvite :: TeamInvite,
    domainRedirect :: Maybe TeamDomainRedirect
  }
  deriving (Show, Eq)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema TeamInviteConfig)

instance ToSchema TeamInviteConfig where
  schema =
    object "TeamInviteConfig" $
      TeamInviteConfig
        <$> (.teamInvite) .= teamInviteObjectSchema
        <*> (maybeTeamDomainRedirectToTuple . (.domainRedirect)) .= maybeTeamDomainRedirectTargetObjectSchema

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

type DomainRedirectResponseV8 = DomainRedirectResponse V8

data DomainRedirectResponse (v :: Version) = DomainRedirectResponse
  { propagateUserExists :: Bool,
    redirect :: DomainRedirect
  }
  deriving (Eq, Show, Generic)

deriving via GenericUniform DomainRedirectResponseV8 instance Arbitrary DomainRedirectResponseV8

deriving via Schema DomainRedirectResponseV8 instance A.ToJSON DomainRedirectResponseV8

deriving via Schema DomainRedirectResponseV8 instance A.FromJSON DomainRedirectResponseV8

deriving via Schema DomainRedirectResponseV8 instance S.ToSchema DomainRedirectResponseV8

instance ToSchema DomainRedirectResponseV8 where
  schema =
    object "DomainRedirectResponseV8" $
      DomainRedirectResponse
        <$> (\r -> True <$ guard r.propagateUserExists)
          .= maybe_
            ( fromMaybe False <$> optField "due_to_existing_account" schema
            )
        <*> (.redirect) .= domainRedirectSchema

type DomainRedirectResponseV9 = DomainRedirectResponse V9

deriving via GenericUniform DomainRedirectResponseV9 instance Arbitrary DomainRedirectResponseV9

deriving via Schema DomainRedirectResponseV9 instance A.ToJSON DomainRedirectResponseV9

deriving via Schema DomainRedirectResponseV9 instance A.FromJSON DomainRedirectResponseV9

deriving via Schema DomainRedirectResponseV9 instance S.ToSchema DomainRedirectResponseV9

instance ToSchema DomainRedirectResponseV9 where
  schema =
    object "DomainRedirectResponseV9" $
      DomainRedirectResponse
        <$> (\r -> True <$ guard r.propagateUserExists)
          .= maybe_
            ( fromMaybe False <$> optField "due_to_existing_account" schema
            )
        <*> (.redirect) .= domainRedirectSchema

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
    "verify-challenge-team"
    ( Summary "Verify a DNS verification challenge for a team"
        :> CanThrow DomainVerificationAuthFailure
        :> CanThrow DomainVerificationPaymentRequired
        :> CanThrow DomainVerificationOperationForbidden
        :> ZLocalUser
        :> "domain-verification"
        :> Capture "domain" Domain
        :> "team"
        :> "challenges"
        :> Capture "challengeId" ChallengeId
        :> ReqBody '[JSON] ChallengeToken
        :> Post '[JSON] DomainOwnershipToken
    )
    :<|> Named
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
    "update-domain-redirect@v8"
    ( Summary "Update the domain redirect configuration"
        :> Until V9
        :> CanThrow DomainVerificationAuthFailure
        :> CanThrow DomainVerificationOperationForbidden
        :> Header' '[Required, Strict] "Authorization" (Bearer Token)
        :> "domain-verification"
        :> Capture "domain" Domain
        :> "backend"
        :> ReqBody '[JSON] DomainRedirectConfigV8
        :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Updated")
    )
    :<|> Named
           "update-domain-redirect"
           ( Summary "Update the domain redirect configuration"
               :> From V9
               :> CanThrow DomainVerificationAuthFailure
               :> CanThrow DomainVerificationOperationForbidden
               :> Header' '[Required, Strict] "Authorization" (Bearer Token)
               :> "domain-verification"
               :> Capture "domain" Domain
               :> "backend"
               :> ReqBody '[JSON] DomainRedirectConfigV9
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Updated")
           )
    :<|> Named
           "get-domain-registration@v8"
           ( Summary "Get domain registration configuration by email"
               :> Until V9
               :> CanThrow DomainVerificationInvalidDomain
               :> "get-domain-registration"
               :> ReqBody '[JSON] GetDomainRegistrationRequest
               :> Post '[JSON] DomainRedirectResponseV8
           )
    :<|> Named
           "get-domain-registration"
           ( Summary "Get domain registration configuration by email"
               :> From V9
               :> CanThrow DomainVerificationInvalidDomain
               :> "get-domain-registration"
               :> ReqBody '[JSON] GetDomainRegistrationRequest
               :> Post '[JSON] DomainRedirectResponseV9
           )
