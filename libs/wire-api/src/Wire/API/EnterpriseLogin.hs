{-# LANGUAGE TemplateHaskell #-}

module Wire.API.EnterpriseLogin where

import Control.Lens (Field1 (_1), makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Domain
import Data.Id
import Data.Misc
import Data.OpenApi qualified as OpenApi
import Data.Schema
import Imports
import SAML2.WebSSO qualified as SAML

data DomainRedirect
  = None
  | Locked
  | SSO SAML.IdPId
  | Backend HttpsUrl
  | NoRegistration
  | PreAuthorized

makePrisms ''DomainRedirect

data DomainRedirectTag
  = NoneTag
  | LockedTag
  | SSOTag
  | BackendTag
  | NoRegistrationTag
  | PreAuthorizedTag
  deriving (Eq, Enum, Bounded)

tagSchema :: ValueSchema NamedSwaggerDoc DomainRedirectTag
tagSchema =
  enum @Text "DomainRedirect Tag" $
    mconcat
      [ element "none" NoneTag,
        element "locked" LockedTag,
        element "sso" SSOTag,
        element "backend" BackendTag,
        element "no-registration" NoRegistrationTag,
        element "pre-authorized" PreAuthorizedTag
      ]

domainRedirectSchema :: ValueSchema NamedSwaggerDoc DomainRedirect
domainRedirectSchema =
  object "DomainRedirect" $
    fromTagged
      <$> toTagged
        .= bind
          (fst .= field "tag" tagSchema)
          (snd .= fieldOver _1 "value" untaggedSchema)
  where
    toTagged :: DomainRedirect -> (DomainRedirectTag, DomainRedirect)
    toTagged None = (NoneTag, None)
    toTagged Locked = (LockedTag, Locked)
    toTagged (SSO idpid) = (SSOTag, SSO idpid)
    toTagged (Backend url) = (BackendTag, Backend url)
    toTagged NoRegistration = (NoRegistrationTag, NoRegistration)
    toTagged PreAuthorized = (PreAuthorizedTag, PreAuthorized)

    fromTagged :: (DomainRedirectTag, DomainRedirect) -> DomainRedirect
    fromTagged = snd

    untaggedSchema = dispatch $ \case
      NoneTag -> tag _None null_
      LockedTag -> tag _Locked null_
      SSOTag -> tag _SSO (unnamed samlIdPIdSchema)
      BackendTag -> tag _Backend (unnamed schema)
      NoRegistrationTag -> tag _NoRegistration null_
      PreAuthorizedTag -> tag _PreAuthorized null_

samlIdPIdSchema :: ValueSchema NamedSwaggerDoc SAML.IdPId
samlIdPIdSchema = SAML.IdPId <$> SAML.fromIdPId .= uuidSchema

instance ToSchema DomainRedirect where
  schema = domainRedirectSchema

deriving via (Schema DomainRedirect) instance FromJSON DomainRedirect

deriving via (Schema DomainRedirect) instance ToJSON DomainRedirect

deriving via (Schema DomainRedirect) instance OpenApi.ToSchema DomainRedirect

data TeamInvite
  = Allowed
  | NotAllowed
  | Team TeamId

makePrisms ''TeamInvite

data TeamInviteTag
  = AllowedTag
  | NotAllowedTag
  | TeamTag
  deriving (Eq, Enum, Bounded)

teamInviteTagSchema :: ValueSchema NamedSwaggerDoc TeamInviteTag
teamInviteTagSchema =
  enum @Text "TeamInvite Tag" $
    mconcat
      [ element "allowed" AllowedTag,
        element "not-allowed" NotAllowedTag,
        element "team" TeamTag
      ]

teamInviteSchema :: ValueSchema NamedSwaggerDoc TeamInvite
teamInviteSchema =
  object "TeamInvite" $
    fromTagged
      <$> toTagged
        .= bind
          (fst .= field "tag" teamInviteTagSchema)
          (snd .= fieldOver _1 "value" untaggedSchema)
  where
    toTagged :: TeamInvite -> (TeamInviteTag, TeamInvite)
    toTagged Allowed = (AllowedTag, Allowed)
    toTagged NotAllowed = (NotAllowedTag, NotAllowed)
    toTagged (Team teamId) = (TeamTag, Team teamId)

    fromTagged :: (TeamInviteTag, TeamInvite) -> TeamInvite
    fromTagged = snd

    untaggedSchema = dispatch $ \case
      AllowedTag -> tag _Allowed null_
      NotAllowedTag -> tag _NotAllowed null_
      TeamTag -> tag _Team (unnamed schema)

instance ToSchema TeamInvite where
  schema = teamInviteSchema

deriving via (Schema TeamInvite) instance FromJSON TeamInvite

deriving via (Schema TeamInvite) instance ToJSON TeamInvite

deriving via (Schema TeamInvite) instance OpenApi.ToSchema TeamInvite

newtype DnsVerificationToken = DnsVerificationToken {unDnsVerificationToken :: Text}
  deriving (ToJSON, FromJSON, OpenApi.ToSchema) via Schema DnsVerificationToken

instance ToSchema DnsVerificationToken where
  schema = DnsVerificationToken <$> unDnsVerificationToken .= schema

data DomainRegistrationUpdate = DomainRegistrationUpdate
  { domainRedirect :: DomainRedirect,
    teamInvite :: TeamInvite
  }
  deriving (ToJSON, FromJSON, OpenApi.ToSchema) via Schema DomainRegistrationUpdate

instance ToSchema DomainRegistrationUpdate where
  schema =
    object "DomainRegistrationUpdate" $
      DomainRegistrationUpdate
        <$> (.domainRedirect) .= field "domain-redirect" schema
        <*> (.teamInvite) .= field "team-invite" schema

data DomainRegistration = DomainRegistration
  { domain :: Domain,
    domainRedirect :: DomainRedirect,
    teamInvite :: TeamInvite,
    dnsVerificationToken :: DnsVerificationToken
  }
  deriving (ToJSON, FromJSON, OpenApi.ToSchema) via Schema DomainRegistration

instance ToSchema DomainRegistration where
  schema =
    object "DomainRegistration" $
      DomainRegistration
        <$> (.domain) .= field "domain" schema
        <*> (.domainRedirect) .= field "domain-redirect" schema
        <*> (.teamInvite) .= field "team-invite" schema
        <*> (.dnsVerificationToken) .= field "dns-verification-token" schema
