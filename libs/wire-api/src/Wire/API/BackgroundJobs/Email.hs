{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Composing payload for outbound email.
--
-- Outbound email is queued to the background-worker as a 'SendEmail' job on
-- the Arbiter @emails@ queue (see "Wire.API.Jobs"). Producers (brig) enqueue
-- the /composing payload/ only: the email type, locale and structured inputs
-- (recipient, keys\/codes, team names, cert summaries, ...). The
-- background-worker composes the actual email (locale template selection,
-- placeholder rendering, MIME building) right before sending. No rendered
-- email content ever crosses the queue.
module Wire.API.BackgroundJobs.Email where

import Control.Arrow ((&&&))
import Control.Lens (makePrisms)
import Data.Aeson qualified as Aeson
import Data.Code qualified as Code
import Data.Id
import Data.Schema
import Imports
import Test.QuickCheck (oneof)
import Wire.API.EnterpriseLogin (DomainRegistrationResponse, mkDomainRegistrationResponse)
import Wire.API.Locale
import Wire.API.Routes.Version (Version (V10))
import Wire.API.User
import Wire.API.User.Activation (ActivationCode, ActivationKey)
import Wire.API.User.Client (Client)
import Wire.API.User.Password (PasswordResetCode, PasswordResetKey)
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

-- | Fingerprint summary of an IdP certificate, as needed for the
-- IdP-configuration-change notification email.
data CertSummary = CertSummary
  { algorithm :: !Text,
    fingerprint :: !Text,
    subject :: !Text,
    issuer :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema CertSummary)
  deriving (Arbitrary) via GenericUniform CertSummary

instance ToSchema CertSummary where
  schema =
    object $
      CertSummary
        <$> (.algorithm) .= field "algorithm" schema
        <*> (.fingerprint) .= field "fingerprint" schema
        <*> (.subject) .= field "subject" schema
        <*> (.issuer) .= field "issuer" schema

data VerificationEmail = MkVerificationEmail
  { to :: !EmailAddress,
    key :: !ActivationKey,
    code :: !ActivationCode,
    locale :: !(Maybe Locale)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema VerificationEmail)
  deriving (Arbitrary) via GenericUniform VerificationEmail

instance ToSchema VerificationEmail where
  schema =
    object $
      MkVerificationEmail
        <$> (.to) .= field "to" schema
        <*> (.key) .= field "key" schema
        <*> (.code) .= field "code" schema
        <*> (.locale) .= maybe_ (optField "locale" schema)

data ActivationEmail = MkActivationEmail
  { to :: !EmailAddress,
    name :: !Name,
    key :: !ActivationKey,
    code :: !ActivationCode,
    locale :: !(Maybe Locale)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema ActivationEmail)
  deriving (Arbitrary) via GenericUniform ActivationEmail

instance ToSchema ActivationEmail where
  schema =
    object $
      MkActivationEmail
        <$> (.to) .= field "to" schema
        <*> (.name) .= field "name" schema
        <*> (.key) .= field "key" schema
        <*> (.code) .= field "code" schema
        <*> (.locale) .= maybe_ (optField "locale" schema)

data TeamActivationEmail = MkTeamActivationEmail
  { to :: !EmailAddress,
    name :: !Name,
    key :: !ActivationKey,
    code :: !ActivationCode,
    teamName :: !Text,
    locale :: !(Maybe Locale)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema TeamActivationEmail)
  deriving (Arbitrary) via GenericUniform TeamActivationEmail

instance ToSchema TeamActivationEmail where
  schema =
    object $
      MkTeamActivationEmail
        <$> (.to) .= field "to" schema
        <*> (.name) .= field "name" schema
        <*> (.key) .= field "key" schema
        <*> (.code) .= field "code" schema
        <*> (.teamName) .= field "team_name" schema
        <*> (.locale) .= maybe_ (optField "locale" schema)

data PasswordResetEmail = MkPasswordResetEmail
  { to :: !EmailAddress,
    key :: !PasswordResetKey,
    code :: !PasswordResetCode,
    locale :: !(Maybe Locale)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema PasswordResetEmail)
  deriving (Arbitrary) via GenericUniform PasswordResetEmail

instance ToSchema PasswordResetEmail where
  schema =
    object $
      MkPasswordResetEmail
        <$> (.to) .= field "to" schema
        <*> (.key) .= field "key" schema
        <*> (.code) .= field "code" schema
        <*> (.locale) .= maybe_ (optField "locale" schema)

data NewClientEmail = MkNewClientEmail
  { to :: !EmailAddress,
    name :: !Name,
    client :: !Client,
    locale :: !Locale
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema NewClientEmail)
  deriving (Arbitrary) via GenericUniform NewClientEmail

instance ToSchema NewClientEmail where
  schema =
    object $
      MkNewClientEmail
        <$> (.to) .= field "to" schema
        <*> (.name) .= field "name" schema
        <*> (.client) .= field "client" schema
        <*> (.locale) .= field "locale" schema

data AccountDeletionEmail = MkAccountDeletionEmail
  { to :: !EmailAddress,
    name :: !Name,
    key :: !Code.Key,
    code :: !Code.Value,
    locale :: !Locale
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema AccountDeletionEmail)
  deriving (Arbitrary) via GenericUniform AccountDeletionEmail

instance ToSchema AccountDeletionEmail where
  schema =
    object $
      MkAccountDeletionEmail
        <$> (.to) .= field "to" schema
        <*> (.name) .= field "name" schema
        <*> (.key) .= field "key" schema
        <*> (.code) .= field "code" schema
        <*> (.locale) .= field "locale" schema

data SecondFactorVerificationEmail = MkSecondFactorVerificationEmail
  { to :: !EmailAddress,
    code :: !Code.Value,
    locale :: !(Maybe Locale)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SecondFactorVerificationEmail)
  deriving (Arbitrary) via GenericUniform SecondFactorVerificationEmail

instance ToSchema SecondFactorVerificationEmail where
  schema =
    object $
      MkSecondFactorVerificationEmail
        <$> (.to) .= field "to" schema
        <*> (.code) .= field "code" schema
        <*> (.locale) .= maybe_ (optField "locale" schema)

data TeamInvitationEmail = MkTeamInvitationEmail
  { to :: !EmailAddress,
    teamId :: !TeamId,
    -- | the inviting user's email address (renders the template's
    -- @${inviter}@ placeholder)
    inviter :: !EmailAddress,
    code :: !InvitationCode,
    locale :: !(Maybe Locale)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema TeamInvitationEmail)
  deriving (Arbitrary) via GenericUniform TeamInvitationEmail

instance ToSchema TeamInvitationEmail where
  schema =
    object $
      MkTeamInvitationEmail
        <$> (.to) .= field "to" schema
        <*> (.teamId) .= field "team_id" schema
        <*> (.inviter) .= field "inviter" schema
        <*> (.code) .= field "code" schema
        <*> (.locale) .= maybe_ (optField "locale" schema)

data MemberWelcomeEmail = MkMemberWelcomeEmail
  { to :: !EmailAddress,
    teamId :: !TeamId,
    teamName :: !Text,
    locale :: !(Maybe Locale)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema MemberWelcomeEmail)
  deriving (Arbitrary) via GenericUniform MemberWelcomeEmail

instance ToSchema MemberWelcomeEmail where
  schema =
    object $
      MkMemberWelcomeEmail
        <$> (.to) .= field "to" schema
        <*> (.teamId) .= field "team_id" schema
        <*> (.teamName) .= field "team_name" schema
        <*> (.locale) .= maybe_ (optField "locale" schema)

data NewTeamOwnerWelcomeEmail = MkNewTeamOwnerWelcomeEmail
  { to :: !EmailAddress,
    teamId :: !TeamId,
    teamName :: !Text,
    profileName :: !Name,
    locale :: !(Maybe Locale)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema NewTeamOwnerWelcomeEmail)
  deriving (Arbitrary) via GenericUniform NewTeamOwnerWelcomeEmail

instance ToSchema NewTeamOwnerWelcomeEmail where
  schema =
    object $
      MkNewTeamOwnerWelcomeEmail
        <$> (.to) .= field "to" schema
        <*> (.teamId) .= field "team_id" schema
        <*> (.teamName) .= field "team_name" schema
        <*> (.profileName) .= field "profile_name" schema
        <*> (.locale) .= maybe_ (optField "locale" schema)

data IdpChangedEmail = MkIdpChangedEmail
  { to :: !EmailAddress,
    teamId :: !TeamId,
    userId :: !(Maybe UserId),
    addedCerts :: ![CertSummary],
    removedCerts :: ![CertSummary],
    idpId :: !Text,
    oldIssuer :: !(Maybe Text),
    oldEndpoint :: !(Maybe Text),
    newIssuer :: !(Maybe Text),
    newEndpoint :: !(Maybe Text),
    locale :: !(Maybe Locale)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema IdpChangedEmail)
  deriving (Arbitrary) via GenericUniform IdpChangedEmail

instance ToSchema IdpChangedEmail where
  schema =
    object $
      MkIdpChangedEmail
        <$> (.to) .= field "to" schema
        <*> (.teamId) .= field "team_id" schema
        <*> (.userId) .= maybe_ (optField "user_id" schema)
        <*> (.addedCerts) .= field "added_certs" (array schema)
        <*> (.removedCerts) .= field "removed_certs" (array schema)
        <*> (.idpId) .= field "idp_id" schema
        <*> (.oldIssuer) .= maybe_ (optField "old_issuer" schema)
        <*> (.oldEndpoint) .= maybe_ (optField "old_endpoint" schema)
        <*> (.newIssuer) .= maybe_ (optField "new_issuer" schema)
        <*> (.newEndpoint) .= maybe_ (optField "new_endpoint" schema)
        <*> (.locale) .= maybe_ (optField "locale" schema)

data ProviderActivationEmail = MkProviderActivationEmail
  { to :: !EmailAddress,
    name :: !Name,
    key :: !Code.Key,
    code :: !Code.Value,
    update :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema ProviderActivationEmail)
  deriving (Arbitrary) via GenericUniform ProviderActivationEmail

instance ToSchema ProviderActivationEmail where
  schema =
    object $
      MkProviderActivationEmail
        <$> (.to) .= field "to" schema
        <*> (.name) .= field "name" schema
        <*> (.key) .= field "key" schema
        <*> (.code) .= field "code" schema
        <*> (.update) .= field "update" schema

data ProviderApprovalConfirmEmail = MkProviderApprovalConfirmEmail
  { to :: !EmailAddress,
    name :: !Name
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema ProviderApprovalConfirmEmail)
  deriving (Arbitrary) via GenericUniform ProviderApprovalConfirmEmail

instance ToSchema ProviderApprovalConfirmEmail where
  schema =
    object $
      MkProviderApprovalConfirmEmail
        <$> (.to) .= field "to" schema
        <*> (.name) .= field "name" schema

data ProviderPasswordResetEmail = MkProviderPasswordResetEmail
  { to :: !EmailAddress,
    key :: !Code.Key,
    code :: !Code.Value
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema ProviderPasswordResetEmail)
  deriving (Arbitrary) via GenericUniform ProviderPasswordResetEmail

instance ToSchema ProviderPasswordResetEmail where
  schema =
    object $
      MkProviderPasswordResetEmail
        <$> (.to) .= field "to" schema
        <*> (.key) .= field "key" schema
        <*> (.code) .= field "code" schema

data EnterpriseAuditEmail = MkEnterpriseAuditEmail
  { from :: !EmailAddress,
    to :: !EmailAddress,
    subject :: !Text,
    url :: !Text,
    before :: !(Maybe (DomainRegistrationResponse V10)),
    after :: !(Maybe (DomainRegistrationResponse V10))
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema EnterpriseAuditEmail)

instance ToSchema EnterpriseAuditEmail where
  schema =
    object $
      MkEnterpriseAuditEmail
        <$> (.from) .= field "from" schema
        <*> (.to) .= field "to" schema
        <*> (.subject) .= field "subject" schema
        <*> (.url) .= field "url" schema
        <*> (.before) .= maybe_ (optField "before" schema)
        <*> (.after) .= maybe_ (optField "after" schema)

instance Arbitrary EnterpriseAuditEmail where
  arbitrary =
    MkEnterpriseAuditEmail
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (fmap mkDomainRegistrationResponse <$> arbitrary)
      <*> (fmap mkDomainRegistrationResponse <$> arbitrary)

-- | The composing payload enqueued on the @emails@ queue: the email variant
-- plus its structured inputs. Keep the type tags and nested data shapes stable
-- when changing job payloads; workers decode them later, so changes require a
-- coordinated rollout.
data SendEmailRequest
  = VerificationEmail !VerificationEmail
  | ActivationEmail !ActivationEmail
  | EmailAddressUpdateEmail !ActivationEmail
  | TeamActivationEmail !TeamActivationEmail
  | PasswordResetEmail !PasswordResetEmail
  | NewClientEmail !NewClientEmail
  | AccountDeletionEmail !AccountDeletionEmail
  | LoginVerificationEmail !SecondFactorVerificationEmail
  | ScimTokenVerificationEmail !SecondFactorVerificationEmail
  | TeamDeletionVerificationEmail !SecondFactorVerificationEmail
  | TeamInvitationEmail !TeamInvitationEmail
  | TeamInvitationPersonalUserEmail !TeamInvitationEmail
  | MemberWelcomeEmail !MemberWelcomeEmail
  | NewTeamOwnerWelcomeEmail !NewTeamOwnerWelcomeEmail
  | IdpChangedEmail !IdpChangedEmail
  | ProviderActivationEmail !ProviderActivationEmail
  | ProviderApprovalConfirmEmail !ProviderApprovalConfirmEmail
  | ProviderPasswordResetEmail !ProviderPasswordResetEmail
  | EnterpriseAuditEmail !EnterpriseAuditEmail
  deriving stock (Eq, Show, Generic)

data SendEmailRequestTag
  = VerificationEmailTag
  | ActivationEmailTag
  | EmailAddressUpdateEmailTag
  | TeamActivationEmailTag
  | PasswordResetEmailTag
  | NewClientEmailTag
  | AccountDeletionEmailTag
  | LoginVerificationEmailTag
  | ScimTokenVerificationEmailTag
  | TeamDeletionVerificationEmailTag
  | TeamInvitationEmailTag
  | TeamInvitationPersonalUserEmailTag
  | MemberWelcomeEmailTag
  | NewTeamOwnerWelcomeEmailTag
  | IdpChangedEmailTag
  | ProviderActivationEmailTag
  | ProviderApprovalConfirmEmailTag
  | ProviderPasswordResetEmailTag
  | EnterpriseAuditEmailTag
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via GenericUniform SendEmailRequestTag

instance ToSchema SendEmailRequestTag where
  schema =
    enum @Text $
      mconcat
        [ element "verification" VerificationEmailTag,
          element "activation" ActivationEmailTag,
          element "email_update" EmailAddressUpdateEmailTag,
          element "team_activation" TeamActivationEmailTag,
          element "password_reset" PasswordResetEmailTag,
          element "new_client" NewClientEmailTag,
          element "account_deletion" AccountDeletionEmailTag,
          element "login_verification" LoginVerificationEmailTag,
          element "scim_token_verification" ScimTokenVerificationEmailTag,
          element "team_deletion_verification" TeamDeletionVerificationEmailTag,
          element "team_invitation" TeamInvitationEmailTag,
          element "team_invitation_personal_user" TeamInvitationPersonalUserEmailTag,
          element "member_welcome" MemberWelcomeEmailTag,
          element "new_team_owner_welcome" NewTeamOwnerWelcomeEmailTag,
          element "idp_changed" IdpChangedEmailTag,
          element "provider_activation" ProviderActivationEmailTag,
          element "provider_approval_confirm" ProviderApprovalConfirmEmailTag,
          element "provider_password_reset" ProviderPasswordResetEmailTag,
          element "enterprise_audit" EnterpriseAuditEmailTag
        ]

makePrisms ''SendEmailRequest

sendEmailRequestTag :: SendEmailRequest -> SendEmailRequestTag
sendEmailRequestTag = \case
  VerificationEmail {} -> VerificationEmailTag
  ActivationEmail {} -> ActivationEmailTag
  EmailAddressUpdateEmail {} -> EmailAddressUpdateEmailTag
  TeamActivationEmail {} -> TeamActivationEmailTag
  PasswordResetEmail {} -> PasswordResetEmailTag
  NewClientEmail {} -> NewClientEmailTag
  AccountDeletionEmail {} -> AccountDeletionEmailTag
  LoginVerificationEmail {} -> LoginVerificationEmailTag
  ScimTokenVerificationEmail {} -> ScimTokenVerificationEmailTag
  TeamDeletionVerificationEmail {} -> TeamDeletionVerificationEmailTag
  TeamInvitationEmail {} -> TeamInvitationEmailTag
  TeamInvitationPersonalUserEmail {} -> TeamInvitationPersonalUserEmailTag
  MemberWelcomeEmail {} -> MemberWelcomeEmailTag
  NewTeamOwnerWelcomeEmail {} -> NewTeamOwnerWelcomeEmailTag
  IdpChangedEmail {} -> IdpChangedEmailTag
  ProviderActivationEmail {} -> ProviderActivationEmailTag
  ProviderApprovalConfirmEmail {} -> ProviderApprovalConfirmEmailTag
  ProviderPasswordResetEmail {} -> ProviderPasswordResetEmailTag
  EnterpriseAuditEmail {} -> EnterpriseAuditEmailTag

instance ToSchema SendEmailRequest where
  schema = object sendEmailRequestObjectSchema

-- | Common representation for all tagged job payload envelopes: the stable
-- @{\"type\": ..., \"data\": ...}@ shape. Defined here (not in
-- "Wire.API.Jobs") because Jobs imports this module; Jobs re-uses it for its
-- own payloads.
taggedJobPayloadObjectSchema ::
  forall tag payload.
  (Bounded tag, Enum tag, ToSchema tag) =>
  (payload -> tag) ->
  (tag -> ObjectSchema SwaggerDoc payload) ->
  ObjectSchema SwaggerDoc payload
taggedJobPayloadObjectSchema toTag toSchema =
  snd <$> (toTag &&& id) .= bind (fst .= tagObjectSchema) (snd .= dispatch toSchema)
  where
    tagObjectSchema :: ObjectSchema SwaggerDoc tag
    tagObjectSchema = field "type" schema

deriving via (Schema SendEmailRequest) instance Aeson.ToJSON SendEmailRequest

deriving via (Schema SendEmailRequest) instance Aeson.FromJSON SendEmailRequest

sendEmailRequestObjectSchema :: ObjectSchema SwaggerDoc SendEmailRequest
sendEmailRequestObjectSchema = taggedJobPayloadObjectSchema sendEmailRequestTag dataSchema
  where
    dataSchema :: SendEmailRequestTag -> ObjectSchema SwaggerDoc SendEmailRequest
    dataSchema = \case
      VerificationEmailTag -> tag _VerificationEmail (field "data" schema)
      ActivationEmailTag -> tag _ActivationEmail (field "data" schema)
      EmailAddressUpdateEmailTag -> tag _EmailAddressUpdateEmail (field "data" schema)
      TeamActivationEmailTag -> tag _TeamActivationEmail (field "data" schema)
      PasswordResetEmailTag -> tag _PasswordResetEmail (field "data" schema)
      NewClientEmailTag -> tag _NewClientEmail (field "data" schema)
      AccountDeletionEmailTag -> tag _AccountDeletionEmail (field "data" schema)
      LoginVerificationEmailTag -> tag _LoginVerificationEmail (field "data" schema)
      ScimTokenVerificationEmailTag -> tag _ScimTokenVerificationEmail (field "data" schema)
      TeamDeletionVerificationEmailTag -> tag _TeamDeletionVerificationEmail (field "data" schema)
      TeamInvitationEmailTag -> tag _TeamInvitationEmail (field "data" schema)
      TeamInvitationPersonalUserEmailTag -> tag _TeamInvitationPersonalUserEmail (field "data" schema)
      MemberWelcomeEmailTag -> tag _MemberWelcomeEmail (field "data" schema)
      NewTeamOwnerWelcomeEmailTag -> tag _NewTeamOwnerWelcomeEmail (field "data" schema)
      IdpChangedEmailTag -> tag _IdpChangedEmail (field "data" schema)
      ProviderActivationEmailTag -> tag _ProviderActivationEmail (field "data" schema)
      ProviderApprovalConfirmEmailTag -> tag _ProviderApprovalConfirmEmail (field "data" schema)
      ProviderPasswordResetEmailTag -> tag _ProviderPasswordResetEmail (field "data" schema)
      EnterpriseAuditEmailTag -> tag _EnterpriseAuditEmail (field "data" schema)

instance Arbitrary SendEmailRequest where
  arbitrary =
    oneof
      [ VerificationEmail <$> arbitrary,
        ActivationEmail <$> arbitrary,
        EmailAddressUpdateEmail <$> arbitrary,
        TeamActivationEmail <$> arbitrary,
        PasswordResetEmail <$> arbitrary,
        NewClientEmail <$> arbitrary,
        AccountDeletionEmail <$> arbitrary,
        LoginVerificationEmail <$> arbitrary,
        ScimTokenVerificationEmail <$> arbitrary,
        TeamDeletionVerificationEmail <$> arbitrary,
        TeamInvitationEmail <$> arbitrary,
        TeamInvitationPersonalUserEmail <$> arbitrary,
        MemberWelcomeEmail <$> arbitrary,
        NewTeamOwnerWelcomeEmail <$> arbitrary,
        IdpChangedEmail <$> arbitrary,
        ProviderActivationEmail <$> arbitrary,
        ProviderApprovalConfirmEmail <$> arbitrary,
        ProviderPasswordResetEmail <$> arbitrary,
        EnterpriseAuditEmail <$> arbitrary
      ]
