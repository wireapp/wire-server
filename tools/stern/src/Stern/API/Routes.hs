-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Stern.API.Routes
  ( SternAPI,
    SternAPIInternal,
    SwaggerDocsAPI,
    swaggerDocs,
    UserConnectionGroups (..),
    doubleMaybeToEither,
    RedirectToSwaggerDocsAPI,
  )
where

import Control.Lens
import Control.Monad.Trans.Except
import Data.Aeson qualified as A
import Data.Handle
import Data.Id
import Data.Kind
import Data.OpenApi qualified as S
import Data.Schema qualified as Schema
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai.Utilities
import Servant hiding (Handler, WithStatus (..), addHeader, respond)
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.OpenApi.Internal.Orphans ()
import Servant.Swagger.UI
import Stern.Types
import Wire.API.CustomBackend
import Wire.API.OAuth
import Wire.API.Routes.Internal.Brig.Connection (ConnectionStatus)
import Wire.API.Routes.Internal.Brig.EJPD qualified as EJPD
import Wire.API.Routes.Named
import Wire.API.SwaggerHelper (cleanupSwagger)
import Wire.API.Team.Feature
import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Search

----------------------------------------------------------------------
-- routing tables
type RedirectToSwaggerDocsAPI =
  Named "swagger-ui-redirect" (Get '[PlainText] NoContent)

type SternAPIInternal =
  Named
    "status"
    ( "i"
        :> "status"
        :> Get '[JSON] NoContent
    )

type SternAPI =
  Named
    "suspend-user"
    ( Summary "Suspends user with this ID"
        :> "users"
        :> Capture "uid" UserId
        :> "suspend"
        :> Post '[JSON] NoContent
    )
    :<|> Named
           "unsuspend-user"
           ( Summary "Unsuspends user with this ID"
               :> "users"
               :> Capture "uid" UserId
               :> "unsuspend"
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "get-users-by-email"
           ( Summary "Displays user's info given an email address"
               :> "users"
               :> "by-email"
               :> QueryParam' [Required, Strict, Description "Email address"] "email" Email
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-users-by-phone"
           ( Summary "Displays user's info given a phone number"
               :> "users"
               :> "by-phone"
               :> QueryParam' [Required, Strict, Description "Phone number"] "phone" Phone
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-users-by-ids"
           ( Summary "Displays active users info given a list of ids"
               :> "users"
               :> "by-ids"
               :> QueryParam' [Required, Strict, Description "List of IDs of the users, separated by comma"] "ids" [UserId]
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-users-by-handles"
           ( Summary "Displays active users info given a list of handles"
               :> "users"
               :> "by-handles"
               :> QueryParam' [Required, Strict, Description "List of Handles of the users, without '@', separated by comma"] "handles" [Handle]
               :> Get '[JSON] [UserAccount]
           )
    :<|> Named
           "get-user-connections"
           ( Summary "Displays user's connections"
               :> Description "[Deprecated] This is using API version V1 and will be removed in the future."
               :> "users"
               :> Capture "uid" UserId
               :> "connections"
               :> Get '[JSON] UserConnectionGroups
           )
    :<|> Named
           "get-users-connections"
           ( Summary "Displays connections of many users given a list of ids"
               :> "users"
               :> "connections"
               :> QueryParam' [Required, Strict, Description "List of IDs of the users, separated by comma"] "ids" [UserId]
               :> Get '[JSON] [ConnectionStatus]
           )
    :<|> Named
           "search-users"
           ( Summary "Search for users on behalf of"
               :> "users"
               :> Capture "uid" UserId
               :> "search"
               :> QueryParam' [Optional, Strict, Description "Search query (default \"\")"] "q" Text
               :> QueryParam' [Optional, Strict, Description "Number of results to return (min 1, max 100, default 10)"] "size" Int32
               :> Get '[JSON] (SearchResult Contact)
           )
    :<|> Named
           "revoke-identity"
           ( Summary "Revoke a verified user identity.  Specify exactly one of phone, email."
               :> Description
                    "Forcefully revokes a verified user identity. \
                    \WARNING: If the given identity is the only verified \
                    \user identity of an account, the account will be \
                    \deactivated (\"wireless\") and might thus become inaccessible. \
                    \If the given identity is not taken / verified, this is a no-op."
               :> "users"
               :> "revoke-identity"
               :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
               :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "put-email"
           ( Summary "Change a user's email address."
               :> Description "The new e-mail address must be verified before the change takes effect."
               :> "users"
               :> Capture "uid" UserId
               :> "email"
               :> Servant.ReqBody '[JSON] EmailUpdate
               :> Put '[JSON] NoContent
           )
    :<|> Named
           "put-phone"
           ( Summary "Change a user's phone number."
               :> Description "The new phone number must be verified before the change takes effect."
               :> "users"
               :> Capture "uid" UserId
               :> "phone"
               :> Servant.ReqBody '[JSON] PhoneUpdate
               :> Put '[JSON] NoContent
           )
    :<|> Named
           "delete-user"
           ( Summary "Delete a user (irrevocable!)"
               :> Description
                    "Email or Phone must match UserId's (to prevent copy/paste mistakes).  Use exactly one of the two query params."
               :> "users"
               :> Capture "uid" UserId
               :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
               :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
               :> Delete '[JSON] NoContent
           )
    :<|> Named
           "suspend-team"
           ( Summary "Suspend a team."
               :> "teams"
               :> Capture "tid" TeamId
               :> "suspend"
               :> Put '[JSON] NoContent
           )
    :<|> Named
           "unsuspend-team"
           ( Summary "Set a team status to 'Active', independently on previous status.  (Cannot be used to un-delete teams, though.)"
               :> "teams"
               :> Capture "tid" TeamId
               :> "unsuspend"
               :> Put '[JSON] NoContent
           )
    :<|> Named
           "delete-team"
           ( Summary "Delete a team (irrevocable!). You can only delete teams with 1 user unless you use the 'force' query flag"
               :> Description
                    "The email address of the user must be provided to prevent copy/paste mistakes.\n\
                    \The force query flag can be used to delete teams with more than one user. \
                    \CAUTION: FORCE DELETE WILL PERMANENTLY DELETE ALL TEAM MEMBERS! \
                    \CHECK TEAM MEMBER LIST (SEE ABOVE OR BELOW) IF YOU ARE UNCERTAIN THAT'S WHAT YOU WANT."
               :> "teams"
               :> Capture "tid" TeamId
               :> QueryParam' [Optional, Strict, Description "THIS WILL PERMANENTLY DELETE ALL TEAM MEMBERS! CHECK TEAM MEMBER LIST (SEE ABOVE OR BELOW) IF YOU ARE UNCERTAIN THAT'S WHAT YOU WANT."] "force" Bool
               :> QueryParam' [Optional, Strict, Description "Matching verified remaining user address"] "email" Email
               :> Delete '[JSON] NoContent
           )
    :<|> Named
           "ejpd-info"
           ( Summary "internal wire.com process: https://wearezeta.atlassian.net/wiki/spaces/~463749889/pages/256738296/EJPD+official+requests+process"
               :> "ejpd-info"
               :> QueryParam' [Optional, Strict, Description "If 'true', this gives you more more exhaustive information about this user (including social network)"] "include_contacts" Bool
               :> QueryParam' [Required, Strict, Description "Handles of the users, separated by commas (NB: all chars need to be lower case!)"] "handles" [Handle]
               :> Get '[JSON] EJPD.EJPDResponseBody
           )
    :<|> Named
           "head-user-blacklist"
           ( Summary "Fetch blacklist information on a email/phone (200: blacklisted; 404: not blacklisted)"
               :> "users"
               :> "blacklist"
               :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
               :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
               :> Verb 'GET 200 '[JSON] NoContent
           )
    :<|> Named
           "post-user-blacklist"
           ( Summary "Add the email/phone to our blacklist"
               :> "users"
               :> "blacklist"
               :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
               :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "delete-user-blacklist"
           ( Summary "Remove the email/phone from our blacklist"
               :> "users"
               :> "blacklist"
               :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
               :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
               :> Delete '[JSON] NoContent
           )
    :<|> Named
           "get-team-info-by-member-email"
           ( Summary "Fetch a team information given a member's email"
               :> "teams"
               :> QueryParam' [Required, Strict, Description "A verified email address"] "email" Email
               :> Get '[JSON] TeamInfo
           )
    :<|> Named
           "get-team-info"
           ( Summary "Gets information about a team"
               :> "teams"
               :> Capture "tid" TeamId
               :> Get '[JSON] TeamInfo
           )
    :<|> Named
           "get-team-admin-info"
           ( Summary "Gets information about a team's members, owners, and admins"
               :> "teams"
               :> Capture "tid" TeamId
               :> "admins"
               :> Get '[JSON] TeamAdminInfo
           )
    :<|> Named "get-route-legalhold-config" (MkFeatureGetRoute LegalholdConfig)
    :<|> Named "put-route-legalhold-config" (MkFeaturePutRouteTrivialConfigNoTTL LegalholdConfig)
    :<|> Named "get-route-sso-config" (MkFeatureGetRoute SSOConfig)
    :<|> Named "put-route-sso-config" (MkFeaturePutRouteTrivialConfigNoTTL SSOConfig)
    :<|> Named "get-route-search-visibility-available-config" (MkFeatureGetRoute SearchVisibilityAvailableConfig)
    :<|> Named "put-route-search-visibility-available-config" (MkFeaturePutRouteTrivialConfigNoTTL SearchVisibilityAvailableConfig)
    :<|> Named "get-route-validate-saml-emails-config" (MkFeatureGetRoute ValidateSAMLEmailsConfig)
    :<|> Named "put-route-validate-saml-emails-config" (MkFeaturePutRouteTrivialConfigNoTTL ValidateSAMLEmailsConfig)
    :<|> Named "get-route-digital-signatures-config" (MkFeatureGetRoute DigitalSignaturesConfig)
    :<|> Named "put-route-digital-signatures-config" (MkFeaturePutRouteTrivialConfigNoTTL DigitalSignaturesConfig)
    :<|> Named "get-route-file-sharing-config" (MkFeatureGetRoute FileSharingConfig)
    :<|> Named "put-route-file-sharing-config" (MkFeaturePutRouteTrivialConfigNoTTL FileSharingConfig)
    :<|> Named "get-route-classified-domains-config" (MkFeatureGetRoute ClassifiedDomainsConfig)
    :<|> Named "get-route-conference-calling-config" (MkFeatureGetRoute ConferenceCallingConfig)
    :<|> Named "put-route-conference-calling-config" (MkFeaturePutRouteTrivialConfigWithTTL ConferenceCallingConfig)
    :<|> Named "get-route-applock-config" (MkFeatureGetRoute AppLockConfig)
    :<|> Named "put-route-applock-config" (MkFeaturePutRoute AppLockConfig)
    :<|> Named "get-route-mls-config" (MkFeatureGetRoute MLSConfig)
    :<|> Named "put-route-mls-config" (MkFeaturePutRoute MLSConfig)
    :<|> Named
           "get-search-visibility"
           ( Summary "Shows the current TeamSearchVisibility value for the given team"
               :> Description
                    "These endpoints should be part of team settings. Until that happens, \
                    \we access them from here for authorized personnel to enable/disable \
                    \this on the team's behalf"
               :> "teams"
               :> Capture "tid" TeamId
               :> "search-visibility"
               :> Get '[JSON] TeamSearchVisibilityView
           )
    :<|> Named
           "put-search-visibility"
           ( Summary "Shows the current TeamSearchVisibility value for the given team"
               :> Description
                    "These endpoints should be part of team settings. Until that happens, \
                    \we access them from here for authorized personnel to enable/disable \
                    \this on the team's behalf"
               :> "teams"
               :> Capture "tid" TeamId
               :> "search-visibility"
               :> ReqBody '[JSON] TeamSearchVisibility
               :> Put '[JSON] NoContent
           )
    :<|> Named "get-route-outlook-cal-config" (MkFeatureGetRoute OutlookCalIntegrationConfig)
    :<|> Named "lock-unlock-route-outlook-cal-config" (MkFeatureLockUnlockRouteTrivialConfigNoTTL OutlookCalIntegrationConfig)
    :<|> Named "put-route-outlook-cal-config" (MkFeaturePutRouteTrivialConfigNoTTL OutlookCalIntegrationConfig)
    :<|> Named
           "get-route-enforce-file-download-location"
           ( Description
               "<p><b>Custom feature: only supported for some decidated on-prem systems.</b></p>"
               :> MkFeatureGetRoute EnforceFileDownloadLocationConfig
           )
    :<|> Named
           "lock-unlock-route-enforce-file-download-location"
           ( Description
               "<p><b>Custom feature: only supported for some decidated on-prem systems.</b></p>"
               :> MkFeatureLockUnlockRouteTrivialConfigNoTTL EnforceFileDownloadLocationConfig
           )
    :<|> Named
           "put-route-enforce-file-download-location"
           ( Description
               "<p><b>Custom feature: only supported for some dedicated on-prem systems.</b></p>"
               :> MkFeaturePutRoute EnforceFileDownloadLocationConfig
           )
    :<|> Named
           "get-team-invoice"
           ( Summary "Get a specific invoice by Number"
               :> Description "Relevant only internally at Wire"
               :> "teams"
               :> Capture "tid" TeamId
               :> "invoice"
               :> Capture "inr" InvoiceId
               :> Get '[JSON] Text
           )
    :<|> Named
           "get-team-billing-info"
           ( Summary "Gets billing information about a team"
               :> Description "Relevant only internally at Wire"
               :> "teams"
               :> Capture "tid" TeamId
               :> "billing"
               :> Get '[JSON] TeamBillingInfo
           )
    :<|> Named
           "put-team-billing-info"
           ( Summary "Updates billing information about a team. Non specified fields will NOT be updated"
               :> Description "Relevant only internally at Wire"
               :> "teams"
               :> Capture "tid" TeamId
               :> "billing"
               :> ReqBody '[JSON] TeamBillingInfoUpdate
               :> Put '[JSON] TeamBillingInfo
           )
    :<|> Named
           "post-team-billing-info"
           ( Summary
               "Sets billing information about a team. Can only be used on teams that do NOT have any \
               \billing information set. To update team billing info, use the update endpoint"
               :> Description "Relevant only internally at Wire"
               :> "teams"
               :> Capture "tid" TeamId
               :> "billing"
               :> ReqBody '[JSON] TeamBillingInfo
               :> Post '[JSON] TeamBillingInfo
           )
    :<|> Named
           "get-consent-log"
           ( Summary "Fetch the consent log given an email address of a non-user"
               :> Description "Relevant only internally at Wire"
               :> "i"
               :> "consent"
               :> QueryParam' [Required, Strict, Description "A verified email address"] "email" Email
               :> Get '[JSON] ConsentLogAndMarketo
           )
    :<|> Named
           "get-user-meta-info"
           ( Summary "Fetch a user's meta info given a user id: TEMPORARY!"
               :> Description "Relevant only internally at Wire"
               :> "i"
               :> "user"
               :> "meta-info"
               :> QueryParam' [Required, Strict, Description "A valid UserId"] "id" UserId
               :> QueryParam' [Optional, Strict, Description "Max number of conversation (default 1)"] "max_conversations" Int
               :> QueryParam' [Optional, Strict, Description "Max number of notifications (default 10)"] "max_notifications" Int
               :> Post '[JSON] UserMetaInfo
           )
    :<|> Named
           "get-sso-domain-redirect"
           ( Summary "read, update, delete domain login redirects custom backends (see https://docs.wire.com/understand/associate/custom-backend-for-desktop-client.html)"
               :> Description "Read from cassandra table galley.custom_backend."
               :> "sso-domain-redirect"
               :> QueryParam' [Required, Strict, Description "Domain"] "domain" Text
               :> Get '[JSON] (Maybe CustomBackend)
           )
    :<|> Named
           "put-sso-domain-redirect"
           ( Summary "read, update, delete domain login redirects custom backends (see https://docs.wire.com/understand/associate/custom-backend-for-desktop-client.html)"
               :> Description "Write to cassandra table galley.custom_backend."
               :> "sso-domain-redirect"
               :> QueryParam' [Required, Strict, Description "Domain key (from email during login)"] "domain" Text
               :> QueryParam' [Required, Strict, Description "Config JSON URL"] "configurl" Text
               :> QueryParam' [Required, Strict, Description "Webapp welcome URL"] "welcomeurl" Text
               :> Put '[JSON] ()
           )
    :<|> Named
           "delete-sso-domain-redirect"
           ( Summary "read, update, delete domain login redirects custom backends (see https://docs.wire.com/understand/associate/custom-backend-for-desktop-client.html)"
               :> Description "Delete from cassandra table galley.custom_backend."
               :> "sso-domain-redirect"
               :> QueryParam' [Required, Strict, Description "Domain key (from email during login)"] "domain" Text
               :> Delete '[JSON] ()
           )
    :<|> Named
           "register-oauth-client"
           ( Summary "Register an OAuth client"
               :> "i"
               :> "oauth"
               :> "clients"
               :> ReqBody '[JSON] OAuthClientConfig
               :> Post '[JSON] OAuthClientCredentials
           )
    :<|> Named
           "get-oauth-client"
           ( Summary "Get OAuth client by id"
               :> "i"
               :> "oauth"
               :> "clients"
               :> Capture "id" OAuthClientId
               :> Get '[JSON] OAuthClient
           )
    :<|> Named
           "update-oauth-client"
           ( Summary "Update OAuth client"
               :> "i"
               :> "oauth"
               :> "clients"
               :> Capture "id" OAuthClientId
               :> ReqBody '[JSON] OAuthClientConfig
               :> Put '[JSON] OAuthClient
           )
    :<|> Named
           "delete-oauth-client"
           ( Summary "Delete OAuth client"
               :> "i"
               :> "oauth"
               :> "clients"
               :> Capture "id" OAuthClientId
               :> Delete '[JSON] ()
           )

-------------------------------------------------------------------------------
-- Swagger

type SwaggerDocsAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDocs :: Servant.Server SwaggerDocsAPI
swaggerDocs =
  swaggerSchemaUIServer $
    toOpenApi (Proxy @SternAPI)
      & S.info . S.title .~ "Stern API"
      & cleanupSwagger

----------------------------------------------------------------------
-- helpers

data UserConnectionGroups = UserConnectionGroups
  { ucgAccepted :: Int,
    ucgSent :: Int,
    ucgPending :: Int,
    ucgBlocked :: Int,
    ucgIgnored :: Int,
    ucgMissingLegalholdConsent :: Int,
    ucgTotal :: Int
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema.Schema UserConnectionGroups

instance Schema.ToSchema UserConnectionGroups where
  schema =
    Schema.object "UserConnectionGroups" $
      UserConnectionGroups
        <$> ucgAccepted Schema..= Schema.field "ucgAccepted" Schema.schema
        <*> ucgSent Schema..= Schema.field "ucgSent" Schema.schema
        <*> ucgPending Schema..= Schema.field "ucgPending" Schema.schema
        <*> ucgBlocked Schema..= Schema.field "ucgBlocked" Schema.schema
        <*> ucgIgnored Schema..= Schema.field "ucgIgnored" Schema.schema
        <*> ucgMissingLegalholdConsent Schema..= Schema.field "ucgMissingLegalholdConsent" Schema.schema
        <*> ucgTotal Schema..= Schema.field "ucgTotal" Schema.schema

doubleMaybeToEither :: (Monad m) => LText -> Maybe a -> Maybe b -> ExceptT Error m (Either a b)
doubleMaybeToEither _ (Just a) Nothing = pure $ Left a
doubleMaybeToEither _ Nothing (Just b) = pure $ Right b
doubleMaybeToEither msg _ _ = throwE $ mkError status400 "either-params" ("Must use exactly one of two query params: " <> msg)

type MkFeatureGetRoute (feature :: Type) =
  Summary "Shows whether a feature flag is enabled or not for a given team."
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol feature
    :> Get '[JSON] (WithStatus feature)

type MkFeaturePutRouteTrivialConfigNoTTL (feature :: Type) =
  Summary "Disable / enable status for a given feature / team"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol feature
    :> QueryParam' [Required, Strict] "status" FeatureStatus
    :> Put '[JSON] NoContent

type MkFeaturePutRouteTrivialConfigWithTTL (feature :: Type) =
  Summary "Disable / enable status for a given feature / team"
    :> Description "team feature time to live, given in days, or 'unlimited' (default).  only available on *some* features!"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol feature
    :> QueryParam' [Required, Strict] "status" FeatureStatus
    :> QueryParam' [Required, Strict, Description "team feature time to live, given in days, or 'unlimited' (default)."] "ttl" FeatureTTLDays
    :> Put '[JSON] NoContent

type MkFeatureLockUnlockRouteTrivialConfigNoTTL (feature :: Type) =
  Summary "Lock / unlock status for a given feature / team (en-/disable should happen in team settings)"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol feature
    :> "lockOrUnlock"
    :> QueryParam' [Required, Strict] "lock-status" LockStatus
    :> Put '[JSON] NoContent

type MkFeaturePutRoute (feature :: Type) =
  Summary "Disable / enable feature flag for a given team"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol feature
    :> ReqBody '[JSON] (WithStatusNoLock feature)
    :> Put '[JSON] NoContent
