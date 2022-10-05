{-# OPTIONS_GHC -Wno-unused-imports #-}

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

-- | FUTUREWORK: remove this module once we don't depend on swagger1.2 for stern any more
module Stern.API.RoutesLegacy (apiDocs) where

import Brig.Types.Intra
import Control.Error
import Control.Lens
import Control.Monad.Trans.Except
import Data.Aeson hiding (Error, json)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (emptyArray)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.Handle
import Data.Id
import Data.Predicate
import Data.Proxy (Proxy (..))
import Data.Range
import qualified Data.Schema as S
import Data.String.Conversions (cs)
import Data.Swagger.Build.Api hiding (Response, def, min, response)
import qualified Data.Swagger.Build.Api as Doc
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import GHC.TypeLits (KnownSymbol)
import qualified Galley.Types.Teams.Intra as Team
import Imports hiding (head)
import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Predicate hiding (Error, reason, setStatus)
import Network.Wai.Routing hiding (trace)
import Network.Wai.Utilities
import qualified Network.Wai.Utilities.Server as Server
import Network.Wai.Utilities.Swagger (document, mkSwaggerApi)
import Stern.API.Predicates
import Stern.App
import qualified Stern.Intra as Intra
import Stern.Options
import qualified Stern.Swagger as Doc
import Stern.Types
import System.Logger.Class hiding (Error, name, trace, (.=))
import Util.Options
import Wire.API.Connection
import Wire.API.Routes.Internal.Brig.Connection (ConnectionStatus)
import qualified Wire.API.Routes.Internal.Brig.EJPD as EJPD
import Wire.API.Routes.Named
import Wire.API.SwaggerHelper (cleanupSwagger)
import Wire.API.Team.Feature hiding (setStatus)
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.SearchVisibility
import qualified Wire.API.Team.SearchVisibility as Public
import Wire.API.User
import Wire.API.User.Search
import qualified Wire.Swagger as Doc

apiDocs :: ByteString -> Value
apiDocs url = toJSON $ mkSwaggerApi (decodeLatin1 url) Doc.sternModels routes

routes :: Routes Doc.ApiBuilder Handler ()
routes = do
  post "/users/:uid/suspend" (continue suspendUser') $
    capture "uid"
  document "POST" "users/:uid/suspend" $ do
    Doc.summary "Suspends user with this ID"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.response 200 "User successfully suspended" Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
    Doc.response 404 "Account not found" (Doc.model Doc.errorModel)

  post "/users/:uid/unsuspend" (continue unsuspendUser') $
    capture "uid"
  document "POST" "users/:uid/unsuspend" $ do
    Doc.summary "Unsuspends user with this ID"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.response 200 "User successfully unsuspended" Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
    Doc.response 404 "Account not found" (Doc.model Doc.errorModel)

  get "/users" (continue usersByEmail') $
    param "email"
  document "GET" "users/by-email" $ do
    Doc.summary "Displays user's info given an email address"
    Doc.parameter Doc.Query "email" Doc.string' $
      Doc.description "Email address"
    Doc.response 200 "List of users" Doc.end

  get
    "/users"
    (continue usersByPhone')
    phoneParam
  document "GET" "users/by-phone" $ do
    Doc.summary "Displays user's info given a phone number"
    Doc.parameter Doc.Query "phone" Doc.string' $
      Doc.description "Phone number"
    Doc.response 200 "List of users" Doc.end

  get "/users" (continue usersByIds') $
    param "ids"
  document "GET" "users/by-ids" $ do
    Doc.summary "Displays active users info given a list of ids"
    Doc.parameter Doc.Query "ids" Doc.string' $
      Doc.description "ID of the user"
    Doc.response 200 "List of users" Doc.end

  get "/users" (continue usersByHandles') $
    param "handles"
  document "GET" "users/by-handles" $ do
    Doc.summary "Displays active users info given a list of handles"
    Doc.parameter Doc.Query "handles" Doc.string' $
      Doc.description "Handle of the user"
    Doc.response 200 "List of users" Doc.end

  get "/users/:uid/connections" (continue userConnections') $
    capture "uid"
  document "GET" "users/:uid/connections" $ do
    Doc.summary "Displays user's connections"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      description "User ID"
    Doc.response 200 "List of user's connections" Doc.end

  get "/users/connections" (continue usersConnections') $
    param "ids"
  document "GET" "users/connections" $ do
    Doc.summary "Displays users connections given a list of ids"
    Doc.parameter Doc.Query "ids" Doc.string' $
      Doc.description "IDs of the users"
    Doc.response 200 "List of users connections" Doc.end

  get "/users/:uid/search" (continue searchOnBehalf') $
    capture "uid"
      .&. def "" (query "q")
      .&. def (unsafeRange 10) (query "size")
  document "GET" "search" $ do
    summary "Search for users on behalf of"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      description "User ID"
    Doc.parameter Query "q" string' $ do
      description "Search query"
      optional
    Doc.parameter Query "size" int32' $ do
      description "Number of results to return"
      optional
    Doc.response 200 "List of users" Doc.end

  post "/users/revoke-identity" (continue revokeIdentity') $
    param "email" ||| phoneParam
  document "POST" "revokeIdentity" $ do
    Doc.summary "Revoke a verified user identity."
    Doc.notes
      "Forcefully revokes a verified user identity. \
      \WARNING: If the given identity is the only verified \
      \user identity of an account, the account will be \
      \deactivated (\"wireless\") and might thus become inaccessible. \
      \If the given identity is not taken / verified, this is a no-op."
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "A verified email address"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "A verified phone number (E.164 format)."
      Doc.optional
    Doc.response 200 "Identity revoked or not verified / taken." Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)

  put "/users/:uid/email" (continue changeEmail') $
    contentType "application" "json"
      .&. capture "uid"
      .&. def False (query "validate")
      .&. jsonRequest @EmailUpdate
  document "PUT" "changeEmail" $ do
    Doc.summary "Change a user's email address."
    Doc.notes
      "The new e-mail address must be verified \
      \before the change takes effect."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.parameter Doc.Query "validate" Doc.bool' $ do
      Doc.description "If set to true, a validation email will be sent to the new email address"
      Doc.optional
    Doc.body (Doc.ref Doc.emailUpdate) $
      Doc.description "JSON body"
    Doc.response 200 "Change of email address initiated." Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)

  put "/users/:uid/phone" (continue changePhone') $
    contentType "application" "json"
      .&. capture "uid"
      .&. jsonRequest @PhoneUpdate
  document "PUT" "changePhone" $ do
    Doc.summary "Change a user's phone number."
    Doc.notes
      "The new phone number must be verified \
      \before the change takes effect."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.body (Doc.ref Doc.phoneUpdate) $
      Doc.description "JSON body"
    Doc.response 200 "Change of phone number initiated." Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)

  delete "/users/:uid" (continue deleteUser') $
    capture "uid"
      .&. (query "email" ||| phoneParam)
  document "DELETE" "deleteUser" $ do
    summary "Delete a user (irrevocable!)"
    Doc.notes "Email or Phone must match UserId's (to prevent copy/paste mistakes)"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      description "User ID"
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "Matching verified email address"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "Matching verified phone number (E.164 format)."
      Doc.optional
    Doc.response 200 "Account deleted" Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)

  put "/teams/:tid/suspend" (continue (setTeamStatusH' Team.Suspended)) $
    capture "tid"
  document "PUT" "setTeamStatusH:suspended" $ do
    summary "Suspend a team."
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.response 200 mempty Doc.end

  put "/teams/:tid/unsuspend" (continue (setTeamStatusH' Team.Active)) $
    capture "tid"
  document "PUT" "setTeamStatusH:active" $ do
    summary "Set a team status to 'Active', independently on previous status.  (Cannot be used to un-delete teams, though.)"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.response 200 mempty Doc.end

  delete "/teams/:tid" (continue deleteTeam') $
    capture "tid"
      .&. def False (query "force")
      .&. opt (query "email")
  document "DELETE" "deleteTeam" $ do
    summary "Delete a team (irrevocable!). You can only delete teams with 1 user unless you use the 'force' query flag"
    Doc.notes
      "The email address of the user must be provided to prevent copy/paste mistakes.\n\
      \The force query flag can be used to delete teams with more than one user. CAUTION: FORCE DELETE WILL PERMANENTLY DELETE ALL TEAM MEMBERS! CHECK TEAM MEMBER LIST (SEE ABOVE OR BELOW) IF YOU ARE UNCERTAIN THAT'S WHAT YOU WANT."
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.parameter Doc.Query "force" Doc.bool' $ do
      Doc.description "THIS WILL PERMANENTLY DELETE ALL TEAM MEMBERS! CHECK TEAM MEMBER LIST (SEE ABOVE OR BELOW) IF YOU ARE UNCERTAIN THAT'S WHAT YOU WANT."
      optional
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "Matching verified remaining user address"
      Doc.optional
    Doc.response 202 "Team scheduled for deletion" Doc.end
    Doc.response 404 "No such user with that email" (Doc.model Doc.errorModel)
    Doc.response 404 "No such binding team" (Doc.model Doc.errorModel)
    Doc.response 403 "Only teams with 1 user can be deleted" (Doc.model Doc.errorModel)
    Doc.response 404 "Binding team mismatch" (Doc.model Doc.errorModel)

  get "/ejpd-info" (continue ejpdInfoByHandles') $
    param "handles"
      .&. def False (query "include_contacts")
  document "GET" "ejpd-info" $ do
    Doc.summary "internal wire.com process: https://wearezeta.atlassian.net/wiki/spaces/~463749889/pages/256738296/EJPD+official+requests+process"
    Doc.parameter Doc.Query "handles" Doc.string' $
      Doc.description "Handles of the user, separated by commas (NB: all chars need to be lower case!)"
    Doc.parameter Doc.Query "include_contacts" Doc.bool' $ do
      Doc.description "If 'true', this gives you more more exhaustive information about this user (including social network)"
      Doc.optional
    Doc.response 200 "Required information about the listed users (where found)" Doc.end

  head "/users/blacklist" (continue isUserKeyBlacklisted) $
    (query "email" ||| phoneParam)
  document "HEAD" "checkBlacklistStatus" $ do
    summary "Fetch blacklist information on a email/phone"
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "An email address to check"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "A phone to check"
      Doc.optional
    Doc.response 200 "The email/phone IS blacklisted" Doc.end
    Doc.response 404 "The email/phone is NOT blacklisted" Doc.end

  post "/users/blacklist" (continue addBlacklist) $
    (query "email" ||| phoneParam)
  document "POST" "addToBlacklist" $ do
    summary "Add the email/phone to our blacklist"
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "An email address to add"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "A phone to add"
      Doc.optional
    Doc.response 200 "Operation succeeded" Doc.end

  delete "/users/blacklist" (continue deleteFromBlacklist) $
    (query "email" ||| phoneParam)
  document "DELETE" "deleteFromBlacklist" $ do
    summary "Remove the email/phone from our blacklist"
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "An email address to remove"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "A phone to remove"
      Doc.optional
    Doc.response 200 "Operation succeeded" Doc.end

  get "/teams" (continue getTeamInfoByMemberEmail) $
    param "email"
  document "GET" "getTeamInfoByMemberEmail" $ do
    summary "Fetch a team information given a member's email"
    Doc.parameter Doc.Query "email" Doc.string' $
      Doc.description "A verified email address"
    Doc.response 200 "Team Information" Doc.end

  get "/teams/:tid" (continue getTeamInfo) $
    capture "tid"
  document "GET" "getTeamInfo" $ do
    summary "Gets information about a team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.response 200 "Team Information" Doc.end

  get "/teams/:tid/admins" (continue getTeamAdminInfo) $
    capture "tid"
  document "GET" "getTeamAdminInfo" $ do
    summary "Gets information about a team's owners and admins only"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.response 200 "Team Information about Owners and Admins" Doc.end

  mkFeatureGetRoute @LegalholdConfig
  mkFeaturePutRouteTrivialConfig @LegalholdConfig

  mkFeatureGetRoute @SSOConfig
  mkFeaturePutRouteTrivialConfig @SSOConfig

  mkFeatureGetRoute @SearchVisibilityAvailableConfig
  mkFeaturePutRouteTrivialConfig @SearchVisibilityAvailableConfig

  mkFeatureGetRoute @ValidateSAMLEmailsConfig
  mkFeaturePutRouteTrivialConfig @ValidateSAMLEmailsConfig

  mkFeatureGetRoute @DigitalSignaturesConfig
  mkFeaturePutRouteTrivialConfig @DigitalSignaturesConfig

  mkFeatureGetRoute @FileSharingConfig
  mkFeaturePutRouteTrivialConfig @FileSharingConfig

  mkFeatureGetRoute @ClassifiedDomainsConfig

  mkFeatureGetRoute @ConferenceCallingConfig
  mkFeaturePutRouteTrivialConfig' @ConferenceCallingConfig TtlEnabled

  mkFeatureGetRoute @AppLockConfig
  mkFeaturePutRoute @AppLockConfig

  mkFeatureGetRoute @MLSConfig
  mkFeaturePutRoute @MLSConfig

  -- These endpoints should be part of team settings. Until then, we access them from here
  -- for authorized personnel to enable/disable this on the team's behalf
  get "/teams/:tid/search-visibility" (continue (fmap json . Intra.getSearchVisibility)) $
    capture "tid"
  document "GET" "getSearchVisibility" $ do
    summary "Shows the current TeamSearchVisibility value for the given team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.returns (Doc.ref Public.modelTeamSearchVisibility)
    Doc.response 200 "TeamSearchVisibility value" Doc.end
  put "/teams/:tid/search-visibility" (continue setSearchVisibility) $
    contentType "application" "json"
      .&. capture "tid"
      .&. jsonRequest @TeamSearchVisibility
  document "PUT" "setSearchVisibility" $ do
    summary "Set specific search visibility for the team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.body Public.typeSearchVisibility $
      Doc.description "JSON body"
    Doc.response 200 "TeamSearchVisibility status set" Doc.end

  -- The following endpoint are only relevant internally at Wire

  get "/teams/:tid/invoices/:inr" (continue getTeamInvoice) $
    capture "tid"
      .&. capture "inr"
      .&. accept "application" "json"
  document "GET" "getTeamInvoice" $ do
    summary "Get a specific invoice by Number"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      Doc.description "Team ID"
    Doc.parameter Doc.Path "inr" Doc.string' $
      Doc.description "Invoice Number"
    Doc.response 307 "Redirect to PDF download" Doc.end

  get "/teams/:tid/billing" (continue getTeamBillingInfo) $
    capture "tid"
  document "GET" "getTeamBillingInfo" $ do
    summary "Gets billing information about a team"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.response 200 "Team Billing Information" Doc.end
    Doc.response 404 "No team or no billing info for given team" Doc.end
    Doc.returns (Doc.ref Doc.teamBillingInfo)

  put "/teams/:tid/billing" (continue updateTeamBillingInfo) $
    contentType "application" "json"
      .&. capture "tid"
      .&. jsonRequest @TeamBillingInfoUpdate
  document "PUT" "updateTeamBillingInfo" $ do
    summary
      "Updates billing information about a team. Non \
      \specified fields will NOT be updated"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.body (Doc.ref Doc.teamBillingInfoUpdate) $
      Doc.description "JSON body"
    Doc.response 200 "Updated Team Billing Information" Doc.end
    Doc.returns (Doc.ref Doc.teamBillingInfo)

  post "/teams/:tid/billing" (continue setTeamBillingInfo) $
    contentType "application" "json"
      .&. capture "tid"
      .&. jsonRequest @TeamBillingInfo
  document "POST" "setTeamBillingInfo" $ do
    summary
      "Sets billing information about a team. Can \
      \only be used on teams that do NOT have any \
      \billing information set. To update team billing \
      \info, use the update endpoint"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.body (Doc.ref Doc.teamBillingInfo) $
      Doc.description "JSON body"
    Doc.response 200 "Updated Team Billing Information" Doc.end
    Doc.returns (Doc.ref Doc.teamBillingInfo)

  get "/i/consent" (continue getConsentLog) $
    param "email"
  document "GET" "getConsentLog" $ do
    summary "Fetch the consent log given an email address of a non-user"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Query "email" Doc.string' $
      Doc.description "An email address"
    Doc.response 200 "Consent Log" Doc.end
    Doc.response 403 "Access denied! There is a user with this email address" Doc.end

  get "/i/user/meta-info" (continue getUserData) $
    param "id"
  document "GET" "getUserMetaInfo" $ do
    summary "Fetch a user's meta info given a user id: TEMPORARY!"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Query "id" Doc.bytes' $
      Doc.description "A user's ID"
    Doc.response 200 "Meta Info" Doc.end

suspendUser' :: UserId -> Handler Response
suspendUser' = undefined

unsuspendUser' :: UserId -> Handler Response
unsuspendUser' = undefined

usersByEmail' :: Email -> Handler Response
usersByEmail' = undefined

usersByPhone' :: Phone -> Handler Response
usersByPhone' = undefined

usersByIds' :: List UserId -> Handler Response
usersByIds' = undefined

usersByHandles' :: List Handle -> Handler Response
usersByHandles' = undefined

ejpdInfoByHandles' :: (List Handle ::: Bool) -> Handler Response
ejpdInfoByHandles' = undefined

userConnections' :: UserId -> Handler Response
userConnections' = undefined

usersConnections' :: List UserId -> Handler Response
usersConnections' = undefined

searchOnBehalf' :: UserId ::: T.Text ::: Range 1 100 Int32 -> Handler Response
searchOnBehalf' = undefined

revokeIdentity' :: Either Email Phone -> Handler Response
revokeIdentity' = undefined

changeEmail' :: JSON ::: UserId ::: Bool ::: JsonRequest EmailUpdate -> Handler Response
changeEmail' = undefined

changePhone' :: JSON ::: UserId ::: JsonRequest PhoneUpdate -> Handler Response
changePhone' = undefined

deleteUser' :: UserId ::: Either Email Phone -> Handler Response
deleteUser' = undefined

setTeamStatusH' :: Team.TeamStatus -> TeamId -> Handler Response
setTeamStatusH' = undefined

deleteTeam' :: TeamId ::: Bool ::: Maybe Email -> Handler Response
deleteTeam' = undefined

isUserKeyBlacklisted :: Either Email Phone -> Handler Response
isUserKeyBlacklisted = undefined

addBlacklist :: Either Email Phone -> Handler Response
addBlacklist = undefined

deleteFromBlacklist :: Either Email Phone -> Handler Response
deleteFromBlacklist = undefined

getTeamInfo :: TeamId -> Handler Response
getTeamInfo = undefined

getTeamAdminInfo :: TeamId -> Handler Response
getTeamAdminInfo = undefined

setSearchVisibility :: JSON ::: TeamId ::: JsonRequest TeamSearchVisibility -> Handler Response
setSearchVisibility = undefined

getTeamBillingInfo :: TeamId -> Handler Response
getTeamBillingInfo = undefined

updateTeamBillingInfo :: JSON ::: TeamId ::: JsonRequest TeamBillingInfoUpdate -> Handler Response
updateTeamBillingInfo = undefined

setTeamBillingInfo :: JSON ::: TeamId ::: JsonRequest TeamBillingInfo -> Handler Response
setTeamBillingInfo = undefined

getTeamInfoByMemberEmail :: Email -> Handler Response
getTeamInfoByMemberEmail = undefined

getTeamInvoice :: TeamId ::: InvoiceId ::: JSON -> Handler Response
getTeamInvoice = undefined

getConsentLog :: Email -> Handler Response
getConsentLog = undefined

getUserData :: UserId -> Handler Response
getUserData = undefined

data SupportsTtl = TtlEnabled | TtlDisabled

type JSON = Media "application" "json"

mkFeatureGetRoute ::
  forall cfg.
  ( IsFeatureConfig cfg,
    S.ToSchema cfg,
    KnownSymbol (FeatureSymbol cfg),
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  Routes Doc.ApiBuilder Handler ()
mkFeatureGetRoute = do
  get ("/teams/:tid/features/" <> featureNameBS @cfg) (continue (getTeamFeatureFlagH @cfg)) $
    capture "tid"
  document "GET" "getTeamFeatureFlag" $ do
    summary "Shows whether a feature flag is enabled or not for a given team."
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.returns (Doc.ref (withStatusModel @cfg))
    Doc.response 200 "Team feature flag status" Doc.end

mkFeaturePutRoute ::
  forall cfg.
  ( IsFeatureConfig cfg,
    S.ToSchema cfg,
    KnownSymbol (FeatureSymbol cfg),
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  Routes Doc.ApiBuilder Handler ()
mkFeaturePutRoute = do
  put ("/teams/:tid/features/" <> featureNameBS @cfg) (continue (setTeamFeatureFlagH @cfg)) $
    capture "tid"
      .&. jsonRequest @(WithStatusNoLock cfg)
      .&. accept "application" "json"
  document "PUT" "setTeamFeatureFlag" $ do
    summary "Disable / enable feature flag for a given team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.body (Doc.ref (withStatusNoLockModel @cfg)) $
      Doc.description "JSON body"
    Doc.response 200 "Team feature flag status" Doc.end

getTeamFeatureFlagH ::
  forall cfg.
  ( IsFeatureConfig cfg,
    S.ToSchema cfg,
    KnownSymbol (FeatureSymbol cfg),
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  TeamId ->
  Handler Response
getTeamFeatureFlagH = undefined

setTeamFeatureFlagH ::
  forall cfg.
  ( IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable (WithStatusNoLock cfg)
  ) =>
  TeamId ::: JsonRequest (WithStatusNoLock cfg) ::: JSON ->
  Handler Response
setTeamFeatureFlagH = undefined

mkFeaturePutRouteTrivialConfig ::
  forall cfg.
  ( IsFeatureConfig cfg,
    FeatureTrivialConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    S.ToSchema cfg,
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  Routes Doc.ApiBuilder Handler ()
mkFeaturePutRouteTrivialConfig = undefined

mkFeaturePutRouteTrivialConfig' ::
  forall cfg.
  ( IsFeatureConfig cfg,
    FeatureTrivialConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    S.ToSchema cfg,
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  SupportsTtl ->
  Routes Doc.ApiBuilder Handler ()
mkFeaturePutRouteTrivialConfig' ttlSupport = do
  handler
  document "PUT" "setTeamFeatureFlag" $ do
    summary "Disable / enable feature flag for a given team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.parameter Doc.Query "status" typeFeatureStatus $ do
      Doc.description "team feature status (enabled or disabled)"
    case ttlSupport of
      TtlEnabled -> Doc.parameter Doc.Query "ttl" Public.typeFeatureTTL $ do
        Doc.description "team feature time to live, given in days, or 'unlimited' (default). Only applies to conference calling. It's ignored by other features."
      TtlDisabled -> pure ()
    Doc.response 200 "Team feature flag status" Doc.end
  where
    handler = case ttlSupport of
      TtlEnabled ->
        put ("/teams/:tid/features/" <> featureNameBS @cfg) (continue (setTeamFeatureFlagTrivialConfigH @cfg)) $
          capture "tid"
            .&. param "status"
            .&. def Public.FeatureTTLUnlimited (query "ttl")
      TtlDisabled ->
        put ("/teams/:tid/features/" <> featureNameBS @cfg) (continue (setTeamFeatureFlagTrivialConfigHNoTtl @cfg)) $
          capture "tid"
            .&. param "status"

setTeamFeatureFlagTrivialConfigHNoTtl ::
  forall cfg.
  ( IsFeatureConfig cfg,
    FeatureTrivialConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    S.ToSchema cfg,
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  TeamId ::: FeatureStatus ->
  Handler Response
setTeamFeatureFlagTrivialConfigHNoTtl = undefined

setTeamFeatureFlagTrivialConfigH ::
  forall cfg.
  ( IsFeatureConfig cfg,
    FeatureTrivialConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    S.ToSchema cfg,
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  TeamId ::: FeatureStatus ::: FeatureTTL' 'FeatureTTLUnitDays ->
  Handler Response
setTeamFeatureFlagTrivialConfigH = undefined
