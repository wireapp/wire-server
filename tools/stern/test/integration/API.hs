{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module API where -- todo(leif): export only test

import Bilge
import Brig.Types.Intra
import Control.Applicative
import Control.Lens hiding ((.=))
import Data.ByteString.Conversion
import Data.Handle
import Data.Id
import Data.Schema
import qualified Data.Set as Set
import Data.String.Conversions
import GHC.TypeLits
import Imports
import Stern.API.Routes (UserConnectionGroups (..))
import Stern.Types
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Util
import Wire.API.Routes.Internal.Brig.Connection
import qualified Wire.API.Routes.Internal.Brig.EJPD as EJPD
import Wire.API.Team.Feature
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Search

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API tests"
    [ test s "GET /i/status" testGetStatus,
      test s "POST /users/:uid/suspend" testSuspendUser,
      test s "POST /users/:uid/unsuspend" testUnsuspendUser,
      test s "GET /users/by-email" testGetUsersByEmail,
      test s "GET /users/by-phone" testGetUsersByPhone,
      test s "GET /users/by-ids" testGetUsersByIds,
      test s "GET /users/by-handles" testGetUsersByHandles,
      test s "GET /users/:id/connections" testGetConnections,
      test s "GET /users/connections?ids=..." testGetConnectionsByIds,
      test s "GET /users/:uid/search" testSearchUsers,
      test s "POST /users/revoke-identity?email=..." testRevokeIdentity,
      test s "PUT /users/:uid/email" testPutEmail,
      -- -- :<|> Named
      -- --        "put-phone"
      -- --        ( Summary "Change a user's phone number."
      -- --            :> Description "The new phone number must be verified before the change takes effect."
      -- --            :> "users"
      -- --            :> Capture "uid" UserId
      -- --            :> "phone"
      -- --            :> Servant.ReqBody '[JSON] PhoneUpdate
      -- --            :> Put '[JSON] NoContent
      -- --        )
      -- putPhone :: UserId -> PhoneUpdate -> TestM ()
      -- putPhone uid phoneUpdate = do
      --   s <- view tsStern
      --   void $ put (s . paths ["users", toByteString' uid, "phone"] . json phoneUpdate . expect2xx)

      -- -- :<|> Named
      -- --        "delete-user"
      -- --        ( Summary "Delete a user (irrevocable!)"
      -- --            :> Description
      -- --                 "Email or Phone must match UserId's (to prevent copy/paste mistakes).  Use exactly one of the two query params."
      -- --            :> "users"
      -- --            :> Capture "uid" UserId
      -- --            :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
      -- --            :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
      -- --            :> Delete '[JSON] NoContent
      -- --        )
      -- deleteUser :: UserId -> Either Email Phone -> TestM ()
      -- deleteUser uid emailOrPhone = do
      --   s <- view tsStern
      --   void $ delete (s . paths ["users", toByteString' uid] . mkQueryParam emailOrPhone . expect2xx)

      -- -- :<|> Named
      -- --        "suspend-team"
      -- --        ( Summary "Suspend a team."
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> "suspend"
      -- --            :> Put '[JSON] NoContent
      -- --        )
      -- suspendTeam :: TeamId -> TestM ()
      -- suspendTeam tid = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "suspend"] . expect2xx)

      -- -- :<|> Named
      -- --        "unsuspend-team"
      -- --        ( Summary "Set a team status to 'Active', independently on previous status.  (Cannot be used to un-delete teams, though.)"
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> "unsuspend"
      -- --            :> Put '[JSON] NoContent
      -- --        )
      -- unsuspendTeam :: TeamId -> TestM ()
      -- unsuspendTeam tid = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "unsuspend"] . expect2xx)

      -- -- :<|> Named
      -- --        "delete-team"
      -- --        ( Summary "Delete a team (irrevocable!). You can only delete teams with 1 user unless you use the 'force' query flag"
      -- --            :> Description
      -- --                 "The email address of the user must be provided to prevent copy/paste mistakes.\n\
      -- --                 \The force query flag can be used to delete teams with more than one user. \
      -- --                 \CAUTION: FORCE DELETE WILL PERMANENTLY DELETE ALL TEAM MEMBERS! \
      -- --                 \CHECK TEAM MEMBER LIST (SEE ABOVE OR BELOW) IF YOU ARE UNCERTAIN THAT'S WHAT YOU WANT."
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> QueryParam' [Optional, Strict, Description "THIS WILL PERMANENTLY DELETE ALL TEAM MEMBERS! CHECK TEAM MEMBER LIST (SEE ABOVE OR BELOW) IF YOU ARE UNCERTAIN THAT'S WHAT YOU WANT."] "force" Bool
      -- --            :> QueryParam' [Optional, Strict, Description "Matching verified remaining user address"] "email" Email
      -- --            :> Delete '[JSON] NoContent
      -- --        )
      -- deleteTeam :: TeamId -> Bool -> Email -> TestM ()
      -- deleteTeam tid force email = do
      --   s <- view tsStern
      --   void $ delete (s . paths ["teams", toByteString' tid] . queryItem "force" (toByteString' force) . queryItem "email" (toByteString' email) . expect2xx)

      -- -- :<|> Named
      -- --        "ejpd-info"
      -- --        ( Summary "internal wire.com process: https://wearezeta.atlassian.net/wiki/spaces/~463749889/pages/256738296/EJPD+official+requests+process"
      -- --            :> "ejpd-info"
      -- --            :> QueryParam' [Optional, Strict, Description "If 'true', this gives you more more exhaustive information about this user (including social network)"] "include_contacts" Bool
      -- --            :> QueryParam' [Required, Strict, Description "Handles of the users, separated by commas (NB: all chars need to be lower case!)"] "handles" [Handle]
      -- --            :> Get '[JSON] EJPD.EJPDResponseBody
      -- --        )
      -- ejpdInfo :: Bool -> [Handle] -> TestM EJPD.EJPDResponseBody
      -- ejpdInfo includeContacts handles = do
      --   s <- view tsStern
      --   r <- get (s . paths ["ejpd-info"] . queryItem "include_contacts" (toByteString' includeContacts) . queryItem "handles" (toByteString' handles) . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named
      -- --        "head-user-blacklist"
      -- --        ( Summary "Fetch blacklist information on a email/phone (200: blacklisted; 404: not blacklisted)"
      -- --            :> "users"
      -- --            :> "blacklist"
      -- --            :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
      -- --            :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
      -- --            :> Verb 'HEAD 200 '[JSON] NoContent
      -- --        )
      -- userBlacklistHead :: Either Email Phone -> TestM ()
      -- userBlacklistHead emailOrPhone = do
      --   s <- view tsStern
      --   void $ Bilge.head (s . paths ["users", "blacklist"] . mkQueryParam emailOrPhone . expect2xx)

      -- -- :<|> Named
      -- --        "post-user-blacklist"
      -- --        ( Summary "Add the email/phone to our blacklist"
      -- --            :> "users"
      -- --            :> "blacklist"
      -- --            :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
      -- --            :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
      -- --            :> Post '[JSON] NoContent
      -- --        )
      -- postUserBlacklist :: Either Email Phone -> TestM ()
      -- postUserBlacklist emailOrPhone = do
      --   s <- view tsStern
      --   void $ post (s . paths ["users", "blacklist"] . mkQueryParam emailOrPhone . expect2xx)

      -- -- :<|> Named
      -- --        "delete-user-blacklist"
      -- --        ( Summary "Remove the email/phone from our blacklist"
      -- --            :> "users"
      -- --            :> "blacklist"
      -- --            :> QueryParam' [Optional, Strict, Description "A verified email address"] "email" Email
      -- --            :> QueryParam' [Optional, Strict, Description "A verified phone number (E.164 format)."] "phone" Phone
      -- --            :> Delete '[JSON] NoContent
      -- --        )
      -- deleteUserBlacklist :: Either Email Phone -> TestM ()
      -- deleteUserBlacklist emailOrPhone = do
      --   s <- view tsStern
      --   void $ delete (s . paths ["users", "blacklist"] . mkQueryParam emailOrPhone . expect2xx)

      -- -- :<|> Named
      -- --        "get-team-info-by-member-email"
      -- --        ( Summary "Fetch a team information given a member's email"
      -- --            :> "teams"
      -- --            :> QueryParam' [Required, Strict, Description "A verified email address"] "email" Email
      -- --            :> Get '[JSON] TeamInfo
      -- --        )
      -- getTeamInfoByMemberEmail :: Email -> TestM TeamInfo
      -- getTeamInfoByMemberEmail email = do
      --   s <- view tsStern
      --   r <- get (s . paths ["teams"] . queryItem "email" (toByteString' email) . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named
      -- --        "get-team-admin-info"
      -- --        ( Summary "Gets information about a team's members, owners, and admins"
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> "admins"
      -- --            :> Get '[JSON] TeamAdminInfo
      -- --        )
      -- getTeamAdminInfo :: TeamId -> TestM TeamAdminInfo
      -- getTeamAdminInfo tid = do
      --   s <- view tsStern
      --   r <- get (s . paths ["teams", toByteString' tid, "admins"] . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named "get-route-legalhold-config" (MkFeatureGetRoute LegalholdConfig)
      -- getFeatureConfig ::
      --   forall cfg.
      --   ( KnownSymbol (FeatureSymbol cfg),
      --     ToSchema cfg,
      --     Typeable cfg,
      --     IsFeatureConfig cfg
      --   ) =>
      --   TeamId ->
      --   TestM (WithStatus cfg)
      -- getFeatureConfig tid = do
      --   s <- view tsStern
      --   r <- get (s . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg] . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named "put-route-legalhold-config" (MkFeaturePutRouteTrivialConfigNoTTL LegalholdConfig)
      -- putLegalholdConfig :: TeamId -> FeatureStatus -> TestM ()
      -- putLegalholdConfig tid status = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "features", "legalhold"] . queryItem "status" (toByteString' status) . expect2xx)

      -- -- :<|> Named "get-route-sso-config" (MkFeatureGetRoute SSOConfig)
      -- getSSOConfig :: TeamId -> TestM (WithStatus SSOConfig)
      -- getSSOConfig tid = do
      --   s <- view tsStern
      --   r <- get (s . paths ["teams", toByteString' tid, "features", "sso"] . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named "put-route-sso-config" (MkFeaturePutRouteTrivialConfigNoTTL SSOConfig)
      -- putSSOConfig :: TeamId -> FeatureStatus -> TestM ()
      -- putSSOConfig tid cfg = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "features", "sso"] . queryItem "status" (toByteString' cfg) . expect2xx)

      -- -- :<|> Named "put-route-search-visibility-available-config" (MkFeaturePutRouteTrivialConfigNoTTL SearchVisibilityAvailableConfig)
      -- putSearchVisibilityAvailableConfig :: TeamId -> FeatureStatus -> TestM ()
      -- putSearchVisibilityAvailableConfig tid cfg = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "features", "search-visibility-available"] . queryItem "status" (toByteString' cfg) . expect2xx)

      -- -- :<|> Named "put-route-validate-saml-emails-config" (MkFeaturePutRouteTrivialConfigNoTTL ValidateSAMLEmailsConfig)
      -- putValidateSAMLEmailsConfig :: TeamId -> FeatureStatus -> TestM ()
      -- putValidateSAMLEmailsConfig tid cfg = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "features", "validate-saml-emails"] . queryItem "status" (toByteString' cfg) . expect2xx)

      -- -- :<|> Named "put-route-digital-signatures-config" (MkFeaturePutRouteTrivialConfigNoTTL DigitalSignaturesConfig)
      -- putDigitalSignaturesConfig :: TeamId -> FeatureStatus -> TestM ()
      -- putDigitalSignaturesConfig tid cfg = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "features", "digital-signatures"] . queryItem "status" (toByteString' cfg) . expect2xx)

      -- -- :<|> Named "put-route-file-sharing-config" (MkFeaturePutRouteTrivialConfigNoTTL FileSharingConfig)
      -- putFileSharingConfig :: TeamId -> FeatureStatus -> TestM ()
      -- putFileSharingConfig tid cfg = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "features", "file-sharing"] . queryItem "status" (toByteString' cfg) . expect2xx)

      -- -- :<|> Named "put-route-conference-calling-config" (MkFeaturePutRouteTrivialConfigWithTTL ConferenceCallingConfig)
      -- putConferenceCallingConfig :: TeamId -> FeatureStatus -> FeatureTTLDays -> TestM ()
      -- putConferenceCallingConfig tid cfg ttl = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "features", "conference-calling"] . queryItem "status" (toByteString' cfg) . queryItem "ttl" (toByteString' ttl) . expect2xx)

      -- -- type MkFeaturePutRoute (feature :: Type) =
      -- --   Summary "Disable / enable feature flag for a given team"
      -- --     :> "teams"
      -- --     :> Capture "tid" TeamId
      -- --     :> "features"
      -- --     :> FeatureSymbol feature
      -- --     :> ReqBody '[JSON] (WithStatusNoLock feature)
      -- --     :> Put '[JSON] NoContent
      -- putFeatureConfig ::
      --   forall cfg.
      --   ( KnownSymbol (FeatureSymbol cfg),
      --     ToSchema cfg,
      --     Typeable cfg,
      --     IsFeatureConfig cfg
      --   ) =>
      --   TeamId ->
      --   WithStatusNoLock cfg ->
      --   TestM ()
      -- putFeatureConfig tid cfg = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg] . json cfg . expect2xx)

      -- -- :<|> Named
      -- --        "get-search-visibility"
      -- --        ( Summary "Shows the current TeamSearchVisibility value for the given team"
      -- --            :> Description
      -- --                 "These endpoints should be part of team settings. Until that happens, \
      -- --                 \we access them from here for authorized personnel to enable/disable \
      -- --                 \this on the team's behalf"
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> "search-visibility"
      -- --            :> Get '[JSON] TeamSearchVisibilityView
      -- --        )
      -- getSearchVisibility :: TeamId -> TestM TeamSearchVisibilityView
      -- getSearchVisibility tid = do
      --   s <- view tsStern
      --   r <- get (s . paths ["teams", toByteString' tid, "search-visibility"] . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named
      -- --        "put-search-visibility"
      -- --        ( Summary "Shows the current TeamSearchVisibility value for the given team"
      -- --            :> Description
      -- --                 "These endpoints should be part of team settings. Until that happens, \
      -- --                 \we access them from here for authorized personnel to enable/disable \
      -- --                 \this on the team's behalf"
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> "search-visibility"
      -- --            :> ReqBody '[JSON] TeamSearchVisibility
      -- --            :> Get '[JSON] NoContent
      -- --        )
      -- putSearchVisibility :: TeamId -> TeamSearchVisibility -> TestM ()
      -- putSearchVisibility tid vis = do
      --   s <- view tsStern
      --   void $ put (s . paths ["teams", toByteString' tid, "search-visibility"] . json vis . expect2xx)

      -- -- :<|> Named "get-route-outlook-cal-config" (MkFeatureGetRoute OutlookCalIntegrationConfig)
      -- -- :<|> Named "put-route-outlook-cal-config" (MkFeaturePutRouteTrivialConfigNoTTL OutlookCalIntegrationConfig)
      -- -- :<|> Named
      -- --        "get-team-invoice"
      -- --        ( Summary "Get a specific invoice by Number"
      -- --            :> Description "Relevant only internally at Wire"
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> "invoice"
      -- --            :> Capture "inr" InvoiceId
      -- --            :> Get '[JSON] Text
      -- --        )
      -- getTeamInvoice :: TeamId -> InvoiceId -> TestM Text
      -- getTeamInvoice tid inr = do
      --   s <- view tsStern
      --   r <- get (s . paths ["teams", toByteString' tid, "invoice", toByteString' inr] . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named
      -- --        "get-team-billing-info"
      -- --        ( Summary "Gets billing information about a team"
      -- --            :> Description "Relevant only internally at Wire"
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> "billing"
      -- --            :> Get '[JSON] TeamBillingInfo
      -- --        )
      -- getTeamBillingInfo :: TeamId -> TestM TeamBillingInfo
      -- getTeamBillingInfo tid = do
      --   s <- view tsStern
      --   r <- get (s . paths ["teams", toByteString' tid, "billing"] . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named
      -- --        "put-team-billing-info"
      -- --        ( Summary "Updates billing information about a team. Non specified fields will NOT be updated"
      -- --            :> Description "Relevant only internally at Wire"
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> "billing"
      -- --            :> ReqBody '[JSON] TeamBillingInfoUpdate
      -- --            :> Put '[JSON] TeamBillingInfo
      -- --        )
      -- putTeamBillingInfo :: TeamId -> TeamBillingInfoUpdate -> TestM TeamBillingInfo
      -- putTeamBillingInfo tid upd = do
      --   s <- view tsStern
      --   r <- put (s . paths ["teams", toByteString' tid, "billing"] . json upd . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named
      -- --        "post-team-billing-info"
      -- --        ( Summary
      -- --            "Sets billing information about a team. Can only be used on teams that do NOT have any \
      -- --            \billing information set. To update team billing info, use the update endpoint"
      -- --            :> Description "Relevant only internally at Wire"
      -- --            :> "teams"
      -- --            :> Capture "tid" TeamId
      -- --            :> "billing"
      -- --            :> ReqBody '[JSON] TeamBillingInfo
      -- --            :> Post '[JSON] TeamBillingInfo
      -- --        )
      -- postTeamBillingInfo :: TeamId -> TeamBillingInfo -> TestM TeamBillingInfo
      -- postTeamBillingInfo tid upd = do
      --   s <- view tsStern
      --   r <- post (s . paths ["teams", toByteString' tid, "billing"] . json upd . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named
      -- --        "get-consent-log"
      -- --        ( Summary "Fetch the consent log given an email address of a non-user"
      -- --            :> Description "Relevant only internally at Wire"
      -- --            :> "i"
      -- --            :> "consent"
      -- --            :> QueryParam' [Required, Strict, Description "A verified email address"] "email" Email
      -- --            :> Get '[JSON] ConsentLogAndMarketo
      -- --        )
      -- getConsentLog :: Email -> TestM ConsentLogAndMarketo
      -- getConsentLog email = do
      --   s <- view tsStern
      --   r <- get (s . paths ["i", "consent"] . queryItem "email" (toByteString' email) . expect2xx)
      --   pure $ responseJsonUnsafe r

      -- -- :<|> Named
      -- --        "get-user-meta-info"
      -- --        ( Summary "Fetch a user's meta info given a user id: TEMPORARY!"
      -- --            :> Description "Relevant only internally at Wire"
      -- --            :> "i"
      -- --            :> "user"
      -- --            :> "meta-info"
      -- --            :> QueryParam' [Required, Strict, Description "A valid UserId"] "id" UserId
      -- --            :> Post '[JSON] UserMetaInfo
      -- --        )
      -- getUserMetaInfo :: UserId -> TestM UserMetaInfo
      -- getUserMetaInfo uid = do
      --   s <- view tsStern
      --   r <- post (s . paths ["i", "user", "meta-info"] . queryItem "id" (toByteString' uid) . expect2xx)
      --   pure $ responseJsonUnsafe r
      test s "GET /teams/:id" testGetTeamInfo
    ]

testGetConnectionsByIds :: TestM ()
testGetConnectionsByIds = do
  uids <- sequence [randomUser, randomUser, randomUser]
  connections <- getConnectionsByUserIds uids
  liftIO $ connections @?= []

testGetConnections :: TestM ()
testGetConnections = do
  uid <- randomUser
  connections <- getConnections uid
  liftIO $ connections @?= UserConnectionGroups 0 0 0 0 0 0 0

testGetUsersByHandles :: TestM ()
testGetUsersByHandles = do
  uid <- randomUser
  h <- randomHandle
  void $ setHandle uid h
  [ua] <- getUsersByHandles h
  liftIO $ ua.accountUser.userId @?= uid

testGetUsersByPhone :: TestM ()
testGetUsersByPhone = do
  (uid, phone) <- randomPhoneUser
  [ua] <- getUsersByPhone phone
  liftIO $ ua.accountUser.userId @?= uid

testGetUsersByEmail :: TestM ()
testGetUsersByEmail = do
  (uid, email) <- randomEmailUser
  [ua] <- getUsersByEmail email
  liftIO $ ua.accountUser.userId @?= uid

testUnsuspendUser :: TestM ()
testUnsuspendUser = do
  uid <- randomUser
  void $ postSupendUser uid
  do
    [ua] <- getUsersByIds [uid]
    liftIO $ ua.accountStatus @?= Suspended
  void $ postUnsuspendUser uid
  do
    [ua] <- getUsersByIds [uid]
    liftIO $ ua.accountStatus @?= Active

testSuspendUser :: TestM ()
testSuspendUser = do
  uid <- randomUser
  void $ postSupendUser uid
  [ua] <- getUsersByIds [uid]
  liftIO $ ua.accountStatus @?= Suspended

testGetStatus :: TestM ()
testGetStatus = do
  r <- getStatus
  liftIO $ do
    statusCode r @?= 200

testGetUsersByIds :: TestM ()
testGetUsersByIds = do
  uid1 <- randomUser
  uid2 <- randomUser
  uas <- getUsersByIds [uid1, uid2]
  liftIO $ do
    length uas @?= 2
    Set.fromList ((.accountUser.userId) <$> uas) @?= Set.fromList [uid1, uid2]

testGetTeamInfo :: TestM ()
testGetTeamInfo = do
  (_, tid, _) <- createBindingTeamWithNMembers 10
  info <- getTeamInfo tid
  liftIO $ length info.tiMembers @?= 11

testSearchUsers :: TestM ()
testSearchUsers = do
  uid <- randomUser
  result <- searchUsers uid
  liftIO $ do
    result.searchFound @?= 0

testRevokeIdentity :: TestM ()
testRevokeIdentity = do
  (_, (email, phone)) <- randomEmailPhoneUser
  do
    [ua] <- getUsersByEmail email
    liftIO $ do
      ua.accountStatus @?= Active
      isJust ua.accountUser.userIdentity @?= True
  void $ revokeIdentity (Left email)
  void $ revokeIdentity (Right phone)
  do
    [ua] <- getUsersByEmail email
    liftIO $ do
      ua.accountStatus @?= Active
      isJust ua.accountUser.userIdentity @?= False

testPutEmail :: TestM ()
testPutEmail = do
  uid <- randomUser
  email <- randomEmail
  -- If the user has a pending email validation, the validation email will be resent. But we simply test that this call returns 200
  putEmail uid (EmailUpdate email)

-------------------------------------------------------------------------------
-- API Calls

instance (ToByteString a) => ToByteString [a] where
  builder xs = builder $ cs @String @ByteString $ intercalate "," (cs . toByteString' <$> xs)

getConnectionsByUserIds :: [UserId] -> TestM [ConnectionStatus]
getConnectionsByUserIds uids = do
  s <- view tsStern
  r <- get (s . paths ["users", "connections"] . queryItem "ids" (toByteString' uids) . expect2xx)
  pure $ responseJsonUnsafe r

getConnections :: UserId -> TestM UserConnectionGroups
getConnections uid = do
  s <- view tsStern
  r <- get (s . paths ["users", toByteString' uid, "connections"] . expect2xx)
  pure $ responseJsonUnsafe r

getUsersByHandles :: Text -> TestM [UserAccount]
getUsersByHandles h = do
  stern <- view tsStern
  r <- get (stern . paths ["users", "by-handles"] . queryItem "handles" (cs h) . expect2xx)
  pure $ responseJsonUnsafe r

getUsersByPhone :: Phone -> TestM [UserAccount]
getUsersByPhone phone = do
  stern <- view tsStern
  r <- get (stern . paths ["users", "by-phone"] . queryItem "phone" (toByteString' phone) . expect2xx)
  pure $ responseJsonUnsafe r

getUsersByEmail :: Email -> TestM [UserAccount]
getUsersByEmail email = do
  stern <- view tsStern
  r <- get (stern . paths ["users", "by-email"] . queryItem "email" (toByteString' email) . expect2xx)
  pure $ responseJsonUnsafe r

postUnsuspendUser :: UserId -> TestM ResponseLBS
postUnsuspendUser uid = do
  stern <- view tsStern
  post (stern . paths ["users", toByteString' uid, "unsuspend"] . expect2xx)

postSupendUser :: UserId -> TestM ResponseLBS
postSupendUser uid = do
  stern <- view tsStern
  post (stern . paths ["users", toByteString' uid, "suspend"] . expect2xx)

getStatus :: TestM ResponseLBS
getStatus = do
  stern <- view tsStern
  get (stern . paths ["i", "status"] . expect2xx)

getUsersByIds :: [UserId] -> TestM [UserAccount]
getUsersByIds uids = do
  stern <- view tsStern
  r <- get (stern . paths ["users", "by-ids"] . queryItem "ids" (toByteString' uids) . expect2xx)
  pure $ responseJsonUnsafe r

getTeamInfo :: TeamId -> TestM TeamInfo
getTeamInfo tid = do
  stern <- view tsStern
  r <- get (stern . paths ["teams", toByteString' tid] . expect2xx)
  pure $ responseJsonUnsafe r

searchUsers :: UserId -> TestM (SearchResult Contact)
searchUsers uid = do
  s <- view tsStern
  r <- get (s . paths ["users", toByteString' uid, "search"] . expect2xx)
  pure $ responseJsonUnsafe r

revokeIdentity :: Either Email Phone -> TestM ()
revokeIdentity emailOrPhone = do
  s <- view tsStern
  void $ post (s . paths ["users", "revoke-identity"] . mkQueryParam emailOrPhone . expect2xx)

mkQueryParam :: Either Email Phone -> Request -> Request
mkQueryParam = \case
  Left email -> queryItem "email" (toByteString' email)
  Right phone -> queryItem "phone" (toByteString' phone)

putEmail :: UserId -> EmailUpdate -> TestM ()
putEmail uid emailUpdate = do
  s <- view tsStern
  void $ put (s . paths ["users", toByteString' uid, "email"] . json emailUpdate . expect2xx)

putPhone :: UserId -> PhoneUpdate -> TestM ()
putPhone uid phoneUpdate = do
  s <- view tsStern
  void $ put (s . paths ["users", toByteString' uid, "phone"] . json phoneUpdate . expect2xx)

deleteUser :: UserId -> Either Email Phone -> TestM ()
deleteUser uid emailOrPhone = do
  s <- view tsStern
  void $ delete (s . paths ["users", toByteString' uid] . mkQueryParam emailOrPhone . expect2xx)

suspendTeam :: TeamId -> TestM ()
suspendTeam tid = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "suspend"] . expect2xx)

unsuspendTeam :: TeamId -> TestM ()
unsuspendTeam tid = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "unsuspend"] . expect2xx)

deleteTeam :: TeamId -> Bool -> Email -> TestM ()
deleteTeam tid force email = do
  s <- view tsStern
  void $ delete (s . paths ["teams", toByteString' tid] . queryItem "force" (toByteString' force) . queryItem "email" (toByteString' email) . expect2xx)

ejpdInfo :: Bool -> [Handle] -> TestM EJPD.EJPDResponseBody
ejpdInfo includeContacts handles = do
  s <- view tsStern
  r <- get (s . paths ["ejpd-info"] . queryItem "include_contacts" (toByteString' includeContacts) . queryItem "handles" (toByteString' handles) . expect2xx)
  pure $ responseJsonUnsafe r

userBlacklistHead :: Either Email Phone -> TestM ()
userBlacklistHead emailOrPhone = do
  s <- view tsStern
  void $ Bilge.head (s . paths ["users", "blacklist"] . mkQueryParam emailOrPhone . expect2xx)

postUserBlacklist :: Either Email Phone -> TestM ()
postUserBlacklist emailOrPhone = do
  s <- view tsStern
  void $ post (s . paths ["users", "blacklist"] . mkQueryParam emailOrPhone . expect2xx)

deleteUserBlacklist :: Either Email Phone -> TestM ()
deleteUserBlacklist emailOrPhone = do
  s <- view tsStern
  void $ delete (s . paths ["users", "blacklist"] . mkQueryParam emailOrPhone . expect2xx)

getTeamInfoByMemberEmail :: Email -> TestM TeamInfo
getTeamInfoByMemberEmail email = do
  s <- view tsStern
  r <- get (s . paths ["teams"] . queryItem "email" (toByteString' email) . expect2xx)
  pure $ responseJsonUnsafe r

getTeamAdminInfo :: TeamId -> TestM TeamAdminInfo
getTeamAdminInfo tid = do
  s <- view tsStern
  r <- get (s . paths ["teams", toByteString' tid, "admins"] . expect2xx)
  pure $ responseJsonUnsafe r

getFeatureConfig ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg
  ) =>
  TeamId ->
  TestM (WithStatus cfg)
getFeatureConfig tid = do
  s <- view tsStern
  r <- get (s . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg] . expect2xx)
  pure $ responseJsonUnsafe r

putLegalholdConfig :: TeamId -> FeatureStatus -> TestM ()
putLegalholdConfig tid status = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "features", "legalhold"] . queryItem "status" (toByteString' status) . expect2xx)

getSSOConfig :: TeamId -> TestM (WithStatus SSOConfig)
getSSOConfig tid = do
  s <- view tsStern
  r <- get (s . paths ["teams", toByteString' tid, "features", "sso"] . expect2xx)
  pure $ responseJsonUnsafe r

putSSOConfig :: TeamId -> FeatureStatus -> TestM ()
putSSOConfig tid cfg = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "features", "sso"] . queryItem "status" (toByteString' cfg) . expect2xx)

putSearchVisibilityAvailableConfig :: TeamId -> FeatureStatus -> TestM ()
putSearchVisibilityAvailableConfig tid cfg = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "features", "search-visibility-available"] . queryItem "status" (toByteString' cfg) . expect2xx)

putValidateSAMLEmailsConfig :: TeamId -> FeatureStatus -> TestM ()
putValidateSAMLEmailsConfig tid cfg = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "features", "validate-saml-emails"] . queryItem "status" (toByteString' cfg) . expect2xx)

putDigitalSignaturesConfig :: TeamId -> FeatureStatus -> TestM ()
putDigitalSignaturesConfig tid cfg = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "features", "digital-signatures"] . queryItem "status" (toByteString' cfg) . expect2xx)

putFileSharingConfig :: TeamId -> FeatureStatus -> TestM ()
putFileSharingConfig tid cfg = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "features", "file-sharing"] . queryItem "status" (toByteString' cfg) . expect2xx)

putConferenceCallingConfig :: TeamId -> FeatureStatus -> FeatureTTLDays -> TestM ()
putConferenceCallingConfig tid cfg ttl = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "features", "conference-calling"] . queryItem "status" (toByteString' cfg) . queryItem "ttl" (toByteString' ttl) . expect2xx)

putFeatureConfig ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg
  ) =>
  TeamId ->
  WithStatusNoLock cfg ->
  TestM ()
putFeatureConfig tid cfg = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg] . json cfg . expect2xx)

getSearchVisibility :: TeamId -> TestM TeamSearchVisibilityView
getSearchVisibility tid = do
  s <- view tsStern
  r <- get (s . paths ["teams", toByteString' tid, "search-visibility"] . expect2xx)
  pure $ responseJsonUnsafe r

putSearchVisibility :: TeamId -> TeamSearchVisibility -> TestM ()
putSearchVisibility tid vis = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "search-visibility"] . json vis . expect2xx)

getTeamInvoice :: TeamId -> InvoiceId -> TestM Text
getTeamInvoice tid inr = do
  s <- view tsStern
  r <- get (s . paths ["teams", toByteString' tid, "invoice", toByteString' inr] . expect2xx)
  pure $ responseJsonUnsafe r

getTeamBillingInfo :: TeamId -> TestM TeamBillingInfo
getTeamBillingInfo tid = do
  s <- view tsStern
  r <- get (s . paths ["teams", toByteString' tid, "billing"] . expect2xx)
  pure $ responseJsonUnsafe r

putTeamBillingInfo :: TeamId -> TeamBillingInfoUpdate -> TestM TeamBillingInfo
putTeamBillingInfo tid upd = do
  s <- view tsStern
  r <- put (s . paths ["teams", toByteString' tid, "billing"] . json upd . expect2xx)
  pure $ responseJsonUnsafe r

postTeamBillingInfo :: TeamId -> TeamBillingInfo -> TestM TeamBillingInfo
postTeamBillingInfo tid upd = do
  s <- view tsStern
  r <- post (s . paths ["teams", toByteString' tid, "billing"] . json upd . expect2xx)
  pure $ responseJsonUnsafe r

getConsentLog :: Email -> TestM ConsentLogAndMarketo
getConsentLog email = do
  s <- view tsStern
  r <- get (s . paths ["i", "consent"] . queryItem "email" (toByteString' email) . expect2xx)
  pure $ responseJsonUnsafe r

getUserMetaInfo :: UserId -> TestM UserMetaInfo
getUserMetaInfo uid = do
  s <- view tsStern
  r <- post (s . paths ["i", "user", "meta-info"] . queryItem "id" (toByteString' uid) . expect2xx)
  pure $ responseJsonUnsafe r
