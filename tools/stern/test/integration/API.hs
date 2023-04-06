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
      test s "PUT /users/:uid/phone" testPutPhone,
      test s "DELETE /users/:uid" testDeleteUser,
      test s "PUT /teams/:tid/suspend" testSuspendTeam,
      test s "PUT /teams/:tid/unsuspend" testUnsuspendTeam,
      test s "DELETE /teams/:tid" testDeleteTeam,
      test s "GET /ejpd-info" testEjpdInfo,
      test s "HEAD /users/blacklist" testUserBlacklistHead,
      test s "POST /users/blacklist" testPostUserBlacklist,
      test s "DELETE /users/blacklist" testDeleteUserBlacklist,
      test s "GET /teams" testGetTeamInfoByMemberEmail,
      test s "GET /teams/:tid/admins" testGetTeamAdminInfo,
      test s "GET /teams/:tid/features/legalhold" testGetLegalholdConfig,
      test s "PUT /teams/:tid/features/legalhold" testPutLegalholdConfig,
      test s "GET /teams/:tid/features/sso" testGetSSOConfig,
      test s "PUT /teams/:tid/features/sso" testPutSSOConfig,
      test s "PUT /teams/:tid/features/search-visibility-available" testPutSearchVisibilityAvailableConfig,
      test s "PUT /teams/:tid/features/validate-saml-emails" testPutValidateSAMLEmailsConfig,
      test s "PUT /teams/:tid/features/digital-signatures" testPutDigitalSignaturesConfig,
      test s "PUT /teams/:tid/features/file-sharing" testPutFileSharingConfig,
      test s "PUT /teams/:tid/features/conference-calling" testPutConferenceCallingConfig,
      test s "PUT /teams/:tid/features/:feature" testPutFeatureConfig,
      test s "GET /teams/:tid/search-visibility" testGetSearchVisibility,
      test s "PUT /teams/:tid/search-visibility" testPutSearchVisibility,
      test s "GET /teams/:tid/invoice/:inr" testGetTeamInvoice,
      test s "GET /teams/:tid/billing" testGetTeamBillingInfo,
      test s "PUT /teams/:tid/billing" testPutTeamBillingInfo,
      test s "POST /teams/:tid/billing" testPostTeamBillingInfo,
      test s "GET /i/consent" testGetConsentLog,
      test s "GET /teams/:id" testGetTeamInfo
    ]

testPutPhone :: TestM ()
testPutPhone = pure ()

testDeleteUser :: TestM ()
testDeleteUser = pure ()

testSuspendTeam :: TestM ()
testSuspendTeam = pure ()

testUnsuspendTeam :: TestM ()
testUnsuspendTeam = pure ()

testDeleteTeam :: TestM ()
testDeleteTeam = pure ()

testEjpdInfo :: TestM ()
testEjpdInfo = pure ()

testUserBlacklistHead :: TestM ()
testUserBlacklistHead = pure ()

testPostUserBlacklist :: TestM ()
testPostUserBlacklist = pure ()

testDeleteUserBlacklist :: TestM ()
testDeleteUserBlacklist = pure ()

testGetTeamInfoByMemberEmail :: TestM ()
testGetTeamInfoByMemberEmail = pure ()

testGetTeamAdminInfo :: TestM ()
testGetTeamAdminInfo = pure ()

testGetLegalholdConfig :: TestM ()
testGetLegalholdConfig = pure ()

testPutLegalholdConfig :: TestM ()
testPutLegalholdConfig = pure ()

testGetSSOConfig :: TestM ()
testGetSSOConfig = pure ()

testPutSSOConfig :: TestM ()
testPutSSOConfig = pure ()

testPutSearchVisibilityAvailableConfig :: TestM ()
testPutSearchVisibilityAvailableConfig = pure ()

testPutValidateSAMLEmailsConfig :: TestM ()
testPutValidateSAMLEmailsConfig = pure ()

testPutDigitalSignaturesConfig :: TestM ()
testPutDigitalSignaturesConfig = pure ()

testPutFileSharingConfig :: TestM ()
testPutFileSharingConfig = pure ()

testPutConferenceCallingConfig :: TestM ()
testPutConferenceCallingConfig = pure ()

testPutFeatureConfig :: TestM ()
testPutFeatureConfig = pure ()

testGetSearchVisibility :: TestM ()
testGetSearchVisibility = pure ()

testPutSearchVisibility :: TestM ()
testPutSearchVisibility = pure ()

testGetTeamInvoice :: TestM ()
testGetTeamInvoice = pure ()

testGetTeamBillingInfo :: TestM ()
testGetTeamBillingInfo = pure ()

testPutTeamBillingInfo :: TestM ()
testPutTeamBillingInfo = pure ()

testPostTeamBillingInfo :: TestM ()
testPostTeamBillingInfo = pure ()

testGetConsentLog :: TestM ()
testGetConsentLog = pure ()

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
