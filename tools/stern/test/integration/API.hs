{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module API (tests) where

import Bilge
import Bilge.Assert
import Brig.Types.Intra
import Control.Applicative
import Control.Lens hiding ((.=))
import Data.Aeson (ToJSON, Value)
import Data.Aeson qualified as A
import Data.ByteString.Conversion
import Data.Default
import Data.Handle
import Data.Id
import Data.Range (unsafeRange)
import Data.Schema
import Data.Set qualified as Set
import Data.String.Conversions
import Data.UUID.V4 (nextRandom)
import GHC.TypeLits
import Imports
import Stern.API.Routes (UserConnectionGroups (..))
import Stern.Types
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Util
import Wire.API.EnterpriseLogin (DomainRedirect (NoRegistration), DomainRegistrationUpdate (DomainRegistrationUpdate), TeamInvite (Allowed))
import Wire.API.OAuth (OAuthApplicationName (OAuthApplicationName), OAuthClientConfig (..), OAuthClientCredentials (..))
import Wire.API.Properties (PropertyKey)
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Internal.Brig.EJPD qualified as EJPD
import Wire.API.Routes.Internal.Galley.TeamsIntra (tdStatus)
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as Team
import Wire.API.Team (teamId)
import Wire.API.Team.Feature
import Wire.API.Team.Feature qualified as Public
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
      test s "GET /users/by-ids" testGetUsersByIds,
      test s "GET /users/by-handles" testGetUsersByHandles,
      test s "GET /users/:id/connections" testGetConnections,
      test s "GET /users/connections?ids=..." testGetConnectionsByIds,
      test s "GET /users/:uid/search" testSearchUsers,
      test s "POST /users/revoke-identity?email=..." testRevokeIdentity,
      test s "PUT /users/:uid/email" testPutEmail,
      test s "DELETE /users/:uid" testDeleteUser,
      test s "PUT /teams/:tid/suspend" testSuspendTeam,
      test s "PUT /teams/:tid/unsuspend" testUnsuspendTeam,
      test s "DELETE /teams/:tid" testDeleteTeam,
      test s "GET /ejpd-info" testEjpdInfo,
      test s "/users/blacklist" testUserBlacklist,
      test s "GET /teams" testGetTeamInfoByMemberEmail,
      test s "GET /teams/:tid/admins" testGetTeamAdminInfo,
      test s "/teams/:tid/features/legalhold" testLegalholdConfig,
      test s "/teams/:tid/features/sso" $ testFeatureStatus @SSOConfig,
      test s "/teams/:tid/features/validateSamlEmails" $ testFeatureStatus @ValidateSAMLEmailsConfig,
      test s "/teams/:tid/features/digitalSignatures" $ testFeatureStatus @DigitalSignaturesConfig,
      test s "/teams/:tid/features/fileSharing" $ testFeatureStatus @FileSharingConfig,
      test s "/teams/:tid/features/conference-calling" $ testFeatureStatusOptTtl defConfCalling (Just FeatureTTLUnlimited),
      test s "/teams/:tid/searchVisibility" $ testFeatureStatus @SearchVisibilityAvailableConfig,
      test s "/teams/:tid/features/appLock" $ testFeatureConfig @AppLockConfig,
      test s "/teams/:tid/features/mls" $ testFeatureConfig @MLSConfig,
      test s "GET /teams/:tid/features/classifiedDomains" $ testGetFeatureConfig @ClassifiedDomainsConfig (Just FeatureStatusEnabled),
      test s "GET /teams/:tid/features/outlookCalIntegration" $ testFeatureStatus @OutlookCalIntegrationConfig,
      test s "PUT /teams/:tid/features/outlookCalIntegration{,'?lockOrUnlock'}" $ testFeatureStatusWithLock @OutlookCalIntegrationConfig,
      test s "GET /i/consent" testGetConsentLog,
      test s "GET /teams/:id" testGetTeamInfo,
      test s "GET i/user/meta-info?id=..." testGetUserMetaInfo,
      test s "/teams/:tid/search-visibility" testSearchVisibility,
      test s "/sso-domain-redirect" testRudSsoDomainRedirect,
      test s "i/oauth/clients" testCrudOAuthClient,
      test s "i/domain-registration" testDomainRegistration
      -- The following endpoints can not be tested here because they require ibis:
      -- - `GET /teams/:tid/billing`
      -- - `GET /teams/:tid/invoice/:inr`
      -- - `PUT /teams/:tid/billing`
      -- - `POST /teams/:tid/billing`
    ]

defConfCalling :: LockableFeature ConferenceCallingConfig
defConfCalling = def {status = FeatureStatusDisabled}

testRudSsoDomainRedirect :: TestM ()
testRudSsoDomainRedirect = do
  testGet 1 Nothing
  putSsoDomainRedirect sampleDomain sampleConfig sampleWelcome
  testGet 2 (Just $ A.object ["config_json_url" A..= sampleConfig, "webapp_welcome_url" A..= sampleWelcome])
  putSsoDomainRedirect sampleDomain sampleConfig' sampleWelcome'
  testGet 3 (Just $ A.object ["config_json_url" A..= sampleConfig', "webapp_welcome_url" A..= sampleWelcome'])
  deleteSsoDomainRedirect sampleDomain
  testGet 4 Nothing
  where
    sampleDomain :: ByteString
    sampleDomain = "57119282-3071-11ee-aebe-a32e317d3fb5.example.com"

    sampleConfig :: Text
    sampleConfig = "https://config.57119282-3071-11ee-aebe-a32e317d3fb5.example.com/config.json"

    sampleWelcome :: Text
    sampleWelcome = "https://app.57119282-3071-11ee-aebe-a32e317d3fb5.example.com/welcome.html"

    sampleConfig' :: Text
    sampleConfig' = "https://s3.57119282-3071-11ee-aebe-a32e317d3fb5.example.com/new-wire-config.json"

    sampleWelcome' :: Text
    sampleWelcome' = "https://new-app.57119282-3071-11ee-aebe-a32e317d3fb5.example.com/new"

    testGet :: Int -> Maybe Value -> TestM ()
    testGet (show -> msg) expectedEntry = liftIO . (assertEqual msg expectedEntry) =<< getSsoDomainRedirect sampleDomain

testCrudOAuthClient :: TestM ()
testCrudOAuthClient = do
  let url = fromMaybe (error "invalid url") . fromByteString $ "https://example.com"
  let name = OAuthApplicationName (unsafeRange "foobar")
  cred <- registerOAuthClient (OAuthClientConfig name url)
  c <- getOAuthClient cred.clientId
  liftIO $ do
    c.applicationName @?= name
    c.redirectUrl @?= url
  let newName = OAuthApplicationName (unsafeRange "barfoo")
  let newUrl = fromMaybe (error "invalid url") . fromByteString $ "https://example.org"
  updateOAuthClient cred.clientId (OAuthClientConfig newName newUrl)
  c' <- getOAuthClient cred.clientId
  liftIO $ do
    c'.applicationName @?= newName
    c'.redirectUrl @?= newUrl
  deleteOAuthClient cred.clientId
  getOAuthClient' cred.clientId !!! const 404 === statusCode

testSearchVisibility :: TestM ()
testSearchVisibility = do
  (_, tid, _) <- createTeamWithNMembers 10
  putFeatureStatus @SearchVisibilityAvailableConfig tid FeatureStatusEnabled Nothing !!! const 200 === statusCode
  do
    TeamSearchVisibilityView sv <- getSearchVisibility tid
    liftIO $ sv @?= SearchVisibilityStandard

  putSearchVisibility tid SearchVisibilityNoNameOutsideTeam
  do
    TeamSearchVisibilityView sv <- getSearchVisibility tid
    liftIO $ sv @?= SearchVisibilityNoNameOutsideTeam

testGetUserMetaInfo :: TestM ()
testGetUserMetaInfo = do
  uid <- randomUser
  let k = fromMaybe (error "invalid property key") $ fromByteString "WIRE_RECEIPT_MODE"
  putUserProperty uid k "bar"
  -- Just make sure this returns a 200
  void $ getUserMetaInfo uid

testDeleteUser :: TestM ()
testDeleteUser = do
  (uid, email) <- randomEmailUser
  do
    [ua] <- getUsersByIds [uid]
    liftIO $ ua.userStatus @?= Active
  deleteUser uid (Left email)
  do
    uas <- getUsersByIds [uid]
    liftIO $ uas @?= []

testSuspendTeam :: TestM ()
testSuspendTeam = do
  (_, tid, _) <- createTeamWithNMembers 10
  do
    info <- getTeamInfo tid
    liftIO $ info.tiData.tdStatus @?= Team.Active
  suspendTeam tid
  do
    info <- getTeamInfo tid
    liftIO $ info.tiData.tdStatus @?= Team.Suspended

testUnsuspendTeam :: TestM ()
testUnsuspendTeam = do
  (_, tid, _) <- createTeamWithNMembers 10
  suspendTeam tid
  do
    info <- getTeamInfo tid
    liftIO $ info.tiData.tdStatus @?= Team.Suspended
  unsuspendTeam tid
  do
    info <- getTeamInfo tid
    liftIO $ info.tiData.tdStatus @?= Team.Active

testDeleteTeam :: TestM ()
testDeleteTeam = do
  (uid, tid, _) <- createTeamWithNMembers 10
  [ua] <- getUsersByIds [uid]
  let email = fromMaybe (error "user has no email") $ emailIdentity =<< ua.userIdentity
  do
    info <- getTeamInfo tid
    liftIO $ info.tiData.tdStatus @?= Team.Active
  deleteTeam tid True email
  eventually $ do
    info <- getTeamInfo tid
    liftIO $ assertEqual "team status should be 'Deleted'" Team.Deleted info.tiData.tdStatus

testEjpdInfo :: TestM ()
testEjpdInfo = do
  uid <- randomUser
  h <- randomHandle
  void $ setHandle uid h
  info <- ejpdInfo True [fromJust $ parseHandle h]
  liftIO $ fmap (.ejpdResponseRootHandle) info.ejpdResponseBody @?= [Just (fromJust (parseHandle h))]

testUserBlacklist :: TestM ()
testUserBlacklist = do
  (_, email) <- randomEmailUser
  userBlacklistHead (Left email) !!! const 404 === statusCode
  postUserBlacklist (Left email)
  userBlacklistHead (Left email) !!! const 200 === statusCode
  deleteUserBlacklist (Left email)
  userBlacklistHead (Left email) !!! const 404 === statusCode

testGetTeamInfoByMemberEmail :: TestM ()
testGetTeamInfoByMemberEmail = do
  (_, tid, member : _) <- createTeamWithNMembers 10
  [ua] <- getUsersByIds [member]
  let email = fromMaybe (error "user has no email") $ emailIdentity =<< ua.userIdentity
  info <- getTeamInfoByMemberEmail email
  liftIO $ (info.tiData.tdTeam ^. teamId) @?= tid

testGetTeamAdminInfo :: TestM ()
testGetTeamAdminInfo = do
  (_, tid, _) <- createTeamWithNMembers 10
  info <- getTeamAdminInfo tid
  liftIO $ do
    (info.taData.tdTeam ^. teamId) @?= tid
    (length info.taOwners) @?= 1
    info.taMembers @?= 11

testLegalholdConfig :: TestM ()
testLegalholdConfig = do
  (_, tid, _) <- createTeamWithNMembers 10
  cfg <- getFeatureConfig @LegalholdConfig tid
  liftIO $ cfg @?= def
  -- Legal hold is enabled for teams via server config and cannot be changed here
  putFeatureStatus @LegalholdConfig tid FeatureStatusEnabled Nothing !!! const 403 === statusCode

testFeatureConfig ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg,
    Eq cfg,
    Show cfg
  ) =>
  TestM ()
testFeatureConfig = do
  (_, tid, _) <- createTeamWithNMembers 10
  cfg <- getFeatureConfig @cfg tid
  liftIO $ cfg @?= def
  let newStatus = if cfg.status == FeatureStatusEnabled then FeatureStatusDisabled else FeatureStatusEnabled
  putFeatureConfig @cfg tid cfg {status = newStatus} !!! const 200 === statusCode
  cfg' <- getFeatureConfig @cfg tid
  liftIO $ cfg'.status @?= newStatus

testGetFeatureConfig ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg,
    Eq cfg,
    Show cfg
  ) =>
  Maybe FeatureStatus ->
  TestM ()
testGetFeatureConfig mDef = do
  (_, tid, _) <- createTeamWithNMembers 10
  cfg <- getFeatureConfig @cfg tid
  liftIO $ cfg.status @?= fromMaybe (def @(Feature cfg)).status mDef

testFeatureStatus ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg,
    Eq cfg,
    Show cfg
  ) =>
  TestM ()
testFeatureStatus = testFeatureStatusOptTtl @cfg def Nothing

testFeatureStatusOptTtl ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg,
    Eq cfg,
    Show cfg
  ) =>
  LockableFeature cfg ->
  Maybe FeatureTTL ->
  TestM ()
testFeatureStatusOptTtl defValue mTtl = do
  (_, tid, _) <- createTeamWithNMembers 10
  cfg <- getFeatureConfig @cfg tid
  liftIO $ cfg @?= defValue
  when (cfg.lockStatus == LockStatusLocked) $ unlockFeature @cfg tid
  let newStatus = if cfg.status == FeatureStatusEnabled then FeatureStatusDisabled else FeatureStatusEnabled
  putFeatureStatus @cfg tid newStatus mTtl !!! const 200 === statusCode
  cfg' <- getFeatureConfig @cfg tid
  liftIO $ cfg'.status @?= newStatus

testFeatureStatusWithLock ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg,
    Eq cfg,
    Show cfg
  ) =>
  TestM ()
testFeatureStatusWithLock = do
  let mTtl = Nothing -- this function can become a variant of `testFeatureStatusOptTtl` if we need one.
  (_, tid, _) <- createTeamWithNMembers 10
  getFeatureConfig @cfg tid >>= \cfg -> liftIO $ do
    cfg @?= def
    -- if either of these two lines fails, it's probably because the default is surprising.
    -- in that case, make the text more flexible.
    cfg.lockStatus @?= LockStatusLocked
    cfg.status @?= FeatureStatusDisabled

  void $ putFeatureStatusLock @cfg tid LockStatusUnlocked mTtl
  getFeatureConfig @cfg tid >>= \cfg -> liftIO $ do
    cfg.lockStatus @?= LockStatusUnlocked
    cfg.status @?= FeatureStatusDisabled

  void $ putFeatureStatus @cfg tid FeatureStatusEnabled Nothing
  getFeatureConfig @cfg tid >>= \cfg -> liftIO $ do
    cfg.lockStatus @?= LockStatusUnlocked
    cfg.status @?= FeatureStatusEnabled

  void $ putFeatureStatusLock @cfg tid LockStatusLocked mTtl
  getFeatureConfig @cfg tid >>= \cfg -> liftIO $ do
    cfg.lockStatus @?= LockStatusLocked
    cfg.status @?= FeatureStatusDisabled

  void $ putFeatureStatusLock @cfg tid LockStatusUnlocked mTtl
  getFeatureConfig @cfg tid >>= \cfg -> liftIO $ do
    cfg.lockStatus @?= LockStatusUnlocked
    cfg.status @?= FeatureStatusEnabled

testGetConsentLog :: TestM ()
testGetConsentLog = do
  (_, email) <- randomEmailUser
  -- We cannot access a consent log of an existing user, so we expect a 403
  getConsentLog email !!! const 403 === statusCode

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
  liftIO $ userId ua @?= uid

testGetUsersByEmail :: TestM ()
testGetUsersByEmail = do
  (uid, email) <- randomEmailUser
  [ua] <- getUsersByEmail email
  liftIO $ userId ua @?= uid

testUnsuspendUser :: TestM ()
testUnsuspendUser = do
  uid <- randomUser
  void $ postSupendUser uid
  do
    [ua] <- getUsersByIds [uid]
    liftIO $ ua.userStatus @?= Suspended
  void $ postUnsuspendUser uid
  do
    [ua] <- getUsersByIds [uid]
    liftIO $ ua.userStatus @?= Active

testSuspendUser :: TestM ()
testSuspendUser = do
  uid <- randomUser
  void $ postSupendUser uid
  [ua] <- getUsersByIds [uid]
  liftIO $ ua.userStatus @?= Suspended

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
    Set.fromList (userId <$> uas) @?= Set.fromList [uid1, uid2]

testGetTeamInfo :: TestM ()
testGetTeamInfo = do
  (_, tid, _) <- createTeamWithNMembers 10
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
  (_, email) <- randomEmailUser
  do
    [ua] <- getUsersByEmail email
    liftIO $ do
      ua.userStatus @?= Active
      isJust ua.userIdentity @?= True
  void $ revokeIdentity (Left email)
  do
    [ua] <- getUsersByEmail email
    liftIO $ do
      ua.userStatus @?= Active
      isJust ua.userIdentity @?= False

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

getUsersByHandles :: Text -> TestM [User]
getUsersByHandles h = do
  stern <- view tsStern
  r <- get (stern . paths ["users", "by-handles"] . queryItem "handles" (cs h) . expect2xx)
  pure $ responseJsonUnsafe r

getUsersByEmail :: EmailAddress -> TestM [User]
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

getUsersByIds :: [UserId] -> TestM [User]
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

revokeIdentity :: Either EmailAddress Phone -> TestM ()
revokeIdentity emailOrPhone = do
  s <- view tsStern
  void $ post (s . paths ["users", "revoke-identity"] . mkQueryParam emailOrPhone . expect2xx)

mkQueryParam :: Either EmailAddress Phone -> Request -> Request
mkQueryParam = \case
  Left email -> queryItem "email" (toByteString' email)
  Right phone -> queryItem "phone" (toByteString' phone)

putEmail :: UserId -> EmailUpdate -> TestM ()
putEmail uid emailUpdate = do
  s <- view tsStern
  void $ put (s . paths ["users", toByteString' uid, "email"] . json emailUpdate . expect2xx)

deleteUser :: UserId -> Either EmailAddress Phone -> TestM ()
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

deleteTeam :: TeamId -> Bool -> EmailAddress -> TestM ()
deleteTeam tid force email = do
  s <- view tsStern
  void $ delete (s . paths ["teams", toByteString' tid] . queryItem "force" (toByteString' force) . queryItem "email" (toByteString' email) . expect2xx)

ejpdInfo :: Bool -> [Handle] -> TestM EJPD.EJPDResponseBody
ejpdInfo includeContacts handles = do
  s <- view tsStern
  r <- get (s . paths ["ejpd-info"] . queryItem "include_contacts" (toByteString' includeContacts) . queryItem "handles" (toByteString' handles) . expect2xx)
  pure $ responseJsonUnsafe r

userBlacklistHead :: Either EmailAddress Phone -> TestM ResponseLBS
userBlacklistHead emailOrPhone = do
  s <- view tsStern
  Bilge.get (s . paths ["users", "blacklist"] . mkQueryParam emailOrPhone)

postUserBlacklist :: Either EmailAddress Phone -> TestM ()
postUserBlacklist emailOrPhone = do
  s <- view tsStern
  void $ post (s . paths ["users", "blacklist"] . mkQueryParam emailOrPhone . expect2xx)

deleteUserBlacklist :: Either EmailAddress Phone -> TestM ()
deleteUserBlacklist emailOrPhone = do
  s <- view tsStern
  void $ delete (s . paths ["users", "blacklist"] . mkQueryParam emailOrPhone . expect2xx)

getTeamInfoByMemberEmail :: EmailAddress -> TestM TeamInfo
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
  TestM (LockableFeature cfg)
getFeatureConfig tid = do
  s <- view tsStern
  r <- get (s . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg] . expect2xx)
  pure $ responseJsonUnsafe r

putFeatureStatus ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg
  ) =>
  TeamId ->
  FeatureStatus ->
  Maybe FeatureTTL ->
  TestM ResponseLBS
putFeatureStatus tid status mTtl = do
  s <- view tsStern
  put (s . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg] . queryItem "status" (toByteString' status) . mkTtlQueryParam mTtl . contentJson)
  where
    mkTtlQueryParam :: Maybe FeatureTTL -> Request -> Request
    mkTtlQueryParam = maybe id (queryItem "ttl" . toByteString')

putFeatureStatusLock ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg
  ) =>
  TeamId ->
  LockStatus ->
  Maybe FeatureTTL ->
  TestM ResponseLBS
putFeatureStatusLock tid lockStatus mTtl = do
  s <- view tsStern
  put
    ( s
        . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg, "lockOrUnlock"]
        . queryItem "lock-status" (toByteString' lockStatus)
        . mkTtlQueryParam mTtl
        . contentJson
        . expect2xx
    )
  where
    mkTtlQueryParam :: Maybe FeatureTTL -> Request -> Request
    mkTtlQueryParam = maybe id (queryItem "ttl" . toByteString')

putFeatureConfig ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg,
    ToJSON (LockableFeature cfg)
  ) =>
  TeamId ->
  LockableFeature cfg ->
  TestM ResponseLBS
putFeatureConfig tid cfg = do
  s <- view tsStern
  put (s . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg] . json cfg . contentJson)

getSearchVisibility :: TeamId -> TestM TeamSearchVisibilityView
getSearchVisibility tid = do
  s <- view tsStern
  r <- get (s . paths ["teams", toByteString' tid, "search-visibility"] . expect2xx)
  pure $ responseJsonUnsafe r

putSearchVisibility :: TeamId -> TeamSearchVisibility -> TestM ()
putSearchVisibility tid vis = do
  s <- view tsStern
  void $ put (s . paths ["teams", toByteString' tid, "search-visibility"] . json vis . expect2xx)

getConsentLog :: EmailAddress -> TestM ResponseLBS
getConsentLog email = do
  s <- view tsStern
  get (s . paths ["i", "consent"] . queryItem "email" (toByteString' email))

getUserMetaInfo :: UserId -> TestM UserMetaInfo
getUserMetaInfo uid = do
  s <- view tsStern
  r <- post (s . paths ["i", "user", "meta-info"] . queryItem "id" (toByteString' uid) . expect2xx)
  pure $ responseJsonUnsafe r

unlockFeature ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    Typeable cfg,
    IsFeatureConfig cfg,
    ToJSON (LockableFeature cfg)
  ) =>
  TeamId ->
  TestM ()
unlockFeature tid = do
  g <- view tsGalley
  void $ put (g . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg, "unlocked"] . expect2xx)

putUserProperty :: UserId -> PropertyKey -> Value -> TestM ()
putUserProperty uid k v = do
  b <- view tsBrig
  void $ put (b . paths ["properties", toByteString' k] . json v . zUser uid . zConn "123" . expect2xx)

getSsoDomainRedirect :: ByteString -> TestM (Maybe Value)
getSsoDomainRedirect domain = do
  s <- view tsStern
  r <- get (s . path "sso-domain-redirect" . query [("domain", Just domain)] . expect2xx)
  pure $ responseJsonUnsafe r

putSsoDomainRedirect :: ByteString -> Text -> Text -> TestM ()
putSsoDomainRedirect domain (cs -> configurl) (cs -> welcomeurl) = do
  s <- view tsStern
  r <-
    put
      ( s
          . path "sso-domain-redirect"
          . query
            [ ("domain", Just domain),
              ("configurl", Just configurl),
              ("welcomeurl", Just welcomeurl)
            ]
          . expect2xx
      )
  pure $ responseJsonUnsafe r

deleteSsoDomainRedirect :: ByteString -> TestM ()
deleteSsoDomainRedirect domain = do
  s <- view tsStern
  void $ delete (s . path "sso-domain-redirect" . query [("domain", Just domain)] . expect2xx)

registerOAuthClient :: OAuthClientConfig -> TestM OAuthClientCredentials
registerOAuthClient cfg = do
  s <- view tsStern
  r <- post (s . paths ["i", "oauth", "clients"] . json cfg . expect2xx)
  pure $ responseJsonUnsafe r

getOAuthClient' :: OAuthClientId -> TestM ResponseLBS
getOAuthClient' cid = do
  s <- view tsStern
  get (s . paths ["i", "oauth", "clients", toByteString' cid])

getOAuthClient :: OAuthClientId -> TestM OAuthClientConfig
getOAuthClient cid = do
  s <- view tsStern
  r <- get (s . paths ["i", "oauth", "clients", toByteString' cid] . expect2xx)
  pure $ responseJsonUnsafe r

updateOAuthClient :: OAuthClientId -> OAuthClientConfig -> TestM ()
updateOAuthClient cid cfg = do
  s <- view tsStern
  void $ put (s . paths ["i", "oauth", "clients", toByteString' cid] . json cfg . expect2xx)

deleteOAuthClient :: OAuthClientId -> TestM ()
deleteOAuthClient cid = do
  s <- view tsStern
  void $ delete (s . paths ["i", "oauth", "clients", toByteString' cid] . expect2xx)

testDomainRegistration :: TestM ()
testDomainRegistration = do
  s <- view tsStern
  dom <- (<> ".example.com") . cs . show <$> liftIO nextRandom
  void $ post (s . paths ["domain-registration", dom, "lock"] . expect2xx)
  void $ get (s . paths ["domain-registration", dom] . expect2xx)
  void $ post (s . paths ["domain-registration", dom, "unlock"] . expect2xx)
  void $ post (s . paths ["domain-registration", dom, "preauthorize"] . expect2xx)
  void $ post (s . paths ["domain-registration", dom, "unauthorize"] . expect2xx)
  void $ delete (s . paths ["domain-registration", dom] . expect2xx)
  void $ get (s . paths ["domain-registration", dom] . expect4xx)
  let upd = DomainRegistrationUpdate NoRegistration Allowed
  void $ put (s . paths ["domain-registration", dom] . json upd . expect2xx)
  void $ get (s . paths ["domain-registration", dom] . expect2xx)
