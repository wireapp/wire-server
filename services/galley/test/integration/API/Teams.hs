{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module API.Teams
  ( tests,
  )
where

import API.SQS
import API.Util hiding (deleteTeam)
import qualified API.Util as Util
import qualified API.Util.TeamFeature as Util
import Bilge hiding (head, timeout)
import Bilge.Assert
import Brig.Types.Intra (fromAccountStatusResp)
import qualified Brig.Types.Intra as Brig
import Control.Arrow ((>>>))
import Control.Lens hiding ((#), (.=))
import Control.Monad.Catch
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Code as Code
import Data.Csv (FromNamedRecord (..), decodeByName)
import qualified Data.Currency as Currency
import Data.Default
import Data.Id
import Data.Json.Util hiding ((#))
import qualified Data.LegalHold as LH
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List1 hiding (head)
import qualified Data.List1 as List1
import qualified Data.Map as Map
import Data.Misc (HttpsUrl, PlainTextPassword (..), mkHttpsUrl)
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID
import qualified Data.UUID.Util as UUID
import qualified Data.UUID.V1 as UUID
import qualified Data.Vector as V
import GHC.TypeLits (KnownSymbol)
import qualified Galley.Env as Galley
import Galley.Options (optSettings, setEnableIndexedBillingTeamMembers, setFeatureFlags, setMaxConvSize, setMaxFanoutSize)
import Galley.Types.Conversations.Roles
import Galley.Types.Teams
import Galley.Types.Teams.Intra as TeamsIntra
import Imports
import Network.HTTP.Types.Status (status403)
import qualified Network.Wai.Utilities.Error as Error
import qualified Network.Wai.Utilities.Error as Wai
import qualified Proto.TeamEvents as E
import qualified Proto.TeamEvents_Fields as E
import qualified SAML2.WebSSO.Types as SAML
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import UnliftIO (mapConcurrently)
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Internal.Notification hiding (target)
import Wire.API.Team
import Wire.API.Team.Export (TeamExportUser (..))
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.Member
import qualified Wire.API.Team.Member as Member
import qualified Wire.API.Team.Member as TM
import qualified Wire.API.Team.Member as Teams
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import qualified Wire.API.User as Public
import qualified Wire.API.User as U
import qualified Wire.API.User.Client as C
import qualified Wire.API.User.Client.Prekey as PC

tests :: IO TestSetup -> TestTree
tests s =
  testGroup "Teams API" $
    [ test s "create team" testCreateTeam,
      test s "GET /teams (deprecated)" testGetTeams,
      test s "create binding team with currency" testCreateBindingTeamWithCurrency,
      testGroup "List Team Members" $
        [ test s "a member should be able to list their team" testListTeamMembersDefaultLimit,
          let numMembers = 5
           in test
                s
                ("admins should be able to get a csv stream with their team (" <> show numMembers <> " members)")
                (testListTeamMembersCsv numMembers),
          test s "the list should be limited to the number requested (hard truncation is not tested here)" testListTeamMembersTruncated,
          test s "pagination" testListTeamMembersPagination
        ],
      testGroup "List Team Members (by ids)" $
        [ test s "a member should be able to list their team" testListTeamMembersDefaultLimitByIds,
          test s "id list length limit is enforced" testListTeamMembersTruncatedByIds
        ],
      testGroup "List Team members unchecked" $
        [test s "the list should be truncated" testUncheckedListTeamMembers],
      test s "enable/disable SSO" testEnableSSOPerTeam,
      test s "enable/disable Custom Search Visibility" testEnableTeamSearchVisibilityPerTeam,
      test s "create 1-1 conversation between non-team members (fail)" testCreateOne2OneFailForNonTeamMembers,
      test s "create 1-1 conversation between binding team members" (testCreateOne2OneWithMembers RoleMember),
      test s "create 1-1 conversation between binding team members as partner" (testCreateOne2OneWithMembers RoleExternalPartner),
      test s "poll team-level event queue" testTeamQueue,
      test s "add new team member internal" testAddTeamMemberInternal,
      test s "remove aka delete team member (binding, owner has passwd)" (testRemoveBindingTeamMember True),
      test s "remove aka delete team member (binding, owner has no passwd)" (testRemoveBindingTeamMember False),
      test s "remove aka delete team owner (binding)" testRemoveBindingTeamOwner,
      test s "add team conversation (no role as argument)" testAddTeamConvLegacy,
      test s "add team conversation with role" testAddTeamConvWithRole,
      test s "add team conversation as partner (fail)" testAddTeamConvAsExternalPartner,
      test s "add team MLS conversation" testCreateTeamMLSConv,
      -- Queue is emptied here to ensure that lingering events do not affect other tests
      test s "add team member to conversation without connection" (testAddTeamMemberToConv >> ensureQueueEmpty),
      test s "update conversation as member" (testUpdateTeamConv RoleMember roleNameWireAdmin),
      test s "update conversation as partner" (testUpdateTeamConv RoleExternalPartner roleNameWireMember),
      test s "delete binding team internal single member" testDeleteBindingTeamSingleMember,
      test s "delete binding team internal no members" testDeleteBindingTeamNoMembers,
      test s "delete binding team more than one member" testDeleteBindingTeamMoreThanOneMember,
      test s "delete binding team (owner has passwd)" (testDeleteBindingTeam True),
      test s "delete binding team (owner has no passwd)" (testDeleteBindingTeam False),
      testGroup
        "delete team - verification code"
        [ test s "success" testDeleteTeamVerificationCodeSuccess,
          test s "wrong code" testDeleteTeamVerificationCodeWrongCode,
          test s "missing code" testDeleteTeamVerificationCodeMissingCode,
          test s "expired code" testDeleteTeamVerificationCodeExpiredCode
        ],
      test s "delete team conversation" testDeleteTeamConv,
      test s "update team data" testUpdateTeam,
      test s "update team data icon validation" testUpdateTeamIconValidation,
      test s "update team member" testUpdateTeamMember,
      test s "update team status" testUpdateTeamStatus,
      -- Queue is emptied here to ensure that lingering events do not affect other tests
      test s "team tests around truncation limits - no events, too large team" (testTeamAddRemoveMemberAboveThresholdNoEvents >> ensureQueueEmpty),
      test s "send billing events to owners even in large teams" testBillingInLargeTeam,
      test s "send billing events to some owners in large teams (indexedBillingTeamMembers disabled)" testBillingInLargeTeamWithoutIndexedBillingTeamMembers,
      testGroup "broadcast" $
        [ (BroadcastLegacyBody, BroadcastJSON),
          (BroadcastLegacyQueryParams, BroadcastJSON),
          (BroadcastLegacyBody, BroadcastProto),
          (BroadcastQualified, BroadcastProto)
        ]
          <&> \(api, ty) ->
            let bcast = def {bAPI = api, bType = ty}
             in testGroup
                  (broadcastAPIName api <> " - " <> broadcastTypeName ty)
                  [ test s "message" (postCryptoBroadcastMessage bcast),
                    test s "filtered only, too large team" (postCryptoBroadcastMessageFilteredTooLargeTeam bcast),
                    test s "report missing in body" (postCryptoBroadcastMessageReportMissingBody bcast),
                    test s "redundant or missing" (postCryptoBroadcastMessage2 bcast),
                    test s "no-team" (postCryptoBroadcastMessageNoTeam bcast),
                    test s "100 (or max conns)" (postCryptoBroadcastMessage100OrMaxConns bcast)
                  ]
    ]

timeout :: WS.Timeout
timeout = 3 # Second

testCreateTeam :: TestM ()
testCreateTeam = do
  owner <- Util.randomUser
  tid <- Util.createBindingTeamInternal "foo" owner
  team <- Util.getTeam owner tid
  assertQueue "create team" tActivate
  liftIO $ assertEqual "owner" owner (team ^. teamCreator)
  assertQueueEmpty

testGetTeams :: TestM ()
testGetTeams = do
  owner <- Util.randomUser
  let getTeams' = Util.getTeams owner
  getTeams' [] >>= checkTeamList Nothing
  tid <- Util.createBindingTeamInternal "foo" owner <* assertQueue "create team" tActivate
  wrongTid <- (Util.randomUser >>= Util.createBindingTeamInternal "foobar") <* assertQueue "create team" tActivate
  getTeams' [] >>= checkTeamList (Just tid)
  getTeams' [("size", Just "1")] >>= checkTeamList (Just tid)
  getTeams' [("ids", Just $ toByteString' tid)] >>= checkTeamList (Just tid)
  getTeams' [("ids", Just $ toByteString' tid <> "," <> toByteString' wrongTid)] >>= checkTeamList (Just tid)
  -- these two queries do not yield responses that are equivalent to the old wai route API
  getTeams' [("ids", Just $ toByteString' wrongTid)] >>= checkTeamList (Just tid)
  getTeams' [("start", Just $ toByteString' tid)] >>= checkTeamList (Just tid)
  where
    checkTeamList :: Maybe TeamId -> TeamList -> TestM ()
    checkTeamList mbTid tl = liftIO $ do
      let teams = tl ^. teamListTeams
      assertEqual "teamListHasMore" False (tl ^. teamListHasMore)
      case mbTid of
        Just tid -> assertEqual "teamId" tid (Imports.head teams ^. teamId)
        Nothing -> assertEqual "teams size" 0 (length teams)

testCreateBindingTeamWithCurrency :: TestM ()
testCreateBindingTeamWithCurrency = do
  _owner <- Util.randomUser
  _ <- Util.createBindingTeamInternal "foo" _owner
  -- Backwards compatible
  assertQueue "create team" (tActivateWithCurrency Nothing)
  -- Ensure currency is properly journaled
  _owner <- Util.randomUser
  _ <- Util.createBindingTeamInternalWithCurrency "foo" _owner Currency.USD
  assertQueue "create team" (tActivateWithCurrency $ Just Currency.USD)

testListTeamMembersDefaultLimit :: TestM ()
testListTeamMembersDefaultLimit = do
  (owner, tid, [member1, member2]) <- Util.createBindingTeamWithNMembers 2
  listFromServer <- Util.getTeamMembers owner tid
  liftIO $
    assertEqual
      "list members"
      (Set.fromList [owner, member1, member2])
      (Set.fromList (map (^. Teams.userId) $ listFromServer ^. teamMembers))
  liftIO $
    assertBool
      "member list indicates that there are no more members"
      (listFromServer ^. teamMemberListType == ListComplete)

-- | for ad-hoc load-testing, set @numMembers@ to, say, 10k and see what
-- happens.  but please don't give that number to our ci!  :)
-- for additional tests of the CSV download particularly with SCIM users, please refer to 'Test.Spar.Scim.UserSpec'
testListTeamMembersCsv :: HasCallStack => Int -> TestM ()
testListTeamMembersCsv numMembers = do
  let teamSize = numMembers + 1

  (owner, tid, mbs) <- Util.createBindingTeamWithNMembersWithHandles True numMembers
  let numClientMappings = Map.fromList $ (owner : mbs) `zip` (cycle [1, 2, 3] :: [Int])
  addClients numClientMappings
  resp <- Util.getTeamMembersCsv owner tid
  let rbody = fromMaybe (error "no body") . responseBody $ resp
  usersInCsv <- either (error "could not decode csv") pure (decodeCSV @TeamExportUser rbody)
  liftIO $ do
    assertEqual "total number of team members" teamSize (length usersInCsv)
    assertEqual "owners in team" 1 (countOn tExportRole (Just RoleOwner) usersInCsv)
    assertEqual "members in team" numMembers (countOn tExportRole (Just RoleMember) usersInCsv)

  do
    let someUsersInCsv = take 50 usersInCsv
        someHandles = tExportHandle <$> someUsersInCsv
    users <- Util.getUsersByHandle (catMaybes someHandles)
    mbrs <- view teamMembers <$> Util.bulkGetTeamMembers owner tid (U.userId <$> users)

    let check :: (Show a, Eq a) => String -> (TeamExportUser -> Maybe a) -> UserId -> Maybe a -> IO ()
        check msg getTeamExportUserAttr uid userAttr = do
          assertBool msg (isJust userAttr)
          assertEqual (msg <> ": " <> show uid) 1 (countOn getTeamExportUserAttr userAttr usersInCsv)

    liftIO . forM_ (zip users mbrs) $ \(user, mbr) -> do
      assertEqual "user/member id match" (U.userId user) (mbr ^. TM.userId)
      check "tExportDisplayName" (Just . tExportDisplayName) (U.userId user) (Just $ U.userDisplayName user)
      check "tExportEmail" tExportEmail (U.userId user) (U.userEmail user)

    liftIO . forM_ (zip3 someUsersInCsv users mbrs) $ \(export, user, mbr) -> do
      -- FUTUREWORK: there are a lot of cases we don't cover here (manual invitation, saml, other roles, ...).
      assertEqual ("tExportDisplayName: " <> show (U.userId user)) (U.userDisplayName user) (tExportDisplayName export)
      assertEqual ("tExportHandle: " <> show (U.userId user)) (U.userHandle user) (tExportHandle export)
      assertEqual ("tExportEmail: " <> show (U.userId user)) (U.userEmail user) (tExportEmail export)
      assertEqual ("tExportRole: " <> show (U.userId user)) (permissionsRole $ view permissions mbr) (tExportRole export)
      assertEqual ("tExportCreatedOn: " <> show (U.userId user)) (snd <$> view invitation mbr) (tExportCreatedOn export)
      assertEqual ("tExportInvitedBy: " <> show (U.userId user)) Nothing (tExportInvitedBy export)
      assertEqual ("tExportIdpIssuer: " <> show (U.userId user)) (userToIdPIssuer user) (tExportIdpIssuer export)
      assertEqual ("tExportManagedBy: " <> show (U.userId user)) (U.userManagedBy user) (tExportManagedBy export)
      assertEqual ("tExportUserId: " <> show (U.userId user)) (U.userId user) (tExportUserId export)
      assertEqual "tExportNumDevices: " (Map.findWithDefault (-1) (U.userId user) numClientMappings) (tExportNumDevices export)
  where
    userToIdPIssuer :: HasCallStack => U.User -> Maybe HttpsUrl
    userToIdPIssuer usr = case (U.userIdentity >=> U.ssoIdentity) usr of
      Just (U.UserSSOId (SAML.UserRef (SAML.Issuer issuer) _)) -> either (const $ error "shouldn't happen") Just $ mkHttpsUrl issuer
      Just _ -> Nothing
      Nothing -> Nothing

    decodeCSV :: FromNamedRecord a => LByteString -> Either String [a]
    decodeCSV bstr = decodeByName bstr <&> (snd >>> V.toList)

    countOn :: Eq b => (a -> b) -> b -> [a] -> Int
    countOn prop val xs = sum $ fmap (bool 0 1 . (== val) . prop) xs

    addClients :: Map.Map UserId Int -> TestM ()
    addClients xs = forM_ (Map.toList xs) addClientForUser

    addClientForUser :: (UserId, Int) -> TestM ()
    addClientForUser (uid, n) = forM_ [0 .. (n - 1)] (addClient uid)

    addClient :: UserId -> Int -> TestM ()
    addClient uid i = do
      brig <- viewBrig
      post (brig . paths ["i", "clients", toByteString' uid] . contentJson . json (newClient (someLastPrekeys !! i)) . queryItem "skip_reauth" "true") !!! const 201 === statusCode

    newClient :: PC.LastPrekey -> C.NewClient
    newClient lpk = C.newClient C.PermanentClientType lpk

testListTeamMembersPagination :: TestM ()
testListTeamMembersPagination = do
  (owner, tid, _) <- Util.createBindingTeamWithNMembers 18
  allMembers <- Util.getTeamMembersPaginated owner tid 100 Nothing
  liftIO $ do
    let actualTeamSize = length (rpResults allMembers)
    let expectedTeamSize = 19
    assertEqual ("expected team size of 19 (18 members + 1 owner), but got " <> show actualTeamSize) expectedTeamSize actualTeamSize
  page1 <- Util.getTeamMembersPaginated owner tid 5 Nothing
  check 1 5 True page1
  page2 <- Util.getTeamMembersPaginated owner tid 5 (Just . rpPagingState $ page1)
  check 2 5 True page2
  page3 <- Util.getTeamMembersPaginated owner tid 5 (Just . rpPagingState $ page2)
  check 3 5 True page3
  page4 <- Util.getTeamMembersPaginated owner tid 5 (Just . rpPagingState $ page3)
  check 4 4 False page4
  where
    check :: Int -> Int -> Bool -> ResultPage -> TestM ()
    check n expectedSize expectedHasMore page = liftIO $ do
      let actualSize = length (rpResults page)
      assertEqual ("page " <> show n <> ": expected " <> show expectedSize <> " members, but got " <> show actualSize) expectedSize actualSize
      let actualHasMore = rpHasMore page
      assertEqual ("page " <> show n <> " (hasMore): expected " <> show expectedHasMore <> ", but got" <> "") expectedHasMore actualHasMore

testListTeamMembersTruncated :: TestM ()
testListTeamMembersTruncated = do
  (owner, tid, _) <- Util.createBindingTeamWithNMembers 4
  listFromServer <- Util.getTeamMembersTruncated owner tid 2
  liftIO $
    assertEqual
      "member list is not limited to the requested number"
      2
      (length $ listFromServer ^. teamMembers)
  liftIO $
    assertBool
      "member list does not indicate that there are more members"
      (listFromServer ^. teamMemberListType == ListTruncated)

testListTeamMembersDefaultLimitByIds :: TestM ()
testListTeamMembersDefaultLimitByIds = do
  (owner, tid, [member1, member2]) <- Util.createBindingTeamWithNMembers 2
  (_, _, [alien]) <- Util.createBindingTeamWithNMembers 1
  let phantom :: UserId = read "686f427a-7e56-11ea-a639-07a531a95937"
  check owner tid [owner, member1, member2] [owner, member1, member2]
  check owner tid [member1, member2] [member1, member2]
  check owner tid [member1] [member1]
  check owner tid [] [] -- a bit silly, but hey.
  check owner tid [alien] []
  check owner tid [phantom] []
  check owner tid [owner, alien, phantom] [owner]
  where
    check :: HasCallStack => UserId -> TeamId -> [UserId] -> [UserId] -> TestM ()
    check owner tid uidsIn uidsOut = do
      listFromServer <- Util.bulkGetTeamMembers owner tid uidsIn
      liftIO $
        assertEqual
          "list members"
          (Set.fromList uidsOut)
          (Set.fromList (map (^. userId) $ listFromServer ^. teamMembers))
      liftIO $
        assertBool
          "has_more is always false"
          (listFromServer ^. teamMemberListType == ListComplete)

testListTeamMembersTruncatedByIds :: TestM ()
testListTeamMembersTruncatedByIds = do
  (owner, tid, mems) <- Util.createBindingTeamWithNMembers 4
  Util.bulkGetTeamMembersTruncated owner tid (owner : mems) 3 !!! do
    const 400 === statusCode
    const "too-many-uids" === Error.label . responseJsonUnsafeWithMsg "error label"

testUncheckedListTeamMembers :: TestM ()
testUncheckedListTeamMembers = do
  (_, tid, _) <- Util.createBindingTeamWithNMembers 4
  listFromServer <- Util.getTeamMembersInternalTruncated tid 2
  liftIO $
    assertEqual
      "member list is not limited to the requested number"
      2
      (length $ listFromServer ^. teamMembers)
  liftIO $
    assertBool
      "member list does not indicate that there are more members"
      (listFromServer ^. teamMemberListType == ListTruncated)

testEnableSSOPerTeam :: TestM ()
testEnableSSOPerTeam = do
  owner <- Util.randomUser
  tid <- Util.createBindingTeamInternal "foo" owner
  assertQueue "create team" tActivate
  let check :: HasCallStack => String -> Public.FeatureStatus -> TestM ()
      check msg enabledness = do
        status :: Public.WithStatusNoLock Public.SSOConfig <- responseJsonUnsafe <$> (getSSOEnabledInternal tid <!! testResponse 200 Nothing)
        let statusValue = Public.wssStatus status
        liftIO $ assertEqual msg enabledness statusValue
  let putSSOEnabledInternalCheckNotImplemented :: HasCallStack => TestM ()
      putSSOEnabledInternalCheckNotImplemented = do
        g <- viewGalley
        Wai.Error status label _ _ <-
          responseJsonUnsafe
            <$> put
              ( g
                  . paths ["i", "teams", toByteString' tid, "features", "sso"]
                  . json (Public.WithStatusNoLock Public.FeatureStatusDisabled Public.SSOConfig Public.FeatureTTLUnlimited)
              )
        liftIO $ do
          assertEqual "bad status" status403 status
          assertEqual "bad label" "not-implemented" label
  featureSSO <- view (tsGConf . optSettings . setFeatureFlags . flagSSO)
  case featureSSO of
    FeatureSSOEnabledByDefault -> check "Teams should start with SSO enabled" Public.FeatureStatusEnabled
    FeatureSSODisabledByDefault -> check "Teams should start with SSO disabled" Public.FeatureStatusDisabled
  putSSOEnabledInternal tid Public.FeatureStatusEnabled
  check "Calling 'putEnabled True' should enable SSO" Public.FeatureStatusEnabled
  putSSOEnabledInternalCheckNotImplemented

testEnableTeamSearchVisibilityPerTeam :: TestM ()
testEnableTeamSearchVisibilityPerTeam = do
  (tid, owner, member : _) <- Util.createBindingTeamWithMembers 2
  let check :: String -> Public.FeatureStatus -> TestM ()
      check msg enabledness = do
        g <- viewGalley
        status :: Public.WithStatusNoLock Public.SearchVisibilityAvailableConfig <- responseJsonUnsafe <$> (Util.getTeamSearchVisibilityAvailableInternal g tid <!! testResponse 200 Nothing)
        let statusValue = Public.wssStatus status

        liftIO $ assertEqual msg enabledness statusValue
  let putSearchVisibilityCheckNotAllowed :: TestM ()
      putSearchVisibilityCheckNotAllowed = do
        g <- viewGalley
        Wai.Error status label _ _ <- responseJsonUnsafe <$> putSearchVisibility g owner tid SearchVisibilityNoNameOutsideTeam
        liftIO $ do
          assertEqual "bad status" status403 status
          assertEqual "bad label" "team-search-visibility-not-enabled" label
  let getSearchVisibilityCheck :: TeamSearchVisibility -> TestM ()
      getSearchVisibilityCheck vis = do
        g <- viewGalley
        getSearchVisibility g owner tid !!! do
          const 200 === statusCode
          const (Just (TeamSearchVisibilityView vis)) === responseJsonUnsafe

  Util.withCustomSearchFeature FeatureTeamSearchVisibilityAvailableByDefault $ do
    g <- viewGalley
    check "Teams should start with Custom Search Visibility enabled" Public.FeatureStatusEnabled
    putSearchVisibility g owner tid SearchVisibilityNoNameOutsideTeam !!! const 204 === statusCode
    putSearchVisibility g owner tid SearchVisibilityStandard !!! const 204 === statusCode
  Util.withCustomSearchFeature FeatureTeamSearchVisibilityUnavailableByDefault $ do
    check "Teams should start with Custom Search Visibility disabled" Public.FeatureStatusDisabled
    putSearchVisibilityCheckNotAllowed

  g <- viewGalley
  Util.putTeamSearchVisibilityAvailableInternal g tid Public.FeatureStatusEnabled
  -- Nothing was set, default value
  getSearchVisibilityCheck SearchVisibilityStandard
  putSearchVisibility g owner tid SearchVisibilityNoNameOutsideTeam !!! testResponse 204 Nothing
  getSearchVisibilityCheck SearchVisibilityNoNameOutsideTeam
  -- Check only admins can change the setting
  putSearchVisibility g member tid SearchVisibilityStandard !!! testResponse 403 (Just "operation-denied")
  getSearchVisibilityCheck SearchVisibilityNoNameOutsideTeam
  -- Members can also see it?
  getSearchVisibility g member tid !!! testResponse 200 Nothing
  -- Once we disable the feature, team setting is back to the default value
  Util.putTeamSearchVisibilityAvailableInternal g tid Public.FeatureStatusDisabled
  getSearchVisibilityCheck SearchVisibilityStandard

testCreateOne2OneFailForNonTeamMembers :: TestM ()
testCreateOne2OneFailForNonTeamMembers = do
  owner <- Util.randomUser
  let p1 = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember]
  let p2 = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember, AddTeamMember]
  mem1 <- newTeamMember' p1 <$> Util.randomUser
  mem2 <- newTeamMember' p2 <$> Util.randomUser
  Util.connectUsers owner (list1 (mem1 ^. userId) [mem2 ^. userId])
  -- Both have a binding team but not the same team
  owner1 <- Util.randomUser
  tid1 <- Util.createBindingTeamInternal "foo" owner1
  assertQueue "create team" tActivate
  owner2 <- Util.randomUser
  void $ Util.createBindingTeamInternal "foo" owner2
  assertQueue "create another team" tActivate
  Util.createOne2OneTeamConv owner1 owner2 Nothing tid1 !!! do
    const 403 === statusCode
    const "non-binding-team-members" === (Error.label . responseJsonUnsafeWithMsg "error label")

testCreateOne2OneWithMembers ::
  HasCallStack =>
  -- | Role of the user who creates the conversation
  Role ->
  TestM ()
testCreateOne2OneWithMembers (rolePermissions -> perms) = do
  c <- view tsCannon
  (owner, tid) <- Util.createBindingTeam
  mem1 <- newTeamMember' perms <$> Util.randomUser
  WS.bracketR c (mem1 ^. userId) $ \wsMem1 -> do
    Util.addTeamMemberInternal tid (mem1 ^. userId) (mem1 ^. permissions) (mem1 ^. invitation)
    checkTeamMemberJoin tid (mem1 ^. userId) wsMem1
    assertQueue "team member join" $ tUpdate 2 [owner]
  void $ retryWhileN 10 repeatIf (Util.createOne2OneTeamConv owner (mem1 ^. userId) Nothing tid)
  -- Recreating a One2One is a no-op, returns a 200
  Util.createOne2OneTeamConv owner (mem1 ^. userId) Nothing tid !!! const 200 === statusCode
  where
    repeatIf :: ResponseLBS -> Bool
    repeatIf r = statusCode r /= 201

-- | At the time of writing this test, the only event sent to this queue is 'MemberJoin'.
testTeamQueue :: TestM ()
testTeamQueue = do
  (owner, tid) <- createBindingTeam
  eventually $ do
    queue <- getTeamQueue owner Nothing Nothing False
    liftIO $ assertEqual "team queue: []" [] (snd <$> queue)

  mem1 :: UserId <- view userId <$> addUserToTeam owner tid
  eventually $ do
    queue1 <- getTeamQueue owner Nothing Nothing False
    queue2 <- getTeamQueue mem1 Nothing Nothing False
    liftIO $ assertEqual "team queue: owner sees [mem1]" [mem1] (snd <$> queue1)
    liftIO $ assertEqual "team queue: mem1 sees the same thing" queue1 queue2

  mem2 :: UserId <- view userId <$> addUserToTeam owner tid
  eventually $ do
    -- known 'NotificationId's
    [(n1, u1), (n2, u2)] <- getTeamQueue owner Nothing Nothing False
    liftIO $ assertEqual "team queue: queue0" (mem1, mem2) (u1, u2)
    queue1 <- getTeamQueue owner (Just n1) Nothing False
    queue2 <- getTeamQueue owner (Just n2) Nothing False
    liftIO $ assertEqual "team queue: from 1" [mem1, mem2] (snd <$> queue1)
    liftIO $ assertEqual "team queue: from 2" [mem2] (snd <$> queue2)

  do
    -- unknown old 'NotificationId'
    let Just n1 = Id <$> UUID.fromText "615c4e38-950d-11ea-b0fc-7b04ea9f81c0"
    queue <- getTeamQueue owner (Just n1) Nothing False
    liftIO $ assertEqual "team queue: from old unknown" (snd <$> queue) [mem1, mem2]

  do
    -- unknown younger 'NotificationId'
    [(Id n1, _), (Id n2, _)] <- getTeamQueue owner Nothing Nothing False
    nu <-
      -- create new UUIDv1 in the gap between n1, n2.
      let Just time1 = UUID.extractTime n1
          Just time2 = UUID.extractTime n2
          timeu = time1 + (time2 - time1) `div` 2
       in Id . fromJust . (`UUID.setTime` timeu) . fromJust <$> liftIO UUID.nextUUID
    queue <- getTeamQueue owner (Just nu) Nothing False
    liftIO $ assertEqual "team queue: from old unknown" (snd <$> queue) [mem2]

  mem3 :: UserId <- view userId <$> addUserToTeam owner tid
  mem4 :: UserId <- view userId <$> addUserToTeam owner tid
  eventually $ do
    -- response size limit
    [_, (n2, _), _, _] <- getTeamQueue owner Nothing Nothing False
    getTeamQueue' owner Nothing (Just (-1)) False !!! const 400 === statusCode
    getTeamQueue' owner Nothing (Just 0) False !!! const 400 === statusCode
    queue1 <- getTeamQueue owner (Just n2) (Just (1, True)) False
    queue2 <- getTeamQueue owner (Just n2) (Just (2, False)) False
    queue3 <- getTeamQueue owner (Just n2) (Just (3, False)) False
    queue4 <- getTeamQueue owner Nothing (Just (1, True)) False
    liftIO $ assertEqual "team queue: size limit 1" (snd <$> queue1) [mem2, mem3]
    liftIO $ assertEqual "team queue: size limit 2" (snd <$> queue2) [mem2, mem3, mem4]
    liftIO $ assertEqual "team queue: size limit 3" (snd <$> queue3) [mem2, mem3, mem4]
    liftIO $ assertEqual "team queue: size limit 1, no start id" (snd <$> queue4) [mem1]

  ensureQueueEmpty

testAddTeamMemberInternal :: TestM ()
testAddTeamMemberInternal = do
  c <- view tsCannon
  (owner, tid) <- createBindingTeam
  let p1 = Util.symmPermissions [GetBilling] -- permissions are irrelevant on internal endpoint
  mem1 <- newTeamMember' p1 <$> Util.randomUser
  WS.bracketRN c [owner, mem1 ^. userId] $ \[wsOwner, wsMem1] -> do
    Util.addTeamMemberInternal tid (mem1 ^. userId) (mem1 ^. permissions) (mem1 ^. invitation)
    liftIO . void $ mapConcurrently (checkJoinEvent tid (mem1 ^. userId)) [wsOwner, wsMem1]
    assertQueue "team member join" $ tUpdate 2 [owner]
  void $ Util.getTeamMemberInternal tid (mem1 ^. userId)

testRemoveBindingTeamMember :: Bool -> TestM ()
testRemoveBindingTeamMember ownerHasPassword = do
  localDomain <- viewFederationDomain
  g <- viewGalley
  c <- view tsCannon
  -- Owner who creates the team must have an email, This is why we run all tests with a second
  -- owner
  (ownerWithPassword, tid) <- Util.createBindingTeam
  ownerMem <-
    if ownerHasPassword
      then Util.addUserToTeam ownerWithPassword tid
      else Util.addUserToTeamWithSSO True tid
  Util.makeOwner ownerWithPassword ownerMem tid
  let owner = view userId ownerMem
  ensureQueueEmpty
  refreshIndex
  mext <- Util.randomUser
  mem1 <- Util.addUserToTeam owner tid
  assertQueue "team member join" $ tUpdate 3 [ownerWithPassword, owner]
  refreshIndex
  Util.connectUsers owner (singleton mext)
  cid1 <- Util.createTeamConv owner tid [mem1 ^. userId, mext] (Just "blaa") Nothing Nothing
  when ownerHasPassword $ do
    -- Deleting from a binding team with empty body is invalid
    delete
      ( g
          . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
          . zUser owner
          . zConn "conn"
      )
      !!! const 400
        === statusCode
    -- Deleting from a binding team without a password is forbidden
    delete
      ( g
          . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
          . zUser owner
          . zConn "conn"
          . json (newTeamMemberDeleteData Nothing)
      )
      !!! do
        const 403 === statusCode
        const "access-denied" === (Error.label . responseJsonUnsafeWithMsg "error label")
  -- Deleting from a binding team with wrong password
  delete
    ( g
        . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
        . zUser owner
        . zConn "conn"
        . json (newTeamMemberDeleteData (Just $ PlainTextPassword "wrong passwd"))
    )
    !!! do
      const 403 === statusCode
      const "access-denied" === (Error.label . responseJsonUnsafeWithMsg "error label")
  -- Mem1 is still part of Wire
  Util.ensureDeletedState False owner (mem1 ^. userId)
  WS.bracketR2 c owner mext $ \(wsOwner, wsMext) -> do
    if ownerHasPassword
      then do
        delete
          ( g
              . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
              . zUser owner
              . zConn "conn"
              . json (newTeamMemberDeleteData (Just $ Util.defPassword))
          )
          !!! const 202
            === statusCode
      else do
        -- Deleting from a binding team without a password is fine if the owner is
        -- authenticated, but has none.
        delete
          ( g
              . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
              . zUser owner
              . zConn "conn"
              . json (newTeamMemberDeleteData Nothing)
          )
          !!! const 202
            === statusCode
    checkTeamMemberLeave tid (mem1 ^. userId) wsOwner
    checkConvMemberLeaveEvent (Qualified cid1 localDomain) (Qualified (mem1 ^. userId) localDomain) wsMext
    assertQueue "team member leave" $ tUpdate 2 [ownerWithPassword, owner]
    WS.assertNoEvent timeout [wsMext]
    -- Mem1 is now gone from Wire
    Util.ensureDeletedState True owner (mem1 ^. userId)

testRemoveBindingTeamOwner :: TestM ()
testRemoveBindingTeamOwner = do
  (ownerA, tid) <- Util.createBindingTeam
  refreshIndex
  ownerB <-
    view userId <$> Util.addUserToTeamWithRole (Just RoleOwner) ownerA tid
  assertQueue "Add owner" $ tUpdate 2 [ownerA, ownerB]
  refreshIndex
  ownerWithoutEmail <- do
    -- users must have a 'UserIdentity', or @get /i/users@ won't find it, so we use
    -- 'UserSSOId'.
    mem <- Util.addUserToTeamWithSSO False tid
    refreshIndex
    assertQueue "Add user with SSO" $ tUpdate 3 [ownerA, ownerB]
    Util.makeOwner ownerA mem tid
    pure $ view userId mem
  assertQueue "Promote user to owner" $ tUpdate 3 [ownerA, ownerB, ownerWithoutEmail]
  admin <-
    view userId <$> Util.addUserToTeamWithRole (Just RoleAdmin) ownerA tid
  assertQueue "Add admin" $ tUpdate 4 [ownerA, ownerB, ownerWithoutEmail]
  refreshIndex
  -- non-owner can NOT delete owner
  check tid admin ownerWithoutEmail (Just Util.defPassword) (Just "access-denied")
  assertQueueEmpty
  -- owners can NOT delete themselves
  check tid ownerA ownerA (Just Util.defPassword) (Just "access-denied")
  assertQueueEmpty
  check tid ownerWithoutEmail ownerWithoutEmail Nothing (Just "access-denied")
  assertQueueEmpty
  -- owners can delete other owners (no matter who has emails)
  check tid ownerWithoutEmail ownerA Nothing Nothing
  assertQueue "Remove ownerA" $ tUpdate 3 [ownerB, ownerWithoutEmail]
  Util.waitForMemberDeletion ownerB tid ownerA
  refreshIndex
  check tid ownerB ownerWithoutEmail (Just Util.defPassword) Nothing
  assertQueue ("Remove ownerWithoutEmail: " <> show ownerWithoutEmail <> ", ownerA: " <> show ownerA) $ tUpdateUncertainCount [2, 3] [ownerB]
  where
    check :: HasCallStack => TeamId -> UserId -> UserId -> Maybe PlainTextPassword -> Maybe LText -> TestM ()
    check tid deleter deletee pass maybeError = do
      g <- viewGalley
      delete
        ( g
            . paths ["teams", toByteString' tid, "members", toByteString' deletee]
            . zUser deleter
            . zConn "conn"
            . json (newTeamMemberDeleteData pass)
        )
        !!! case maybeError of
          Nothing ->
            const 202 === statusCode
          Just label -> do
            const 403 === statusCode
            const label === (Error.label . responseJsonUnsafeWithMsg "error label")

testAddTeamConvLegacy :: TestM ()
testAddTeamConvLegacy = do
  c <- view tsCannon
  (owner, tid) <- Util.createBindingTeam
  extern <- Util.randomUser
  let p = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember]
  mem1 <- newTeamMember' p <$> Util.randomUser
  mem2 <- newTeamMember' p <$> Util.randomUser
  Util.connectUsers owner (list1 (mem1 ^. userId) [extern, mem2 ^. userId])
  allUserIds <- for [owner, extern, mem1 ^. userId, mem2 ^. userId] $
    \u -> Qualified u <$> viewFederationDomain
  WS.bracketRN c (qUnqualified <$> allUserIds) $ \wss -> do
    cid <- Util.createTeamConvLegacy owner tid (qUnqualified <$> allUserIds) (Just "blaa")
    mapM_ (checkConvCreateEvent cid) wss
    -- All members become admin by default
    mapM_ (assertConvMemberWithRole roleNameWireAdmin cid) allUserIds
  ensureQueueEmpty

testAddTeamConvWithRole :: TestM ()
testAddTeamConvWithRole = do
  c <- view tsCannon
  (tid, owner, mem2 : _) <- Util.createBindingTeamWithMembers 2
  qOwner <- Qualified owner <$> viewFederationDomain
  extern <- Util.randomUser
  qExtern <- Qualified extern <$> viewFederationDomain
  Util.connectUsers owner (list1 extern [])
  Util.connectUsers mem2 (list1 extern [])
  WS.bracketRN c [owner, extern, mem2] $ \[wsOwner, wsExtern, wsMem2] -> do
    -- Regular conversation:
    cid2 <- Util.createTeamConvWithRole owner tid [extern] (Just "blaa") Nothing Nothing roleNameWireAdmin
    checkConvCreateEvent cid2 wsOwner
    checkConvCreateEvent cid2 wsExtern
    mapM_ (assertConvMemberWithRole roleNameWireAdmin cid2) [qOwner, qExtern]
    -- Regular conversation (using member role for participants):
    cid3 <- Util.createTeamConvWithRole owner tid [extern] (Just "blaa") Nothing Nothing roleNameWireMember
    checkConvCreateEvent cid3 wsOwner
    checkConvCreateEvent cid3 wsExtern
    assertConvMemberWithRole roleNameWireAdmin cid3 qOwner
    assertConvMemberWithRole roleNameWireMember cid3 qExtern
    -- mem2 is not a conversation member and no longer receives
    -- an event that a new team conversation has been created

    mem1 <- Util.addUserToTeam owner tid
    checkTeamMemberJoin tid (mem1 ^. userId) wsOwner
    checkTeamMemberJoin tid (mem1 ^. userId) wsMem2
    -- ... but not to regular ones.
    Util.assertNotConvMember (mem1 ^. userId) cid2
    -- there is another team update event in the queue, so we clean it up
    ensureQueueEmpty

testCreateTeamMLSConv :: TestM ()
testCreateTeamMLSConv = do
  c <- view tsCannon
  (owner, tid) <- Util.createBindingTeam
  lOwner <- flip toLocalUnsafe owner <$> viewFederationDomain
  extern <- Util.randomUser
  WS.bracketR2 c owner extern $ \(wsOwner, wsExtern) -> do
    lConvId <-
      Util.createMLSTeamConv
        lOwner
        (newClientId 0)
        tid
        mempty
        (Just "Team MLS conversation")
        Nothing
        Nothing
        Nothing
        Nothing
    Right conv <- responseJsonError <$> getConv owner (tUnqualified lConvId)
    liftIO $ do
      assertEqual "protocol mismatch" ProtocolMLSTag (protocolTag (cnvProtocol conv))
    checkConvCreateEvent (tUnqualified lConvId) wsOwner
    WS.assertNoEvent (2 # Second) [wsExtern]

testAddTeamConvAsExternalPartner :: TestM ()
testAddTeamConvAsExternalPartner = do
  (owner, tid) <- Util.createBindingTeam
  memMember1 <- Util.addUserToTeamWithRole (Just RoleMember) owner tid
  assertQueue "team member join 2" $ tUpdate 2 [owner]
  refreshIndex
  memMember2 <- Util.addUserToTeamWithRole (Just RoleMember) owner tid
  assertQueue "team member join 3" $ tUpdate 3 [owner]
  refreshIndex
  memExternalPartner <- Util.addUserToTeamWithRole (Just RoleExternalPartner) owner tid
  assertQueue "team member join 4" $ tUpdate 4 [owner]
  refreshIndex
  let acc = Just $ Set.fromList [InviteAccess, CodeAccess]
  Util.createTeamConvAccessRaw
    (memExternalPartner ^. userId)
    tid
    [memMember1 ^. userId, memMember2 ^. userId]
    (Just "blaa")
    acc
    (Just (Set.fromList [TeamMemberAccessRole]))
    Nothing
    Nothing
    !!! do
      const 403 === statusCode
      const "operation-denied" === (Error.label . responseJsonUnsafeWithMsg "error label")

testAddTeamMemberToConv :: TestM ()
testAddTeamMemberToConv = do
  personalUser <- Util.randomUser
  (ownerT1, qOwnerT1) <- Util.randomUserTuple
  let p = Util.symmPermissions [DoNotUseDeprecatedAddRemoveConvMember]
  mem1T1 <- Util.randomUser
  qMem1T1 <- Qualified mem1T1 <$> viewFederationDomain
  mem2T1 <- Util.randomUser
  qMem2T1 <- Qualified mem2T1 <$> viewFederationDomain

  let pEmpty = Util.symmPermissions []
  mem3T1 <- Util.randomUser
  qMem3T1 <- Qualified mem3T1 <$> viewFederationDomain

  mem4T1 <- newTeamMember' pEmpty <$> Util.randomUser
  qMem4T1 <- Qualified (mem4T1 ^. userId) <$> viewFederationDomain
  (ownerT2, qOwnerT2) <- Util.randomUserTuple
  mem1T2 <- newTeamMember' p <$> Util.randomUser
  qMem1T2 <- Qualified (mem1T2 ^. userId) <$> viewFederationDomain
  Util.connectUsers ownerT1 (list1 mem1T1 [mem2T1, mem3T1, ownerT2, personalUser])
  tidT1 <- createBindingTeamInternal "foo" ownerT1
  do
    Util.addTeamMemberInternal tidT1 mem1T1 p Nothing
    Util.addTeamMemberInternal tidT1 mem2T1 p Nothing
    Util.addTeamMemberInternal tidT1 mem3T1 pEmpty Nothing
  tidT2 <- Util.createBindingTeamInternal "foo" ownerT2
  Util.addTeamMemberInternal tidT2 (mem1T2 ^. userId) (mem1T2 ^. permissions) (mem1T2 ^. invitation)
  -- Team owners create new regular team conversation:
  cidT1 <- Util.createTeamConv ownerT1 tidT1 [] (Just "blaa") Nothing Nothing
  qcidT1 <- Qualified cidT1 <$> viewFederationDomain
  cidT2 <- Util.createTeamConv ownerT2 tidT2 [] (Just "blaa") Nothing Nothing
  qcidT2 <- Qualified cidT2 <$> viewFederationDomain
  cidPersonal <- decodeConvId <$> Util.postConv personalUser [] (Just "blaa") [] Nothing Nothing
  qcidPersonal <- Qualified cidPersonal <$> viewFederationDomain
  -- NOTE: This functionality was _changed_ as there was no need for it...
  -- mem1T1 (who is *not* a member of the new conversation) can *not* add other team members
  -- despite being a team member and having the permission `DoNotUseDeprecatedAddRemoveConvMember`.
  Util.assertNotConvMember mem1T1 cidT1
  Util.postMembers mem1T1 (pure qMem2T1) qcidT1 !!! const 404 === statusCode
  Util.assertNotConvMember mem2T1 cidT1
  -- OTOH, mem3T1 _can_ add another team member despite lacking the required team permission
  -- since conversation roles trump any team roles. Note that all users are admins by default
  Util.assertConvMember qOwnerT1 cidT1
  Util.postMembers ownerT1 (pure qMem2T1) qcidT1 !!! const 200 === statusCode
  Util.assertConvMember qMem2T1 cidT1
  -- The following tests check the logic: users can add other users to a conversation
  -- iff:
  --    - *the adding user is connected to the users being added*
  --    OR
  --    - *the adding user is part of the team of the users being added*

  -- Now we add someone from T2 that we are connected to
  Util.postMembers ownerT1 (pure qOwnerT2) qcidT1 !!! const 200 === statusCode
  Util.assertConvMember qOwnerT2 cidT1
  -- And they can add their own team members
  Util.postMembers ownerT2 (pure qMem1T2) qcidT1 !!! const 200 === statusCode
  Util.assertConvMember qMem1T2 cidT1
  -- Still, they cannot add random members without a connection from T1, despite the conversation being "hosted" there
  Util.postMembers ownerT2 (pure qMem4T1) qcidT1 !!! const 403 === statusCode
  Util.assertNotConvMember (mem4T1 ^. userId) cidT1
  -- Now let's look at convs hosted on team2
  -- ownerT2 *is* connected to ownerT1
  Util.postMembers ownerT2 (pure qOwnerT1) qcidT2 !!! const 200 === statusCode
  Util.assertConvMember qOwnerT1 cidT2
  -- and mem1T2 is on the same team, but mem1T1 is *not*
  Util.postMembers ownerT2 (qMem1T2 :| [qMem1T1]) qcidT2 !!! const 403 === statusCode
  Util.assertNotConvMember mem1T1 cidT2
  Util.assertNotConvMember (mem1T2 ^. userId) cidT2
  -- mem1T2 is on the same team, so that is fine too
  Util.postMembers ownerT2 (pure qMem1T2) qcidT2 !!! const 200 === statusCode
  Util.assertConvMember qMem1T2 cidT2
  -- ownerT2 is *NOT* connected to mem3T1 and not on the same team, so should not be allowed to add
  Util.postMembers ownerT2 (pure qMem3T1) qcidT2 !!! const 403 === statusCode
  Util.assertNotConvMember mem3T1 cidT2
  -- For personal conversations, same logic applies

  -- Can add connected users
  Util.postMembers personalUser (pure qOwnerT1) qcidPersonal !!! const 200 === statusCode
  Util.assertConvMember qOwnerT1 cidPersonal
  -- Can *not* add users that are *not* connected
  Util.postMembers personalUser (pure qOwnerT2) qcidPersonal !!! const 403 === statusCode
  Util.assertNotConvMember ownerT2 cidPersonal
  -- Users of the same team can add one another
  Util.postMembers ownerT1 (pure qMem1T1) qcidPersonal !!! const 200 === statusCode
  Util.assertConvMember qMem1T1 cidPersonal
  -- Users can not add across teams if *not* connected
  Util.postMembers mem1T1 (pure qOwnerT2) qcidPersonal !!! const 403 === statusCode
  Util.assertNotConvMember ownerT2 cidPersonal
  -- Users *can* add across teams if *connected*
  Util.postMembers ownerT1 (pure qOwnerT2) qcidPersonal !!! const 200 === statusCode
  Util.assertConvMember qOwnerT2 cidPersonal

testUpdateTeamConv ::
  -- | Team role of the user who creates the conversation
  Role ->
  -- | Conversation role of the user who creates the conversation
  RoleName ->
  TestM ()
testUpdateTeamConv _ convRole = do
  (tid, owner, member : _) <- Util.createBindingTeamWithMembers 2
  cid <- Util.createTeamConvWithRole owner tid [member] (Just "gossip") Nothing Nothing convRole
  resp <- updateTeamConv member cid (ConversationRename "not gossip")
  -- FUTUREWORK: Ensure that the team role _really_ does not matter
  liftIO $ assertEqual "status conv" convRoleCheck (statusCode resp)
  where
    convRoleCheck = if isActionAllowed ModifyConversationName convRole == Just True then 200 else 403

testDeleteBindingTeamSingleMember :: TestM ()
testDeleteBindingTeamSingleMember = do
  g <- viewGalley
  c <- view tsCannon
  (owner, tid) <- Util.createBindingTeam
  other <- Util.addUserToTeam owner tid
  ensureQueueEmpty
  refreshIndex
  -- Useful for tests
  extern <- Util.randomUser
  delete
    ( g
        . paths ["/i/teams", toByteString' tid]
        . zUser owner
        . zConn "conn"
        . json (newTeamDeleteData (Just $ Util.defPassword))
    )
    !!! do
      const 403 === statusCode
      const "not-one-member-team" === (Error.label . responseJsonUnsafeWithMsg "error label when deleting a team")
  delete
    ( g
        . paths ["teams", toByteString' tid, "members", toByteString' (other ^. userId)]
        . zUser owner
        . zConn "conn"
        . json
          ( newTeamMemberDeleteData (Just Util.defPassword)
          )
    )
    !!! const 202
      === statusCode
  assertQueue "team member leave 1" $ tUpdate 1 [owner]
  -- Async things are hard...
  void $
    retryWhileN
      10
      (/= Just True)
      (getDeletedState extern (other ^. userId))

  void . WS.bracketRN c [owner, extern] $ \[wsOwner, wsExtern] -> do
    delete
      ( g
          . paths ["/i/teams", toByteString' tid]
          . zUser owner
          . zConn "conn"
      )
      !!! const 202
        === statusCode
    checkUserDeleteEvent owner wsOwner

    WS.assertNoEvent (1 # Second) [wsExtern]
    -- Note that given the async nature of team deletion, we may
    -- have other events in the queue (such as TEAM_UPDATE)
    tryAssertQueue 10 "team delete, should be there" tDelete

  Util.ensureDeletedState True extern owner
  -- Ensure users are marked as deleted; since we already
  -- received the event, should _really_ be deleted
  -- Let's clean the queue, just in case
  ensureQueueEmpty

testDeleteBindingTeamNoMembers :: TestM ()
testDeleteBindingTeamNoMembers = do
  g <- viewGalley
  (owner, tid) <- Util.createBindingTeam
  deleteUser owner !!! const 200 === statusCode
  ensureQueueEmpty
  refreshIndex
  delete (g . paths ["/i/teams", toByteString' tid]) !!! const 202 === statusCode
  tryAssertQueue 10 "team delete, should be there" tDelete
  ensureQueueEmpty

testDeleteBindingTeamMoreThanOneMember :: TestM ()
testDeleteBindingTeamMoreThanOneMember = do
  g <- viewGalley
  b <- viewBrig
  c <- view tsCannon
  (alice, tid, members) <- Util.createBindingTeamWithNMembers 10
  ensureQueueEmpty
  void . WS.bracketRN c (alice : members) $ \(wsAlice : wsMembers) -> do
    -- deleting a team with more than one member should be forbidden
    delete (g . paths ["/i/teams", toByteString' tid]) !!! do
      const 403 === statusCode
      const "not-one-member-team" === (Error.label . responseJsonUnsafeWithMsg "error label when deleting a team")
    -- now try again with the 'force' query flag, which should work
    delete (g . paths ["/i/teams", toByteString' tid] . queryItem "force" "true") !!! do
      const 202 === statusCode
    checkUserDeleteEvent alice wsAlice
    zipWithM_ checkUserDeleteEvent members wsMembers
    tryAssertQueue 10 "team delete, should be there" tDelete

  let ensureDeleted :: UserId -> TestM ()
      ensureDeleted uid = do
        resp <- get (b . paths ["/i/users", toByteString' uid, "status"]) <!! const 200 === statusCode
        let mbStatus = fmap fromAccountStatusResp . responseJsonUnsafe $ resp
        liftIO $ mbStatus @?= Just Brig.Deleted

  ensureDeleted alice
  for_ members ensureDeleted
  ensureQueueEmpty

testDeleteTeamVerificationCodeSuccess :: TestM ()
testDeleteTeamVerificationCodeSuccess = do
  g <- viewGalley
  (owner, tid) <- Util.createBindingTeam'
  let Just email = U.userEmail owner
  setFeatureLockStatus @Public.SndFactorPasswordChallengeConfig tid Public.LockStatusUnlocked
  setTeamSndFactorPasswordChallenge tid Public.FeatureStatusEnabled
  generateVerificationCode $ Public.SendVerificationCode Public.DeleteTeam email
  code <- getVerificationCode (U.userId owner) Public.DeleteTeam
  delete
    ( g
        . paths ["teams", toByteString' tid]
        . zUser (U.userId owner)
        . zConn "conn"
        . json (newTeamDeleteDataWithCode (Just Util.defPassword) (Just code))
    )
    !!! do
      const 202 === statusCode
  tryAssertQueue 10 "team delete, should be there" tDelete
  assertQueueEmpty

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that team cannot be deleted with missing second factor email verification code when this feature is enabled
testDeleteTeamVerificationCodeMissingCode :: TestM ()
testDeleteTeamVerificationCodeMissingCode = do
  g <- viewGalley
  (owner, tid) <- Util.createBindingTeam'
  setFeatureLockStatus @Public.SndFactorPasswordChallengeConfig tid Public.LockStatusUnlocked
  setTeamSndFactorPasswordChallenge tid Public.FeatureStatusEnabled
  let Just email = U.userEmail owner
  generateVerificationCode $ Public.SendVerificationCode Public.DeleteTeam email
  delete
    ( g
        . paths ["teams", toByteString' tid]
        . zUser (U.userId owner)
        . zConn "conn"
        . json (newTeamMemberDeleteData (Just Util.defPassword))
    )
    !!! do
      const 403 === statusCode
      const "code-authentication-required" === (Error.label . responseJsonUnsafeWithMsg "error label")
  assertQueueEmpty

-- @END

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that team cannot be deleted with expired second factor email verification code when this feature is enabled
testDeleteTeamVerificationCodeExpiredCode :: TestM ()
testDeleteTeamVerificationCodeExpiredCode = do
  g <- viewGalley
  (owner, tid) <- Util.createBindingTeam'
  setFeatureLockStatus @Public.SndFactorPasswordChallengeConfig tid Public.LockStatusUnlocked
  setTeamSndFactorPasswordChallenge tid Public.FeatureStatusEnabled
  let Just email = U.userEmail owner
  generateVerificationCode $ Public.SendVerificationCode Public.DeleteTeam email
  code <- getVerificationCode (U.userId owner) Public.DeleteTeam
  -- wait > 5 sec for the code to expire (assumption: setVerificationTimeout in brig.integration.yaml is set to <= 5 sec)
  threadDelay $ (5 * 1000 * 1000) + 600 * 1000
  delete
    ( g
        . paths ["teams", toByteString' tid]
        . zUser (U.userId owner)
        . zConn "conn"
        . json (newTeamDeleteDataWithCode (Just Util.defPassword) (Just code))
    )
    !!! do
      const 403 === statusCode
      const "code-authentication-failed" === (Error.label . responseJsonUnsafeWithMsg "error label")
  assertQueueEmpty

-- @END

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that team cannot be deleted with wrong second factor email verification code when this feature is enabled
testDeleteTeamVerificationCodeWrongCode :: TestM ()
testDeleteTeamVerificationCodeWrongCode = do
  g <- viewGalley
  (owner, tid) <- Util.createBindingTeam'
  setFeatureLockStatus @Public.SndFactorPasswordChallengeConfig tid Public.LockStatusUnlocked
  setTeamSndFactorPasswordChallenge tid Public.FeatureStatusEnabled
  let Just email = U.userEmail owner
  generateVerificationCode $ Public.SendVerificationCode Public.DeleteTeam email
  let wrongCode = Code.Value $ unsafeRange (fromRight undefined (validate "123456"))
  delete
    ( g
        . paths ["teams", toByteString' tid]
        . zUser (U.userId owner)
        . zConn "conn"
        . json (newTeamDeleteDataWithCode (Just Util.defPassword) (Just wrongCode))
    )
    !!! do
      const 403 === statusCode
      const "code-authentication-failed" === (Error.label . responseJsonUnsafeWithMsg "error label")
  assertQueueEmpty

-- @END

setFeatureLockStatus :: forall cfg. (Public.IsFeatureConfig cfg, KnownSymbol (Public.FeatureSymbol cfg)) => TeamId -> Public.LockStatus -> TestM ()
setFeatureLockStatus tid status = do
  g <- viewGalley
  put (g . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg, toByteString' status]) !!! const 200 === statusCode

generateVerificationCode :: Public.SendVerificationCode -> TestM ()
generateVerificationCode req = do
  brig <- viewBrig
  let js = RequestBodyLBS $ encode req
  post (brig . paths ["verification-code", "send"] . contentJson . body js) !!! const 200 === statusCode

setTeamSndFactorPasswordChallenge :: TeamId -> Public.FeatureStatus -> TestM ()
setTeamSndFactorPasswordChallenge tid status = do
  g <- viewGalley
  let js = RequestBodyLBS $ encode $ Public.WithStatusNoLock status Public.SndFactorPasswordChallengeConfig Public.FeatureTTLUnlimited
  put (g . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @Public.SndFactorPasswordChallengeConfig] . contentJson . body js) !!! const 200 === statusCode

getVerificationCode :: UserId -> Public.VerificationAction -> TestM Code.Value
getVerificationCode uid action = do
  brig <- viewBrig
  resp <-
    get (brig . paths ["i", "users", toByteString' uid, "verification-code", toByteString' action])
      <!! const 200
        === statusCode
  pure $ responseJsonUnsafe @Code.Value resp

testDeleteBindingTeam :: Bool -> TestM ()
testDeleteBindingTeam ownerHasPassword = do
  g <- viewGalley
  c <- view tsCannon
  (ownerWithPassword, tid) <- Util.createBindingTeam
  ownerMem <-
    if ownerHasPassword
      then Util.addUserToTeam ownerWithPassword tid
      else Util.addUserToTeamWithSSO True tid
  Util.makeOwner ownerWithPassword ownerMem tid
  let owner = view userId ownerMem
  ensureQueueEmpty
  refreshIndex
  mem1 <- Util.addUserToTeam owner tid
  assertQueue "team member join 3" $ tUpdate 3 [ownerWithPassword, owner]
  refreshIndex
  mem2 <- Util.addUserToTeam owner tid
  assertQueue "team member join 4" $ tUpdate 4 [ownerWithPassword, owner]
  refreshIndex
  mem3 <- Util.addUserToTeam owner tid
  assertQueue "team member join 5" $ tUpdate 5 [ownerWithPassword, owner]
  refreshIndex
  extern <- Util.randomUser
  delete
    ( g
        . paths ["teams", toByteString' tid]
        . zUser owner
        . zConn "conn"
        . json (newTeamDeleteData (Just $ PlainTextPassword "wrong passwd"))
    )
    !!! do
      const 403 === statusCode
      const "access-denied" === (Error.label . responseJsonUnsafeWithMsg "error label")
  delete
    ( g
        . paths ["teams", toByteString' tid, "members", toByteString' (mem3 ^. userId)]
        . zUser owner
        . zConn "conn"
        . json
          ( newTeamMemberDeleteData
              ( if ownerHasPassword
                  then Just Util.defPassword
                  else Nothing
              )
          )
    )
    !!! const 202
      === statusCode
  assertQueue "team member leave 1" $ tUpdate 4 [ownerWithPassword, owner]
  void . WS.bracketRN c [owner, mem1 ^. userId, mem2 ^. userId, extern] $ \[wsOwner, wsMember1, wsMember2, wsExtern] -> do
    delete
      ( g
          . paths ["teams", toByteString' tid]
          . zUser owner
          . zConn "conn"
          . json
            ( newTeamDeleteData
                ( if ownerHasPassword
                    then Just Util.defPassword
                    else Nothing
                )
            )
      )
      !!! const 202
        === statusCode
    checkUserDeleteEvent owner wsOwner
    checkUserDeleteEvent (mem1 ^. userId) wsMember1
    checkUserDeleteEvent (mem2 ^. userId) wsMember2
    checkTeamDeleteEvent tid wsOwner
    checkTeamDeleteEvent tid wsMember1
    checkTeamDeleteEvent tid wsMember2
    WS.assertNoEvent (1 # Second) [wsExtern]
    -- Note that given the async nature of team deletion, we may
    -- have other events in the queue (such as TEAM_UPDATE)
    tryAssertQueue 10 "team delete, should be there" tDelete
  forM_ [owner, mem1 ^. userId, mem2 ^. userId] $
    -- Ensure users are marked as deleted; since we already
    -- received the event, should _really_ be deleted
    Util.ensureDeletedState True extern
  -- Let's clean it up, just in case
  ensureQueueEmpty

testDeleteTeamConv :: TestM ()
testDeleteTeamConv = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  (tid, owner, _) <- Util.createBindingTeamWithMembers 2
  qOwner <- Qualified owner <$> viewFederationDomain
  let p = Util.symmPermissions [DoNotUseDeprecatedDeleteConversation]
  member <- newTeamMember' p <$> Util.randomUser
  qMember <- Qualified (member ^. userId) <$> viewFederationDomain
  Util.addTeamMemberInternal tid (member ^. userId) (member ^. permissions) Nothing
  -- get rid of team update event
  ensureQueueEmpty
  let members = [qOwner, qMember]
  extern <- Util.randomUser
  qExtern <- Qualified extern <$> viewFederationDomain
  for_ members $ \m -> Util.connectUsers (m & qUnqualified) (list1 extern [])
  cid1 <- Util.createTeamConv owner tid [] (Just "blaa") Nothing Nothing
  qcid1 <- Qualified cid1 <$> viewFederationDomain
  let access = ConversationAccessData (Set.fromList [InviteAccess, CodeAccess]) (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole])
  putAccessUpdate owner cid1 access !!! const 200 === statusCode
  code <- decodeConvCodeEvent <$> (postConvCode owner cid1 <!! const 201 === statusCode)
  cid2 <- Util.createTeamConv owner tid (qUnqualified <$> members) (Just "blup") Nothing Nothing
  Util.postMembers owner (qExtern :| [qMember]) qcid1 !!! const 200 === statusCode
  for_ (qExtern : members) $ \u -> Util.assertConvMember u cid1
  for_ members $ flip Util.assertConvMember cid2
  WS.bracketR3 c owner extern (member ^. userId) $ \(wsOwner, wsExtern, wsMember) -> do
    deleteTeamConv tid cid2 (member ^. userId)
      !!! const 200
        === statusCode

    -- We no longer send duplicate conv deletion events
    -- i.e., as both a regular "conversation.delete" to all
    -- conversation members and as "team.conversation-delete"
    -- to all team members not part of the conversation
    let qcid2 = Qualified cid2 localDomain
    checkConvDeleteEvent qcid2 wsOwner
    checkConvDeleteEvent qcid2 wsMember
    WS.assertNoEvent timeout [wsOwner, wsMember]

    deleteTeamConv tid cid1 (member ^. userId)
      !!! const 200
        === statusCode
    -- We no longer send duplicate conv deletion events
    -- i.e., as both a regular "conversation.delete" to all
    -- conversation members and as "team.conversation-delete"
    -- to all team members not part of the conversation
    checkConvDeleteEvent qcid1 wsOwner
    checkConvDeleteEvent qcid1 wsMember
    checkConvDeleteEvent qcid1 wsExtern
    WS.assertNoEvent timeout [wsOwner, wsMember, wsExtern]
  for_ [cid1, cid2] $ \x ->
    for_ [owner, member ^. userId, extern] $ \u -> do
      Util.getConv u x !!! const 404 === statusCode
      Util.assertNotConvMember u x
  postConvCodeCheck code !!! const 404 === statusCode

testUpdateTeamIconValidation :: TestM ()
testUpdateTeamIconValidation = do
  g <- viewGalley
  (tid, owner, _) <- Util.createBindingTeamWithMembers 2
  let update payload expectedStatusCode =
        put
          ( g
              . paths ["teams", toByteString' tid]
              . zUser owner
              . zConn "conn"
              . json payload
          )
          !!! const expectedStatusCode
            === statusCode
  let payloadWithInvalidIcon = object ["name" .= String "name", "icon" .= String "invalid"]
  update payloadWithInvalidIcon 400
  let payloadWithValidIcon =
        object
          [ "name" .= String "name",
            "icon" .= String "3-1-47de4580-ae51-4650-acbb-d10c028cb0ac"
          ]
  update payloadWithValidIcon 200
  let payloadSetIconToDefault = object ["icon" .= String "default"]
  update payloadSetIconToDefault 200

testUpdateTeam :: TestM ()
testUpdateTeam = do
  g <- viewGalley
  c <- view tsCannon
  (tid, owner, [member]) <- Util.createBindingTeamWithMembers 2

  let doPut :: LByteString -> Int -> TestM ()
      doPut payload code =
        put
          ( g
              . paths ["teams", toByteString' tid]
              . zUser owner
              . zConn "conn"
              . contentJson
              . body (RequestBodyLBS payload)
          )
          !!! const code
            === statusCode

  let bad = object ["name" .= T.replicate 100 "too large"]
  doPut (encode bad) 400

  let u =
        newTeamUpdateData
          & nameUpdate ?~ unsafeRange "bar"
          & iconUpdate .~ fromByteString "3-1-47de4580-ae51-4650-acbb-d10c028cb0ac"
          & iconKeyUpdate ?~ unsafeRange "yyy"
          & splashScreenUpdate .~ fromByteString "3-1-e1c89a56-882e-4694-bab3-c4f57803c57a"
  WS.bracketR2 c owner member $ \(wsOwner, wsMember) -> do
    doPut (encode u) 200
    checkTeamUpdateEvent tid u wsOwner
    checkTeamUpdateEvent tid u wsMember
    WS.assertNoEvent timeout [wsOwner, wsMember]
  t <- Util.getTeam owner tid
  liftIO $ assertEqual "teamSplashScreen" (t ^. teamSplashScreen) (fromJust $ fromByteString "3-1-e1c89a56-882e-4694-bab3-c4f57803c57a")

  do
    -- setting fields to `null` is the same as omitting the them from the update json record.
    -- ("name" is set because a completely empty update object is rejected.)
    doPut "{\"name\": \"new team name\", \"splash_screen\": null}" 200
    t' <- Util.getTeam owner tid
    liftIO $ assertEqual "teamSplashScreen" (t' ^. teamSplashScreen) (fromJust $ fromByteString "3-1-e1c89a56-882e-4694-bab3-c4f57803c57a")

  do
    -- setting splash screen to `"default"` will delete the splash screen.
    doPut "{\"splash_screen\": \"default\"}" 200
    t' <- Util.getTeam owner tid
    liftIO $ assertEqual "teamSplashScreen" (t' ^. teamSplashScreen) DefaultIcon

testTeamAddRemoveMemberAboveThresholdNoEvents :: HasCallStack => TestM ()
testTeamAddRemoveMemberAboveThresholdNoEvents = do
  localDomain <- viewFederationDomain
  o <- view tsGConf
  c <- view tsCannon
  let fanoutLimit = fromIntegral . fromRange $ Galley.currentFanoutLimit o
  (owner, tid) <- Util.createBindingTeam
  member1 <- addTeamMemberAndExpectEvent True tid owner
  -- Now last fill the team until truncationSize - 2

  replicateM_ (fanoutLimit - 4) $ Util.addUserToTeam owner tid
  (extern, qextern) <- Util.randomUserTuple
  modifyTeamDataAndExpectEvent True tid owner
  -- Let's create and remove a member
  member2 <- do
    temp <- addTeamMemberAndExpectEvent True tid owner
    Util.connectUsers extern (list1 temp [])
    removeTeamMemberAndExpectEvent True owner tid temp [extern]
    addTeamMemberAndExpectEvent True tid owner
  modifyUserProfileAndExpectEvent True owner [member1, member2]
  -- Let's connect an external to test the different behavior
  Util.connectUsers extern (list1 owner [member1, member2])
  _memLastWithFanout <- addTeamMemberAndExpectEvent True tid owner
  -- We should really wait until we see that the team is of full size
  -- Due to the async nature of pushes, waiting even a second might not
  -- be enough...
  WS.bracketR c owner $ \wsOwner -> WS.assertNoEvent (1 # Second) [wsOwner]
  -- No events are now expected

  -- Team member added also not
  _memWithoutFanout <- addTeamMemberAndExpectEvent False tid owner
  -- Team updates are not propagated
  modifyTeamDataAndExpectEvent False tid owner
  -- User event updates are not propagated in the team
  modifyUserProfileAndExpectEvent False owner [member1, member2]
  -- Let us remove 1 member that exceeds the limit, verify that team users
  -- do not get the deletion event but the connections do!
  removeTeamMemberAndExpectEvent False owner tid member2 [extern]
  -- Now we are just on the limit, events are back!
  removeTeamMemberAndExpectEvent True owner tid member1 [extern]
  -- Let's go back to having a very large team
  _memLastWithFanout <- addTeamMemberAndExpectEvent True tid owner
  -- We should really wait until we see that the team is of full size
  -- Due to the async nature of pushes, waiting even a second might not
  -- be enough...
  WS.bracketR c owner $ \wsOwner -> WS.assertNoEvent (1 # Second) [wsOwner]
  _memWithoutFanout <- addTeamMemberAndExpectEvent False tid owner
  -- Add extern to a team conversation
  cid1 <- Util.createTeamConv owner tid [] (Just "blaa") Nothing Nothing
  qcid1 <- Qualified cid1 <$> viewFederationDomain
  Util.postMembers owner (pure qextern) qcid1 !!! const 200 === statusCode
  -- Test team deletion (should contain only conv. removal and user.deletion for _non_ team members)
  deleteTeam tid owner [] [Qualified cid1 localDomain] extern
  ensureQueueEmpty
  where
    modifyUserProfileAndExpectEvent :: HasCallStack => Bool -> UserId -> [UserId] -> TestM ()
    modifyUserProfileAndExpectEvent expect target listeners = do
      c <- view tsCannon
      b <- viewBrig
      WS.bracketRN c listeners $ \wsListeners -> do
        -- Do something
        let u = U.UserUpdate (Just $ U.Name "name") Nothing Nothing Nothing
        put
          ( b
              . paths ["self"]
              . zUser target
              . zConn "conn"
              . json u
          )
          !!! const 200
            === statusCode
        if expect
          then mapM_ (checkUserUpdateEvent target) wsListeners
          else WS.assertNoEvent (1 # Second) wsListeners
    modifyTeamDataAndExpectEvent :: HasCallStack => Bool -> TeamId -> UserId -> TestM ()
    modifyTeamDataAndExpectEvent expect tid origin = do
      c <- view tsCannon
      g <- viewGalley
      let u = newTeamUpdateData & nameUpdate ?~ unsafeRange "bar"
      WS.bracketR c origin $ \wsOrigin -> do
        put
          ( g
              . paths ["teams", toByteString' tid]
              . zUser origin
              . zConn "conn"
              . json u
          )
          !!! const 200
            === statusCode
        -- Due to the fact that the team is too large, we expect no events!
        if expect
          then checkTeamUpdateEvent tid u wsOrigin
          else WS.assertNoEvent (1 # Second) [wsOrigin]
    addTeamMemberAndExpectEvent :: HasCallStack => Bool -> TeamId -> UserId -> TestM UserId
    addTeamMemberAndExpectEvent expect tid origin = do
      c <- view tsCannon
      WS.bracketR c origin $ \wsOrigin -> do
        member <- view userId <$> Util.addUserToTeam origin tid
        refreshIndex
        if expect
          then checkTeamMemberJoin tid member wsOrigin
          else WS.assertNoEvent (1 # Second) [wsOrigin]
        pure member
    removeTeamMemberAndExpectEvent :: HasCallStack => Bool -> UserId -> TeamId -> UserId -> [UserId] -> TestM ()
    removeTeamMemberAndExpectEvent expect owner tid victim others = do
      c <- view tsCannon
      g <- viewGalley
      WS.bracketRN c (owner : victim : others) $ \(wsOwner : _wsVictim : wsOthers) -> do
        delete
          ( g
              . paths ["teams", toByteString' tid, "members", toByteString' victim]
              . zUser owner
              . zConn "conn"
              . json (newTeamMemberDeleteData (Just $ Util.defPassword))
          )
          !!! const 202
            === statusCode
        if expect
          then checkTeamMemberLeave tid victim wsOwner
          else WS.assertNoEvent (1 # Second) [wsOwner]
        -- User deletion events
        mapM_ (checkUserDeleteEvent victim) wsOthers
        Util.ensureDeletedState True owner victim
    deleteTeam :: HasCallStack => TeamId -> UserId -> [UserId] -> [Qualified ConvId] -> UserId -> TestM ()
    deleteTeam tid owner otherRealUsersInTeam teamCidsThatExternBelongsTo extern = do
      c <- view tsCannon
      g <- viewGalley
      void . WS.bracketRN c (owner : extern : otherRealUsersInTeam) $ \(_wsOwner : wsExtern : _wsotherRealUsersInTeam) -> do
        delete
          ( g
              . paths ["teams", toByteString' tid]
              . zUser owner
              . zConn "conn"
              . json (newTeamDeleteData (Just Util.defPassword))
          )
          !!! const 202
            === statusCode
        for_ (owner : otherRealUsersInTeam) $ \u -> checkUserDeleteEvent u wsExtern
        -- Ensure users are marked as deleted; since we already
        -- received the event, should _really_ be deleted
        for_ (owner : otherRealUsersInTeam) $ Util.ensureDeletedState True extern
        mapM_ (flip checkConvDeleteEvent wsExtern) teamCidsThatExternBelongsTo
      -- ensure the team has a deleted status
      void $
        retryWhileN
          10
          ((/= TeamsIntra.Deleted) . TeamsIntra.tdStatus)
          (getTeamInternal tid)

testBillingInLargeTeam :: TestM ()
testBillingInLargeTeam = do
  (firstOwner, team) <- Util.createBindingTeam
  refreshIndex
  opts <- view tsGConf
  galley <- viewGalley
  let fanoutLimit = fromRange $ Galley.currentFanoutLimit opts
  allOwnersBeforeFanoutLimit <-
    foldM
      ( \billingMembers n -> do
          newBillingMemberId <- view userId <$> Util.addUserToTeamWithRole (Just RoleOwner) firstOwner team
          let allBillingMembers = newBillingMemberId : billingMembers
          assertQueue ("add " <> show n <> "th billing member: " <> show newBillingMemberId) $
            tUpdate n allBillingMembers
          refreshIndex
          pure allBillingMembers
      )
      [firstOwner]
      [2 .. (fanoutLimit + 1)]

  -- Additions after the fanout limit should still send events to all owners
  ownerFanoutPlusTwo <- view userId <$> Util.addUserToTeamWithRole (Just RoleOwner) firstOwner team
  assertQueue ("add fanoutLimit + 2nd billing member: " <> show ownerFanoutPlusTwo) $
    tUpdate (fanoutLimit + 2) (ownerFanoutPlusTwo : allOwnersBeforeFanoutLimit)
  refreshIndex

  -- Deletions after the fanout limit should still send events to all owners
  ownerFanoutPlusThree <- view userId <$> Util.addUserToTeamWithRole (Just RoleOwner) firstOwner team
  assertQueue ("add fanoutLimit + 3rd billing member: " <> show ownerFanoutPlusThree) $
    tUpdate (fanoutLimit + 3) (allOwnersBeforeFanoutLimit <> [ownerFanoutPlusTwo, ownerFanoutPlusThree])
  refreshIndex

  Util.deleteTeamMember galley team firstOwner ownerFanoutPlusThree
  assertQueue ("delete fanoutLimit + 3rd billing member: " <> show ownerFanoutPlusThree) $
    tUpdate (fanoutLimit + 2) (allOwnersBeforeFanoutLimit <> [ownerFanoutPlusTwo])
  refreshIndex

testBillingInLargeTeamWithoutIndexedBillingTeamMembers :: TestM ()
testBillingInLargeTeamWithoutIndexedBillingTeamMembers = do
  (firstOwner, team) <- Util.createBindingTeam
  refreshIndex
  opts <- view tsGConf
  galley <- viewGalley
  let withoutIndexedBillingTeamMembers =
        withSettingsOverrides (\o -> o & optSettings . setEnableIndexedBillingTeamMembers ?~ False)
  let fanoutLimit = fromRange $ Galley.currentFanoutLimit opts

  -- Billing should work properly upto fanout limit
  allOwnersBeforeFanoutLimit <-
    foldM
      ( \billingMembers n -> do
          newBillingMemberId <- randomUser
          let mem = json $ Member.mkNewTeamMember newBillingMemberId (rolePermissions RoleOwner) Nothing
          -- We cannot properly add the new owner with an invite as we don't have a way to
          -- override galley settings while making a call to brig
          withoutIndexedBillingTeamMembers $
            post (galley . paths ["i", "teams", toByteString' team, "members"] . mem)
              !!! const 200
                === statusCode
          let allBillingMembers = newBillingMemberId : billingMembers
          -- We don't make a call to brig to add member, hence the count of team is always 2
          assertQueue ("add " <> show n <> "th billing member: " <> show newBillingMemberId) $
            tUpdate 2 allBillingMembers
          refreshIndex
          pure allBillingMembers
      )
      [firstOwner]
      [2 .. (fanoutLimit + 1)]

  -- If we add another owner, one of them won't get notified
  ownerFanoutPlusTwo <- randomUser
  let memFanoutPlusTwo = json $ Member.mkNewTeamMember ownerFanoutPlusTwo (rolePermissions RoleOwner) Nothing
  -- We cannot properly add the new owner with an invite as we don't have a way to
  -- override galley settings while making a call to brig
  withoutIndexedBillingTeamMembers $ do
    g <- viewGalley
    post (g . paths ["i", "teams", toByteString' team, "members"] . memFanoutPlusTwo)
      !!! const 200
        === statusCode
  assertQueue ("add " <> show (fanoutLimit + 2) <> "th billing member: " <> show ownerFanoutPlusTwo) $
    \s maybeEvent ->
      case maybeEvent of
        Nothing -> assertFailure "Expected 1 TeamUpdate, got nothing"
        Just event -> do
          assertEqual (s <> ": eventType") E.TeamEvent'TEAM_UPDATE (event ^. E.eventType)
          assertEqual (s <> ": count") 2 (event ^. E.eventData . E.memberCount)
          let reportedBillingUserIds = mapMaybe (UUID.fromByteString . fromStrict) (event ^. E.eventData . E.billingUser)
          assertEqual (s <> ": number of billing users") (fromIntegral fanoutLimit + 1) (length reportedBillingUserIds)

  -- While members are added with indexedBillingTeamMembers disabled, new owners must still be
  -- indexed, just not used. When the feature is enabled, we should be able to send billing to
  -- all the owners
  ownerFanoutPlusThree <- view userId <$> Util.addUserToTeamWithRole (Just RoleOwner) firstOwner team
  assertQueue ("add fanoutLimit + 3rd billing member: " <> show ownerFanoutPlusThree) $
    tUpdateUncertainCount [2, 3] (allOwnersBeforeFanoutLimit <> [ownerFanoutPlusTwo, ownerFanoutPlusThree])
  refreshIndex

  -- Deletions with indexedBillingTeamMembers disabled should still remove owners from the
  -- indexed table
  withoutIndexedBillingTeamMembers $ Util.deleteTeamMember galley team firstOwner ownerFanoutPlusTwo
  ensureQueueEmpty
  Util.waitForMemberDeletion firstOwner team ownerFanoutPlusTwo

  ownerFanoutPlusFour <- view userId <$> Util.addUserToTeamWithRole (Just RoleOwner) firstOwner team
  assertQueue ("add billing member to test deletion: " <> show ownerFanoutPlusFour) $
    tUpdateUncertainCount [3, 4] (allOwnersBeforeFanoutLimit <> [ownerFanoutPlusThree, ownerFanoutPlusFour])
  refreshIndex

  -- Promotions and demotion should also be kept track of regardless of feature being enabled
  let demoteFanoutPlusThree = Member.mkNewTeamMember ownerFanoutPlusThree (rolePermissions RoleAdmin) Nothing
  withoutIndexedBillingTeamMembers $ updateTeamMember galley team firstOwner demoteFanoutPlusThree !!! const 200 === statusCode
  ensureQueueEmpty

  ownerFanoutPlusFive <- view userId <$> Util.addUserToTeamWithRole (Just RoleOwner) firstOwner team
  assertQueue ("add billing member to test demotion: " <> show ownerFanoutPlusFive) $
    tUpdateUncertainCount [4, 5] (allOwnersBeforeFanoutLimit <> [ownerFanoutPlusFour, ownerFanoutPlusFive])
  refreshIndex

  let promoteFanoutPlusThree = Member.mkNewTeamMember ownerFanoutPlusThree (rolePermissions RoleOwner) Nothing
  withoutIndexedBillingTeamMembers $ updateTeamMember galley team firstOwner promoteFanoutPlusThree !!! const 200 === statusCode
  ensureQueueEmpty

  ownerFanoutPlusSix <- view userId <$> Util.addUserToTeamWithRole (Just RoleOwner) firstOwner team
  assertQueue ("add billing member to test promotion: " <> show ownerFanoutPlusSix) $
    tUpdateUncertainCount [5, 6] (allOwnersBeforeFanoutLimit <> [ownerFanoutPlusThree, ownerFanoutPlusFour, ownerFanoutPlusFive, ownerFanoutPlusSix])
  where
    updateTeamMember g tid zusr change =
      put
        ( g
            . paths ["teams", toByteString' tid, "members"]
            . zUser zusr
            . zConn "conn"
            . json change
        )

-- | @SF.Management @TSFI.RESTfulAPI @S2
-- This test covers:
-- Promotion, demotion of team roles.
-- Demotion by superior roles is allowed.
-- Demotion by inferior roles is NOT allowed.
testUpdateTeamMember :: TestM ()
testUpdateTeamMember = do
  g <- viewGalley
  c <- view tsCannon
  (owner, tid) <- Util.createBindingTeam
  member <- Util.addUserToTeamWithRole (Just RoleAdmin) owner tid
  assertQueue "add member" $ tUpdate 2 [owner]
  refreshIndex
  -- non-owner can **NOT** demote owner
  let demoteOwner = Member.mkNewTeamMember owner (rolePermissions RoleAdmin) Nothing
  updateTeamMember g tid (member ^. userId) demoteOwner !!! do
    const 403 === statusCode
    const "access-denied" === (Error.label . responseJsonUnsafeWithMsg "error label")
  -- owner can demote non-owner
  let demoteMember = Member.mkNewTeamMember (member ^. userId) noPermissions (member ^. invitation)
  WS.bracketR2 c owner (member ^. userId) $ \(wsOwner, wsMember) -> do
    updateTeamMember g tid owner demoteMember !!! do
      const 200 === statusCode
    member' <- Util.getTeamMember owner tid (member ^. userId)
    liftIO $ assertEqual "permissions" (member' ^. permissions) (demoteMember ^. nPermissions)
    checkTeamMemberUpdateEvent tid (member ^. userId) wsOwner (pure noPermissions)
    checkTeamMemberUpdateEvent tid (member ^. userId) wsMember (pure noPermissions)
    WS.assertNoEvent timeout [wsOwner, wsMember]
  assertQueue "Member demoted" $ tUpdate 2 [owner]
  -- owner can promote non-owner
  let promoteMember = Member.mkNewTeamMember (member ^. userId) fullPermissions (member ^. invitation)
  WS.bracketR2 c owner (member ^. userId) $ \(wsOwner, wsMember) -> do
    updateTeamMember g tid owner promoteMember !!! do
      const 200 === statusCode
    member' <- Util.getTeamMember owner tid (member ^. userId)
    liftIO $ assertEqual "permissions" (member' ^. permissions) (promoteMember ^. nPermissions)
    checkTeamMemberUpdateEvent tid (member ^. userId) wsOwner (pure fullPermissions)
    checkTeamMemberUpdateEvent tid (member ^. userId) wsMember (pure fullPermissions)
    WS.assertNoEvent timeout [wsOwner, wsMember]
  assertQueue "Member promoted to owner" $ tUpdate 2 [owner, member ^. userId]
  -- owner can **NOT** demote herself, even when another owner exists
  updateTeamMember g tid owner demoteOwner !!! do
    const 403 === statusCode
  -- Now that the other member has full permissions, she can demote the owner
  WS.bracketR2 c (member ^. userId) owner $ \(wsMember, wsOwner) -> do
    updateTeamMember g tid (member ^. userId) demoteOwner !!! do
      const 200 === statusCode
    owner' <- Util.getTeamMember (member ^. userId) tid owner
    liftIO $ assertEqual "permissions" (owner' ^. permissions) (demoteOwner ^. nPermissions)
    -- owner no longer has GetPermissions, but she can still see the update because it's about her!
    checkTeamMemberUpdateEvent tid owner wsOwner (pure (rolePermissions RoleAdmin))
    checkTeamMemberUpdateEvent tid owner wsMember (pure (rolePermissions RoleAdmin))
    WS.assertNoEvent timeout [wsOwner, wsMember]
  assertQueue "Owner demoted" $ tUpdate 2 [member ^. userId]
  where
    updateTeamMember g tid zusr change =
      put
        ( g
            . paths ["teams", toByteString' tid, "members"]
            . zUser zusr
            . zConn "conn"
            . json change
        )
    checkTeamMemberUpdateEvent tid uid w mPerm = WS.assertMatch_ timeout w $ \notif -> do
      ntfTransient notif @?= False
      let e = List1.head (WS.unpackPayload notif)
      e ^. eventTeam @?= tid
      e ^. eventData @?= EdMemberUpdate uid mPerm

-- @END

testUpdateTeamStatus :: TestM ()
testUpdateTeamStatus = do
  g <- viewGalley
  (_, tid) <- Util.createBindingTeam
  -- Check for idempotency
  Util.changeTeamStatus tid TeamsIntra.Active
  assertQueueEmpty
  Util.changeTeamStatus tid TeamsIntra.Suspended
  assertQueue "suspend first time" tSuspend
  Util.changeTeamStatus tid TeamsIntra.Suspended
  assertQueueEmpty
  Util.changeTeamStatus tid TeamsIntra.Suspended
  assertQueueEmpty
  Util.changeTeamStatus tid TeamsIntra.Active
  assertQueue "activate again" tActivate
  void $
    put
      ( g
          . paths ["i", "teams", toByteString' tid, "status"]
          . json (TeamStatusUpdate TeamsIntra.Deleted Nothing)
      )
      !!! do
        const 403 === statusCode
        const "invalid-team-status-update" === (Error.label . responseJsonUnsafeWithMsg "error label")

postCryptoBroadcastMessage :: Broadcast -> TestM ()
postCryptoBroadcastMessage bcast = do
  localDomain <- viewFederationDomain
  let q :: Id a -> Qualified (Id a)
      q = (`Qualified` localDomain)
  c <- view tsCannon
  -- Team1: Alice, Bob. Team2: Charlie. Regular user: Dan. Connect Alice,Charlie,Dan
  (alice, tid) <- Util.createBindingTeam
  bob <- view userId <$> Util.addUserToTeam alice tid
  assertQueue "add bob" $ tUpdate 2 [alice]
  refreshIndex
  (charlie, _) <- Util.createBindingTeam
  refreshIndex
  ac <- Util.randomClient alice (head someLastPrekeys)
  bc <- Util.randomClient bob (someLastPrekeys !! 1)
  cc <- Util.randomClient charlie (someLastPrekeys !! 2)
  (dan, dc) <- randomUserWithClient (someLastPrekeys !! 3)
  connectUsers alice (list1 charlie [dan])
  -- A second client for Alice
  ac2 <- randomClient alice (someLastPrekeys !! 4)
  -- Complete: Alice broadcasts a message to Bob,Charlie,Dan and herself
  let t = 1 # Second -- WS receive timeout
  let msg =
        [ (alice, ac2, toBase64Text "ciphertext0"),
          (bob, bc, toBase64Text "ciphertext1"),
          (charlie, cc, toBase64Text "ciphertext2"),
          (dan, dc, toBase64Text "ciphertext3")
        ]
  WS.bracketRN c [bob, charlie, dan] $ \[wsB, wsC, wsD] ->
    -- Alice's clients 1 and 2 listen to their own messages only
    WS.bracketR (c . queryItem "client" (toByteString' ac2)) alice $ \wsA2 ->
      WS.bracketR (c . queryItem "client" (toByteString' ac)) alice $ \wsA1 -> do
        Util.postBroadcast (q alice) ac bcast {bMessage = msg} !!! do
          const 201 === statusCode
          assertBroadcastMismatch localDomain (bAPI bcast) [] [] []
        -- Bob should get the broadcast (team member of alice)
        void . liftIO $
          WS.assertMatch t wsB (wsAssertOtr (q (selfConv bob)) (q alice) ac bc (toBase64Text "ciphertext1"))
        -- Charlie should get the broadcast (contact of alice and user of teams feature)
        void . liftIO $
          WS.assertMatch t wsC (wsAssertOtr (q (selfConv charlie)) (q alice) ac cc (toBase64Text "ciphertext2"))
        -- Dan should get the broadcast (contact of alice and not user of teams feature)
        void . liftIO $
          WS.assertMatch t wsD (wsAssertOtr (q (selfConv dan)) (q alice) ac dc (toBase64Text "ciphertext3"))
        -- Alice's first client should not get the broadcast
        assertNoMsg wsA1 (wsAssertOtr (q (selfConv alice)) (q alice) ac ac (toBase64Text "ciphertext0"))
        -- Alice's second client should get the broadcast
        void . liftIO $
          WS.assertMatch t wsA2 (wsAssertOtr (q (selfConv alice)) (q alice) ac ac2 (toBase64Text "ciphertext0"))

postCryptoBroadcastMessageFilteredTooLargeTeam :: Broadcast -> TestM ()
postCryptoBroadcastMessageFilteredTooLargeTeam bcast = do
  localDomain <- viewFederationDomain
  let q :: Id a -> Qualified (Id a)
      q = (`Qualified` localDomain)
  c <- view tsCannon
  -- Team1: alice, bob and 3 unnamed
  (alice, tid) <- Util.createBindingTeam
  bob <- view userId <$> Util.addUserToTeam alice tid
  assertQueue "add bob" $ tUpdate 2 [alice]
  refreshIndex
  forM_ [3 .. 5] $ \count -> do
    void $ Util.addUserToTeam alice tid
    assertQueue "add user" $ tUpdate count [alice]
    refreshIndex
  -- Team2: charlie
  (charlie, _) <- Util.createBindingTeam
  refreshIndex
  ac <- Util.randomClient alice (head someLastPrekeys)
  bc <- Util.randomClient bob (someLastPrekeys !! 1)
  cc <- Util.randomClient charlie (someLastPrekeys !! 2)
  (dan, dc) <- randomUserWithClient (someLastPrekeys !! 3)
  connectUsers alice (list1 charlie [dan])
  -- A second client for Alice
  ac2 <- randomClient alice (someLastPrekeys !! 4)
  -- Complete: Alice broadcasts a message to Bob,Charlie,Dan and herself
  let t = 1 # Second -- WS receive timeout
  let msg =
        [ (alice, ac2, toBase64Text "ciphertext0"),
          (bob, bc, toBase64Text "ciphertext1"),
          (charlie, cc, toBase64Text "ciphertext2"),
          (dan, dc, toBase64Text "ciphertext3")
        ]
  WS.bracketRN c [bob, charlie, dan] $ \[wsB, wsC, wsD] ->
    -- Alice's clients 1 and 2 listen to their own messages only
    WS.bracketR (c . queryItem "client" (toByteString' ac2)) alice $ \wsA2 ->
      WS.bracketR (c . queryItem "client" (toByteString' ac)) alice $ \wsA1 -> do
        -- We change also max conv size due to the invariants that galley forces us to keep
        let newOpts =
              ((optSettings . setMaxFanoutSize) ?~ unsafeRange 4)
                . (optSettings . setMaxConvSize .~ 4)
        withSettingsOverrides newOpts $ do
          -- Untargeted, Alice's team is too large
          Util.postBroadcast (q alice) ac bcast {bMessage = msg} !!! do
            const 400 === statusCode
            const "too-many-users-to-broadcast" === Error.label . responseJsonUnsafeWithMsg "error label"
          -- We target the message to the 4 users, that should be fine
          let inbody = Just [alice, bob, charlie, dan]
          Util.postBroadcast (q alice) ac bcast {bReport = inbody, bMessage = msg} !!! do
            const 201 === statusCode
            assertBroadcastMismatch localDomain (bAPI bcast) [] [] []
        -- Bob should get the broadcast (team member of alice)
        void . liftIO $
          WS.assertMatch t wsB (wsAssertOtr (q (selfConv bob)) (q alice) ac bc (toBase64Text "ciphertext1"))
        -- Charlie should get the broadcast (contact of alice and user of teams feature)
        void . liftIO $
          WS.assertMatch t wsC (wsAssertOtr (q (selfConv charlie)) (q alice) ac cc (toBase64Text "ciphertext2"))
        -- Dan should get the broadcast (contact of alice and not user of teams feature)
        void . liftIO $
          WS.assertMatch t wsD (wsAssertOtr (q (selfConv dan)) (q alice) ac dc (toBase64Text "ciphertext3"))
        -- Alice's first client should not get the broadcast
        assertNoMsg wsA1 (wsAssertOtr (q (selfConv alice)) (q alice) ac ac (toBase64Text "ciphertext0"))
        -- Alice's second client should get the broadcast
        void . liftIO $
          WS.assertMatch t wsA2 (wsAssertOtr (q (selfConv alice)) (q alice) ac ac2 (toBase64Text "ciphertext0"))

postCryptoBroadcastMessageReportMissingBody :: Broadcast -> TestM ()
postCryptoBroadcastMessageReportMissingBody bcast = do
  localDomain <- viewFederationDomain
  (alice, tid) <- Util.createBindingTeam
  let qalice = Qualified alice localDomain
  bob <- view userId <$> Util.addUserToTeam alice tid
  _bc <- Util.randomClient bob (someLastPrekeys !! 1) -- this is important!
  assertQueue "add bob" $ tUpdate 2 [alice]
  refreshIndex
  ac <- Util.randomClient alice (head someLastPrekeys)
  let -- add extraneous query parameter (unless using query parameter API)
      inquery = case bAPI bcast of
        BroadcastLegacyQueryParams -> id
        _ -> queryItem "report_missing" (toByteString' alice)
      msg = [(alice, ac, "ciphertext0")]
  Util.postBroadcast qalice ac bcast {bReport = Just [bob], bMessage = msg, bReq = inquery}
    !!! const 412
      === statusCode

postCryptoBroadcastMessage2 :: Broadcast -> TestM ()
postCryptoBroadcastMessage2 bcast = do
  localDomain <- viewFederationDomain
  let q :: Id a -> Qualified (Id a)
      q = (`Qualified` localDomain)
  c <- view tsCannon
  -- Team1: Alice, Bob. Team2: Charlie. Connect Alice,Charlie
  (alice, tid) <- Util.createBindingTeam
  bob <- view userId <$> Util.addUserToTeam alice tid
  assertQueue "add bob" $ tUpdate 2 [alice]
  refreshIndex
  (charlie, _) <- Util.createBindingTeam
  refreshIndex
  ac <- Util.randomClient alice (head someLastPrekeys)
  bc <- Util.randomClient bob (someLastPrekeys !! 1)
  cc <- Util.randomClient charlie (someLastPrekeys !! 2)
  connectUsers alice (list1 charlie [])
  let t = 3 # Second -- WS receive timeout
  -- Missing charlie
  let m1 = [(bob, bc, toBase64Text "ciphertext1")]
  Util.postBroadcast (q alice) ac bcast {bMessage = m1} !!! do
    const 412 === statusCode
    assertBroadcastMismatch localDomain (bAPI bcast) [(charlie, Set.singleton cc)] [] []
  -- Complete
  WS.bracketR2 c bob charlie $ \(wsB, wsE) -> do
    let m2 = [(bob, bc, toBase64Text "ciphertext2"), (charlie, cc, toBase64Text "ciphertext2")]
    Util.postBroadcast (q alice) ac bcast {bMessage = m2} !!! do
      const 201 === statusCode
      assertBroadcastMismatch localDomain (bAPI bcast) [] [] []
    void . liftIO $
      WS.assertMatch t wsB (wsAssertOtr (q (selfConv bob)) (q alice) ac bc (toBase64Text "ciphertext2"))
    void . liftIO $
      WS.assertMatch t wsE (wsAssertOtr (q (selfConv charlie)) (q alice) ac cc (toBase64Text "ciphertext2"))
  -- Redundant self
  WS.bracketR3 c alice bob charlie $ \(wsA, wsB, wsE) -> do
    let m3 =
          [ (alice, ac, toBase64Text "ciphertext3"),
            (bob, bc, toBase64Text "ciphertext3"),
            (charlie, cc, toBase64Text "ciphertext3")
          ]
    Util.postBroadcast (q alice) ac bcast {bMessage = m3} !!! do
      const 201 === statusCode
      assertBroadcastMismatch localDomain (bAPI bcast) [] [(alice, Set.singleton ac)] []
    void . liftIO $
      WS.assertMatch t wsB (wsAssertOtr (q (selfConv bob)) (q alice) ac bc (toBase64Text "ciphertext3"))
    void . liftIO $
      WS.assertMatch t wsE (wsAssertOtr (q (selfConv charlie)) (q alice) ac cc (toBase64Text "ciphertext3"))
    -- Alice should not get it
    assertNoMsg wsA (wsAssertOtr (q (selfConv alice)) (q alice) ac ac (toBase64Text "ciphertext3"))
  -- Deleted charlie
  WS.bracketR2 c bob charlie $ \(wsB, wsE) -> do
    deleteClient charlie cc (Just defPassword) !!! const 200 === statusCode
    liftIO $
      WS.assertMatch_ (5 # WS.Second) wsE $
        wsAssertClientRemoved cc
    let m4 = [(bob, bc, toBase64Text "ciphertext4"), (charlie, cc, toBase64Text "ciphertext4")]
    Util.postBroadcast (q alice) ac bcast {bMessage = m4} !!! do
      const 201 === statusCode
      assertBroadcastMismatch localDomain (bAPI bcast) [] [] [(charlie, Set.singleton cc)]
    void . liftIO $
      WS.assertMatch t wsB (wsAssertOtr (q (selfConv bob)) (q alice) ac bc (toBase64Text "ciphertext4"))
    -- charlie should not get it
    assertNoMsg wsE (wsAssertOtr (q (selfConv charlie)) (q alice) ac cc (toBase64Text "ciphertext4"))

postCryptoBroadcastMessageNoTeam :: Broadcast -> TestM ()
postCryptoBroadcastMessageNoTeam bcast = do
  localDomain <- viewFederationDomain
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  let qalice = Qualified alice localDomain
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  connectUsers alice (list1 bob [])
  let msg = [(bob, bc, toBase64Text "ciphertext1")]
  Util.postBroadcast qalice ac bcast {bMessage = msg} !!! const 404 === statusCode

postCryptoBroadcastMessage100OrMaxConns :: Broadcast -> TestM ()
postCryptoBroadcastMessage100OrMaxConns bcast = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  let qalice = Qualified alice localDomain
  _ <- createBindingTeamInternal "foo" alice
  assertQueue "" tActivate
  ((bob, bc), others) <- createAndConnectUserWhileLimitNotReached alice (100 :: Int) [] (someLastPrekeys !! 1)
  connectUsers alice (list1 bob (fst <$> others))
  let t = 3 # Second -- WS receive timeout
  WS.bracketRN c (bob : (fst <$> others)) $ \ws -> do
    let f (u, clt) = (u, clt, toBase64Text "ciphertext")
    let msg = (bob, bc, toBase64Text "ciphertext") : (f <$> others)
    Util.postBroadcast qalice ac bcast {bMessage = msg} !!! do
      const 201 === statusCode
      assertBroadcastMismatch localDomain (bAPI bcast) [] [] []
    let qbobself = Qualified (selfConv bob) localDomain
    void . liftIO $
      WS.assertMatch t (Imports.head ws) (wsAssertOtr qbobself qalice ac bc (toBase64Text "ciphertext"))
    for_ (zip (tail ws) others) $ \(wsU, (u, clt)) -> do
      let qself = Qualified (selfConv u) localDomain
      liftIO $ WS.assertMatch t wsU (wsAssertOtr qself qalice ac clt (toBase64Text "ciphertext"))
  where
    createAndConnectUserWhileLimitNotReached alice remaining acc pk = do
      (uid, cid) <- randomUserWithClient pk
      (r1, r2) <- List1.head <$> connectUsersUnchecked alice (singleton uid)
      case (statusCode r1, statusCode r2, remaining, acc) of
        (201, 200, 0, []) -> error "Need to connect with at least 1 user"
        (201, 200, 0, x : xs) -> pure (x, xs)
        (201, 200, _, _) -> createAndConnectUserWhileLimitNotReached alice (remaining - 1) ((uid, cid) : acc) pk
        (403, 403, _, []) -> error "Need to connect with at least 1 user"
        (403, 403, _, x : xs) -> pure (x, xs)
        (xxx, yyy, _, _) -> error ("Unexpected while connecting users: " ++ show xxx ++ " and " ++ show yyy)

newTeamMember' :: Permissions -> UserId -> TeamMember
newTeamMember' perms uid = Member.mkTeamMember uid perms Nothing LH.defUserLegalHoldStatus

-- NOTE: all client functions calling @{/i,}/teams/*/features/*@ can be replaced by
-- hypothetical functions 'getTeamFeatureFlag', 'getTeamFeatureFlagInternal',
-- 'putTeamFeatureFlagInternal'.  Since these functions all work in slightly different monads
-- and with different kinds of internal checks, it's quite tedious to do so.

getSSOEnabledInternal :: HasCallStack => TeamId -> TestM ResponseLBS
getSSOEnabledInternal = Util.getTeamFeatureFlagInternal @Public.SSOConfig

putSSOEnabledInternal :: HasCallStack => TeamId -> Public.FeatureStatus -> TestM ()
putSSOEnabledInternal tid statusValue =
  void $ Util.putTeamFeatureFlagInternal @Public.SSOConfig expect2xx tid (Public.WithStatusNoLock statusValue Public.SSOConfig Public.FeatureTTLUnlimited)

getSearchVisibility :: HasCallStack => (Request -> Request) -> UserId -> TeamId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getSearchVisibility g uid tid = do
  get $
    g
      . paths ["teams", toByteString' tid, "search-visibility"]
      . zUser uid

putSearchVisibility :: HasCallStack => (Request -> Request) -> UserId -> TeamId -> TeamSearchVisibility -> (MonadIO m, MonadHttp m) => m ResponseLBS
putSearchVisibility g uid tid vis = do
  put $
    g
      . paths ["teams", toByteString' tid, "search-visibility"]
      . zUser uid
      . json (TeamSearchVisibilityView vis)

checkJoinEvent :: (MonadIO m, MonadCatch m) => TeamId -> UserId -> WS.WebSocket -> m ()
checkJoinEvent tid usr w = WS.assertMatch_ timeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventTeam @?= tid
  e ^. eventData @?= EdMemberJoin usr
