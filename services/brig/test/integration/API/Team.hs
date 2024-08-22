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

module API.Team
  ( tests,
  )
where

import API.Search.Util qualified as SearchUtil
import API.Team.Util
import API.User.Util as Util
import Bilge hiding (accept, head, timeout)
import Bilge qualified
import Bilge.Assert
import Brig.Options qualified as Opt
import Control.Arrow ((&&&))
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.Default (def)
import Data.Either.Extra (eitherToMaybe)
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldDisabled))
import Data.String.Conversions (cs)
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID (fromString)
import Data.UUID.V4 qualified as UUID
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Network.Wai.Utilities.Error qualified as Error
import Numeric.Natural (Natural)
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import URI.ByteString
import UnliftIO.Async (mapConcurrently_, pooledForConcurrentlyN_, replicateConcurrently)
import Util
import Util.AWS as Util
import Web.Cookie (parseSetCookie, setCookieName)
import Wire.API.Asset
import Wire.API.Connection
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as Team
import Wire.API.Team hiding (newTeam)
import Wire.API.Team.Feature
import Wire.API.Team.Feature qualified as Public
import Wire.API.Team.Invitation
import Wire.API.Team.Member hiding (invitation, userId)
import Wire.API.Team.Member qualified as Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.Size
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.User.Client (ClientType (PermanentClientType))

newtype TeamSizeLimit = TeamSizeLimit Word32

tests :: Opt.Opts -> Manager -> Nginz -> Brig -> Cannon -> Galley -> UserJournalWatcher -> IO TestTree
tests conf m n b c g aws = do
  let tl = TeamSizeLimit . Opt.setMaxTeamSize . Opt.optSettings $ conf
  let it = Opt.setTeamInvitationTimeout . Opt.optSettings $ conf
  pure $
    testGroup
      "team"
      [ testGroup "invitation" $
          [ test m "post /teams/:tid/invitations - 201" $ testInvitationEmail b,
            test m "get /teams/:tid/invitations/:iid - 200" $ testGetInvitation b,
            test m "delete /teams/:tid/invitations/:iid - 200" $ testDeleteInvitation b,
            test m "post /teams/:tid/invitations - invitation url" $ testInvitationUrl conf b,
            test m "post /teams/:tid/invitations - no invitation url" $ testNoInvitationUrl conf b,
            test m "post /teams/:tid/invitations - email lookup" $ testInvitationEmailLookup b,
            test m "post /teams/:tid/invitations - email lookup nginz" $ testInvitationEmailLookupNginz b n,
            test m "post /teams/:tid/invitations - email lookup register" $ testInvitationEmailLookupRegister b,
            test m "post /teams/:tid/invitations - 403 no permission" $ testInvitationNoPermission b,
            test m "post /teams/:tid/invitations - 403 too many pending" $ testInvitationTooManyPending conf b tl,
            test m "post /teams/:tid/invitations - roles" $ testInvitationRoles b g,
            test m "post /register - 201 accepted" $ testInvitationEmailAccepted b g,
            test m "post /register - 201 accepted (with domain blocking customer extension)" $ testInvitationEmailAcceptedInBlockedDomain conf b g,
            test m "post /register user & team - 201 accepted" $ testCreateTeam b g aws,
            test m "post /register user & team - 201 preverified" $ testCreateTeamPreverified b g aws,
            test m "post /register - 400 no passwordless" $ testTeamNoPassword b,
            test m "post /register - 400 code already used" $ testInvitationCodeExists b,
            test m "post /register - 400 bad code" $ testInvitationInvalidCode b,
            test m "post /register - 400 no wireless" $ testInvitationCodeNoIdentity b,
            test m "post /register - 400 mutually exclusive" $ testInvitationMutuallyExclusive b,
            test m "post /register - 403 too many members" $ testInvitationTooManyMembers b g tl,
            test m "get /teams/:tid/invitations - 200 (paging)" $ testInvitationPaging conf b,
            test m "get /teams/:tid/invitations/info - 200" $ testInvitationInfo b,
            test m "get /teams/:tid/invitations/info - 400" $ testInvitationInfoBadCode b,
            test m "get /teams/:tid/invitations/info - 400 expired" $ testInvitationInfoExpired b it,
            -- "get /i/teams/invitations/by-email?email=..." is tested in 'testCreateUserNoIdP', 'testCreateUserTimeout'
            -- in spar's integration tests, module "Test.Spar.Scim.UserSpec"
            test m "post /i/teams/:tid/suspend - 200" $ testSuspendTeam b,
            test m "put /self - 200 update events" $ testUpdateEvents b c,
            test m "delete /self - 200 (ensure no orphan teams)" $ testDeleteTeamUser b g,
            test m "post /connections - 403 (same binding team)" $ testConnectionSameTeam b
          ],
        testGroup "sso" $
          [ test m "post /i/users  - 201 internal-SSO" $ testCreateUserInternalSSO b g,
            test m "delete /i/users/:uid - 202 internal-SSO (ensure no orphan teams)" $ testDeleteUserSSO b g,
            test m "get /i/teams/:tid/is-team-owner/:uid" $ testSSOIsTeamOwner b g,
            test m "2FA disabled for SSO user" $ test2FaDisabledForSsoUser b g
          ],
        testGroup "size" $
          [ test m "get /i/teams/:tid/size" $ testTeamSizeInternal b,
            test m "get /teams/:tid/size" $ testTeamSizePublic b
          ]
      ]

testTeamSizeInternal :: Brig -> Http ()
testTeamSizeInternal brig = do
  testTeamSize brig (\tid _ -> brig . paths ["i", "teams", toByteString' tid, "size"])

testTeamSizePublic :: Brig -> Http ()
testTeamSizePublic brig = do
  testTeamSize brig (\tid uid -> brig . paths ["teams", toByteString' tid, "size"] . zUser uid)

testTeamSize :: Brig -> (TeamId -> UserId -> Request -> Request) -> Http ()
testTeamSize brig req = do
  (tid, owner, _) <- createPopulatedBindingTeam brig 10
  SearchUtil.refreshIndex brig
  -- 10 Team Members and an admin
  let expectedSize = 11
  assertSize tid owner expectedSize

  -- Even suspended teams should report correct size
  suspendTeam brig tid !!! const 200 === statusCode
  SearchUtil.refreshIndex brig
  assertSize tid owner expectedSize
  where
    assertSize :: (HasCallStack) => TeamId -> UserId -> Natural -> Http ()
    assertSize tid uid expectedSize =
      void $
        get (req tid uid) <!! do
          const 200 === statusCode
          (const . Right $ TeamSize expectedSize) === responseJsonEither

-------------------------------------------------------------------------------
-- Invitation Tests

testUpdateEvents :: Brig -> Cannon -> Http ()
testUpdateEvents brig cannon = do
  (alice, tid) <- createUserWithTeam brig
  inviteeEmail <- randomEmail
  -- invite and register Bob
  let invite = stdInvitationRequest inviteeEmail
  inv <- responseJsonError =<< postInvitation brig tid alice invite
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  rsp2 <-
    post
      ( brig
          . path "/register"
          . contentJson
          . body (accept inviteeEmail inviteeCode)
      )
      <!! const 201 === statusCode
  let Just bob = userId <$> responseJsonMaybe rsp2
  -- ensure Alice and Bob are not connected
  void $ getConnection brig bob alice <!! const 404 === statusCode
  void $ getConnection brig alice bob <!! const 404 === statusCode
  -- Alice updates her profile
  let newColId = Just 5
      newAssets = Just [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)]
      newName = Just $ Name "Alice in Wonderland"
      newPic = Nothing -- Legacy
      userUpdate = UserUpdate newName Nothing newPic newAssets newColId
      update = RequestBodyLBS . encode $ userUpdate
  -- Update profile & receive notification
  WS.bracketRN cannon [alice, bob] $ \[aliceWS, bobWS] -> do
    put (brig . path "/self" . contentJson . zUser alice . zConn "c" . body update)
      !!! const 200 === statusCode
    liftIO $ mapConcurrently_ (\ws -> assertUpdateNotification ws alice userUpdate) [aliceWS, bobWS]

testInvitationEmail :: Brig -> Http ()
testInvitationEmail brig = do
  (inviter, tid) <- createUserWithTeam brig
  invite <- stdInvitationRequest <$> randomEmail
  res <-
    postInvitation brig tid inviter invite <!! do
      const 201 === statusCode
  inv <- responseJsonError res
  let actualHeader = getHeader "Location" res
  let expectedHeader = "/teams/" <> toByteString' tid <> "/invitations/" <> toByteString' (inInvitation inv)
  liftIO $ do
    Just inviter @=? inCreatedBy inv
    tid @=? inTeam inv
    assertInvitationResponseInvariants invite inv
    (isNothing . inInviteeUrl) inv @? "No invitation url expected"
    actualHeader @?= Just expectedHeader

assertInvitationResponseInvariants :: InvitationRequest -> Invitation -> Assertion
assertInvitationResponseInvariants invReq inv = do
  inviteeName invReq @=? inInviteeName inv
  inviteeEmail invReq @=? inInviteeEmail inv

testGetInvitation :: Brig -> Http ()
testGetInvitation brig = do
  (inviter, tid) <- createUserWithTeam brig
  invite <- stdInvitationRequest <$> randomEmail
  inv1 <- responseJsonError =<< postInvitation brig tid inviter invite <!! do const 201 === statusCode
  inv2 <- responseJsonError =<< getInvitation brig tid (inInvitation inv1) inviter <!! do const 200 === statusCode
  liftIO $ inv1 @=? inv2

testDeleteInvitation :: Brig -> Http ()
testDeleteInvitation brig = do
  (inviter, tid) <- createUserWithTeam brig
  invite <- stdInvitationRequest <$> randomEmail
  iid <- inInvitation <$> (responseJsonError =<< postInvitation brig tid inviter invite <!! do const 201 === statusCode)
  deleteInvitation brig tid iid inviter
  getInvitation brig tid iid inviter !!! do const 404 === statusCode

-- FUTUREWORK: This test should be rewritten to be free of mocks once Galley is
-- inlined into Brig.
testInvitationUrl :: Opt.Opts -> Brig -> Http ()
testInvitationUrl opts brig = do
  (inviter, tid) <- createUserWithTeam brig
  invite <- stdInvitationRequest <$> randomEmail

  void . withMockedGalley opts (invitationUrlGalleyMock FeatureStatusEnabled tid inviter) $ do
    resp <-
      postInvitation brig tid inviter invite
        <!! (const 201 === statusCode)

    inv :: Invitation <- responseJsonError resp
    invCode <- getInvitationCode brig tid (inInvitation inv)
    liftIO $ do
      assertInvitationResponseInvariants invite inv
      isJust invCode @? "Expect an invitation code in the backend"
      Just inviter @=? inCreatedBy inv
      tid @=? inTeam inv
      getQueryParam "team_code" resp @=? (invCode <&> (toStrict . toByteString))
      getQueryParam "team" resp @=? (pure . encodeUtf8 . idToText) tid

getQueryParam :: ByteString -> ResponseLBS -> Maybe ByteString
getQueryParam name r = do
  inv <- (eitherToMaybe . responseJsonEither) r
  url <- inInviteeUrl inv
  (lookup name . queryPairs . uriQuery) url

-- | Mock the feature API because exposeInvitationURLsToTeamAdmin depends on
-- static configuration that cannot be changed at runtime.
invitationUrlGalleyMock ::
  FeatureStatus ->
  TeamId ->
  UserId ->
  ReceivedRequest ->
  MockT IO Wai.Response
invitationUrlGalleyMock featureStatus tid inviter (ReceivedRequest mth pth body_)
  | mth == "GET"
      && pth == ["i", "teams", Text.pack (show tid), "features", "exposeInvitationURLsToTeamAdmin"] =
      pure . Wai.responseLBS HTTP.status200 mempty $
        encode
          ( LockableFeature
              featureStatus
              LockStatusUnlocked
              ExposeInvitationURLsToTeamAdminConfig
          )
  | mth == "GET"
      && pth == ["i", "teams", Text.pack (show tid), "members", Text.pack (show inviter)] =
      pure . Wai.responseLBS HTTP.status200 mempty $
        encode (mkTeamMember inviter fullPermissions Nothing UserLegalHoldDisabled)
  | mth == "GET"
      && pth == ["i", "feature-configs"] =
      pure $ Wai.responseLBS HTTP.status200 mempty (encode (def @AllTeamFeatures))
  | otherwise =
      let errBody =
            encode . object $
              [ "msg" .= ("unexpecUnexpected request to mocked galley" :: Text),
                "method" .= show mth,
                "path" .= pth,
                "body" .= (cs @_ @Text body_)
              ]
       in pure $ Wai.responseLBS HTTP.status500 mempty errBody

-- FUTUREWORK: This test should be rewritten to be free of mocks once Galley is
-- inlined into Brig.
testNoInvitationUrl :: Opt.Opts -> Brig -> Http ()
testNoInvitationUrl opts brig = do
  (inviter, tid) <- createUserWithTeam brig
  invite <- stdInvitationRequest <$> randomEmail

  void . withMockedGalley opts (invitationUrlGalleyMock FeatureStatusDisabled tid inviter) $ do
    resp <-
      postInvitation brig tid inviter invite
        <!! (const 201 === statusCode)

    inv :: Invitation <- responseJsonError resp
    invCode <- getInvitationCode brig tid (inInvitation inv)
    liftIO $ do
      assertInvitationResponseInvariants invite inv
      isJust invCode @? "Expect an invitation code in the backend"
      Just inviter @=? inCreatedBy inv
      tid @=? inTeam inv
      (isNothing . inInviteeUrl) inv @? "No invitation url expected"

testInvitationEmailLookup :: Brig -> Http ()
testInvitationEmailLookup brig = do
  email <- randomEmail
  -- expect no invitation to be found for an email before that person is invited
  headInvitationByEmail brig email 404
  (uid, tid) <- createUserWithTeam brig
  let invite = stdInvitationRequest email
  void $ postInvitation brig tid uid invite
  -- expect an invitation to be found querying with email after invite
  headInvitationByEmail brig email 200
  (uid2, tid2) <- createUserWithTeam brig
  let invite2 = stdInvitationRequest email
  void $ postInvitation brig tid2 uid2 invite2
  -- expect a 409 conflict result for a second team inviting the same user
  headInvitationByEmail brig email 409

testInvitationEmailLookupRegister :: Brig -> Http ()
testInvitationEmailLookupRegister brig = do
  email <- randomEmail
  (owner, tid) <- createUserWithTeam brig
  let invite = stdInvitationRequest email
  void $ postInvitation brig tid owner invite
  inv :: Invitation <- responseJsonError =<< postInvitation brig tid owner invite
  -- expect an invitation to be found querying with email after invite
  headInvitationByEmail brig email 200
  void $ registerInvite brig tid inv email
  -- expect a 404 after invitation has been used.
  headInvitationByEmail brig email 404

testInvitationEmailLookupNginz :: Brig -> Nginz -> Http ()
testInvitationEmailLookupNginz brig nginz = do
  email <- randomEmail
  -- expect no invitation to be found for an email before that person is invited
  headInvitationByEmail nginz email 404
  (uid, tid) <- createUserWithTeam brig
  let invite = stdInvitationRequest email
  void $ postInvitation brig tid uid invite
  -- expect an invitation to be found querying with email after invite
  headInvitationByEmail nginz email 200

headInvitationByEmail :: (Request -> Request) -> EmailAddress -> Int -> Http ()
headInvitationByEmail service email expectedCode =
  Bilge.head (service . path "/teams/invitations/by-email" . contentJson . queryItem "email" (toByteString' email))
    !!! const expectedCode === statusCode

testInvitationTooManyPending :: Opt.Opts -> Brig -> TeamSizeLimit -> Http ()
testInvitationTooManyPending opts brig (TeamSizeLimit limit) = do
  (inviter, tid) <- createUserWithTeam brig
  emails <- replicateConcurrently (fromIntegral limit) randomEmail
  email <- randomEmail
  -- If this test takes longer to run than `team-invitation-timeout`, then some of the
  -- invitations have likely expired already and this test will actually _fail_
  -- therefore we increase the timeout from default 10 to 300 seconds
  let longerTimeout = opts {Opt.optSettings = (Opt.optSettings opts) {Opt.setTeamInvitationTimeout = 300}}
  withSettingsOverrides longerTimeout $ do
    forM_ emails $ postInvitation brig tid inviter . stdInvitationRequest
  postInvitation brig tid inviter (stdInvitationRequest email) !!! do
    const 403 === statusCode
    const (Just "too-many-team-invitations") === fmap Error.label . responseJsonMaybe

registerInvite :: Brig -> TeamId -> Invitation -> EmailAddress -> Http UserId
registerInvite brig tid inv invemail = do
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  rsp <-
    post
      ( brig
          . path "/register"
          . contentJson
          . body (accept invemail inviteeCode)
      )
      <!! const 201 === statusCode
  let Just invitee = userId <$> responseJsonMaybe rsp
  pure invitee

-- | Admins can invite external partners, but not owners.
testInvitationRoles :: (HasCallStack) => Brig -> Galley -> Http ()
testInvitationRoles brig galley = do
  (owner, tid) <- createUserWithTeam brig
  -- owner creates a member alice.
  alice :: UserId <- do
    aliceEmail <- randomEmail
    let invite = stdInvitationRequest' Nothing (Just RoleAdmin) aliceEmail
    inv :: Invitation <- responseJsonError =<< postInvitation brig tid owner invite
    registerInvite brig tid inv aliceEmail
  -- alice creates a external partner bob.  success!  bob only has externalPartner perms.
  do
    bobEmail <- randomEmail
    let invite = stdInvitationRequest' Nothing (Just RoleExternalPartner) bobEmail
    inv :: Invitation <-
      responseJsonError
        =<< ( postInvitation brig tid alice invite <!! do
                const 201 === statusCode
            )
    uid <- registerInvite brig tid inv bobEmail
    let memreq =
          galley
            . zUser owner
            . zConn "c"
            . paths ["teams", toByteString' tid, "members", toByteString' uid]
    mem :: TeamMember <- responseJsonError =<< (get memreq <!! const 200 === statusCode)
    liftIO $ assertEqual "perms" (rolePermissions RoleExternalPartner) (mem ^. permissions)
  -- alice creates an owner charly.  failure!
  do
    charlyEmail <- randomEmail
    let invite = stdInvitationRequest' Nothing (Just RoleOwner) charlyEmail
    postInvitation brig tid alice invite !!! do
      const 403 === statusCode
      const (Just "insufficient-permissions") === fmap Error.label . responseJsonMaybe

testInvitationEmailAccepted :: Brig -> Galley -> Http ()
testInvitationEmailAccepted brig galley = do
  email <- randomEmail
  let invite = stdInvitationRequest email
  void $ createAndVerifyInvitation (accept invite.inviteeEmail) invite brig galley

-- | Related: 'testDomainsBlockedForRegistration'.  When we remove the customer-specific
-- extension of domain blocking, this test will fail to compile (so you will know it's time to
-- remove it).
testInvitationEmailAcceptedInBlockedDomain :: Opt.Opts -> Brig -> Galley -> Http ()
testInvitationEmailAcceptedInBlockedDomain opts brig galley = do
  email :: EmailAddress <- randomEmail
  let invite = stdInvitationRequest email
      replacementBrigApp = withDomainsBlockedForRegistration opts [decodeUtf8 $ domainPart email]
  void $ createAndVerifyInvitation' (Just replacementBrigApp) (accept invite.inviteeEmail) invite brig galley

-- | FUTUREWORK: this is an alternative helper to 'createPopulatedBindingTeam'.  it has been
-- added concurrently, and the two should probably be consolidated.
createAndVerifyInvitation ::
  (HasCallStack) =>
  (InvitationCode -> RequestBody) ->
  InvitationRequest ->
  Brig ->
  Galley ->
  Http (Maybe SelfProfile, Invitation)
createAndVerifyInvitation acceptFn invite brig galley = do
  createAndVerifyInvitation' Nothing acceptFn invite brig galley

-- | The optional first argument uses a brig fake 'Application' to the test instead of the
-- one on the network.
createAndVerifyInvitation' ::
  forall m a.
  ( HasCallStack,
    MonadIO m,
    MonadHttp m,
    MonadCatch m,
    MonadFail m,
    a ~ (Maybe (UserId, UTCTimeMillis), Invitation, UserId, ResponseLBS)
  ) =>
  Maybe (WaiTest.Session a -> m a) ->
  (InvitationCode -> RequestBody) ->
  InvitationRequest ->
  Brig ->
  Galley ->
  m (Maybe SelfProfile, Invitation)
createAndVerifyInvitation' replacementBrigApp acceptFn invite brig galley = do
  (inviter, tid) <- createUserWithTeam brig
  let invitationHandshake ::
        forall m'.
        ( HasCallStack,
          MonadIO m',
          MonadHttp m',
          MonadCatch m',
          MonadFail m'
        ) =>
        m' (Maybe (UserId, UTCTimeMillis), Invitation, UserId, ResponseLBS)
      invitationHandshake = do
        inv <- responseJsonError =<< postInvitation brig tid inviter invite
        let invmeta = Just (inviter, inCreatedAt inv)
        Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
        Just invitation <- getInvitationInfo brig inviteeCode
        rsp2 <-
          post
            ( brig
                . path "/register"
                . contentJson
                . body (acceptFn inviteeCode)
            )
            <!! const 201 === statusCode
        let Just (invitee, Just email2) = (userId &&& userEmail) <$> responseJsonMaybe rsp2
        let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
        liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)
        -- Verify that the invited user is active
        login brig (defEmailLogin email2) PersistentCookie !!! const 200 === statusCode
        pure (invmeta, invitation, invitee, rsp2)
  (invmeta, invitation, invitee, rsp2) <- case replacementBrigApp of
    Nothing -> invitationHandshake
    Just app -> app invitationHandshake
  -- Verify that the user is part of the team
  mem <- getTeamMember invitee tid galley
  liftIO $ assertEqual "Member not part of the team" invitee (mem ^. Member.userId)
  liftIO $ assertEqual "Member has no/wrong invitation metadata" invmeta (mem ^. Member.invitation)
  conns <-
    responseJsonError
      =<< listConnections brig invitee
        <!! const 200 === statusCode
  liftIO $ assertBool "User should have no connections" (null (clConnections conns) && not (clHasMore conns))
  pure (responseJsonMaybe rsp2, invitation)

testCreateTeam :: Brig -> Galley -> UserJournalWatcher -> Http ()
testCreateTeam brig galley userJournalWatcher = do
  email <- randomEmail
  usr <- responseJsonError =<< register email newTeam brig
  let uid = userId usr
  let tid = fromMaybe (error "No team??") $ userTeam usr
  team <- Team.tdTeam <$> getTeam galley tid
  mem <- getTeamMember uid (team ^. teamId) galley
  liftIO $ assertBool "Member not part of the team" (uid == mem ^. Member.userId)
  -- Verify that the user cannot send invitations before activating their account
  inviteeEmail <- randomEmail
  let invite = stdInvitationRequest inviteeEmail
  postInvitation brig (team ^. teamId) uid invite !!! const 403 === statusCode
  -- Verify that the team is still in status "pending"
  team2 <- getTeam galley (team ^. teamId)
  liftIO $ assertEqual "status" Team.PendingActive (Team.tdStatus team2)
  -- Activate account
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "activation key/code not found"
    Just kc -> activate brig kc !!! const 200 === statusCode
  Util.assertUserActivateJournaled userJournalWatcher usr "user activate"
  -- Verify that Team has status Active now
  team3 <- getTeam galley (team ^. teamId)
  liftIO $ assertEqual "status" Team.Active (Team.tdStatus team3)

testCreateTeamPreverified :: Brig -> Galley -> UserJournalWatcher -> Http ()
testCreateTeamPreverified brig galley userJournalWatcher = do
  email <- randomEmail
  requestActivationCode brig 200 (Left email)
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "activation key/code not found"
    Just (_, c) -> do
      usr <- responseJsonError =<< register' email newTeam c brig <!! const 201 === statusCode
      let uid = userId usr
      Util.assertUserActivateJournaled userJournalWatcher usr "user activate"
      let tid = fromMaybe (error "No team??") $ userTeam usr
      team <- Team.tdTeam <$> getTeam galley tid
      mem <- getTeamMember uid (team ^. teamId) galley
      liftIO $ assertBool "Member not part of the team" (uid == mem ^. Member.userId)
      team2 <- getTeam galley (team ^. teamId)
      liftIO $ assertEqual "Team should already be active" Team.Active (Team.tdStatus team2)
      -- Verify that the user can already send invitations before activating their account
      inviteeEmail <- randomEmail
      let invite = stdInvitationRequest inviteeEmail
      postInvitation brig (team ^. teamId) uid invite !!! const 201 === statusCode

testInvitationNoPermission :: Brig -> Http ()
testInvitationNoPermission brig = do
  (_, tid) <- createUserWithTeam brig
  alice <- userId <$> randomUser brig
  email <- randomEmail
  let invite = stdInvitationRequest email
  postInvitation brig tid alice invite !!! do
    const 403 === statusCode
    const (Just "insufficient-permissions") === fmap Error.label . responseJsonMaybe

testTeamNoPassword :: Brig -> Http ()
testTeamNoPassword brig = do
  e <- randomEmail
  -- Team creators must have a password
  post
    ( brig
        . path "/register"
        . contentJson
        . body
          ( RequestBodyLBS . encode $
              object
                [ "name" .= ("Bob" :: Text),
                  "email" .= fromEmail e,
                  "team" .= newTeam
                ]
          )
    )
    !!! const 400 === statusCode
  -- And so do any other binding team members
  code <- liftIO $ InvitationCode . Ascii.encodeBase64Url <$> randomBytes 24
  post
    ( brig
        . path "/register"
        . contentJson
        . body
          ( RequestBodyLBS . encode $
              object
                [ "name" .= ("Bob" :: Text),
                  "email" .= fromEmail e,
                  "team_code" .= code
                ]
          )
    )
    !!! const 400 === statusCode

testInvitationCodeExists :: Brig -> Http ()
testInvitationCodeExists brig = do
  (uid, tid) <- createUserWithTeam brig
  let invite email = stdInvitationRequest email
  email <- randomEmail
  rsp <- postInvitation brig tid uid (invite email) <!! const 201 === statusCode
  let Just invId = inInvitation <$> responseJsonMaybe rsp
  Just invCode <- getInvitationCode brig tid invId
  post (brig . path "/register" . contentJson . body (accept email invCode))
    !!! const 201 === statusCode
  post (brig . path "/register" . contentJson . body (accept email invCode)) !!! do
    const 409 === statusCode
    const (Just "key-exists") === fmap Error.label . responseJsonMaybe
  email2 <- randomEmail
  post (brig . path "/register" . contentJson . body (accept email2 invCode)) !!! do
    const 400 === statusCode
    const (Just "invalid-invitation-code") === fmap Error.label . responseJsonMaybe

testInvitationInvalidCode :: Brig -> Http ()
testInvitationInvalidCode brig = do
  email <- randomEmail
  -- Syntactically invalid
  let code1 = InvitationCode (Ascii.unsafeFromText "8z6JVcO1o4o¿9kFeb4Y3N-BmhIjH6b33")
  post (brig . path "/register" . contentJson . body (accept email code1)) !!! do
    const 400 === statusCode
    const (Just "bad-request") === fmap Error.label . responseJsonMaybe
  -- Syntactically valid but semantically invalid
  code2 <- liftIO $ InvitationCode . Ascii.encodeBase64Url <$> randomBytes 24
  post (brig . path "/register" . contentJson . body (accept email code2)) !!! do
    const 400 === statusCode
    const (Just "invalid-invitation-code") === fmap Error.label . responseJsonMaybe

testInvitationCodeNoIdentity :: Brig -> Http ()
testInvitationCodeNoIdentity brig = do
  uid <- liftIO $ Id <$> UUID.nextRandom
  post (brig . path "/register" . contentJson . body (payload uid)) !!! do
    const 403 === statusCode
    const (Just "missing-identity") === fmap Error.label . responseJsonMaybe
  where
    payload u =
      RequestBodyLBS . encode $
        object
          [ "name" .= ("Bob" :: Text),
            "team_code" .= u,
            "password" .= defPassword
          ]

testInvitationMutuallyExclusive :: Brig -> Http ()
testInvitationMutuallyExclusive brig = do
  email <- randomEmail
  code <- liftIO $ InvitationCode . Ascii.encodeBase64Url <$> randomBytes 24
  req email (Just code) (Just newTeam) Nothing !!! const 400 === statusCode
  req email (Just code) Nothing (Just code) !!! const 400 === statusCode
  req email Nothing (Just newTeam) (Just code) !!! const 400 === statusCode
  req email (Just code) (Just newTeam) (Just code) !!! const 400 === statusCode
  where
    req ::
      EmailAddress ->
      Maybe InvitationCode ->
      Maybe BindingNewTeam ->
      Maybe InvitationCode ->
      HttpT IO (Response (Maybe LByteString))
    req e c t i =
      post
        ( brig
            . path "/register"
            . contentJson
            . body
              ( RequestBodyLBS . encode $
                  object
                    [ "name" .= ("Bob" :: Text),
                      "email" .= fromEmail e,
                      "password" .= defPassword,
                      "team_code" .= c,
                      "team" .= t,
                      "invitation_code" .= i
                    ]
              )
        )

testInvitationTooManyMembers :: Brig -> Galley -> TeamSizeLimit -> Http ()
testInvitationTooManyMembers brig galley (TeamSizeLimit limit) = do
  (creator, tid) <- createUserWithTeam brig
  pooledForConcurrentlyN_ 16 [1 .. limit - 1] $ \_ -> do
    void $ createTeamMember brig galley creator tid fullPermissions
  SearchUtil.refreshIndex brig
  let invite email = stdInvitationRequest email
  email <- randomEmail
  inv <- responseJsonError =<< postInvitation brig tid creator (invite email)
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  post
    ( brig
        . path "/register"
        . contentJson
        . body (accept email inviteeCode)
    )
    !!! do
      const 403 === statusCode
      const (Just "too-many-team-members") === fmap Error.label . responseJsonMaybe

testInvitationPaging :: (HasCallStack) => Opt.Opts -> Brig -> Http ()
testInvitationPaging opts brig = do
  before <- liftIO $ toUTCTimeMillis . addUTCTime (-1) <$> getCurrentTime
  (uid, tid) <- createUserWithTeam brig
  let total = 5
      invite email = stdInvitationRequest email
      longerTimeout = opts {Opt.optSettings = (Opt.optSettings opts) {Opt.setTeamInvitationTimeout = 300}}
  emails <-
    withSettingsOverrides longerTimeout $
      replicateM total $ do
        email <- randomEmail
        postInvitation brig tid uid (invite email) !!! const 201 === statusCode
        pure email
  after1ms <- liftIO $ toUTCTimeMillis . addUTCTime 1 <$> getCurrentTime
  let getPages :: (HasCallStack) => Int -> Maybe InvitationId -> Int -> Http [[Invitation]]
      getPages count start step = do
        let range = queryRange (toByteString' <$> start) (Just step)
        r <-
          get (brig . paths ["teams", toByteString' tid, "invitations"] . zUser uid . range)
            <!! const 200
              === statusCode
        (invs, more) <- (ilInvitations &&& ilHasMore) <$> responseJsonError r
        if more
          then (invs :) <$> getPages (count + step) (fmap inInvitation . listToMaybe . reverse $ invs) step
          else pure [invs]
  let checkSize :: (HasCallStack) => Int -> [Int] -> Http ()
      checkSize pageSize expectedSizes =
        getPages 0 Nothing pageSize >>= \invss -> liftIO $ do
          assertEqual "page sizes" expectedSizes (take (length expectedSizes) (map length invss))
          mapM_ validateInv $ concat invss
      validateInv :: Invitation -> Assertion
      validateInv inv = do
        assertEqual "tid" tid (inTeam inv)
        assertBool "email" (inInviteeEmail inv `elem` emails)
        -- (the output list is not ordered chronologically and emails are unique, so we just
        -- check whether the email is one of the valid ones.)
        assertBool "timestamp" (inCreatedAt inv > before && inCreatedAt inv < after1ms)
        assertEqual "uid" (Just uid) (inCreatedBy inv)
  -- not checked: @inInvitation inv :: InvitationId@

  checkSize 2 [2, 2, 1]
  checkSize total [total]
  checkSize (total + 1) [total]

testInvitationInfo :: Brig -> Http ()
testInvitationInfo brig = do
  email <- randomEmail
  (uid, tid) <- createUserWithTeam brig
  let invite = stdInvitationRequest email
  inv <- responseJsonError =<< postInvitation brig tid uid invite
  Just invCode <- getInvitationCode brig tid (inInvitation inv)
  Just invitation <- getInvitationInfo brig invCode
  liftIO $ assertEqual "Invitations differ" inv invitation

testInvitationInfoBadCode :: Brig -> Http ()
testInvitationInfoBadCode brig = do
  -- The code contains non-ASCII characters after url-decoding
  let icode = "8z6JVcO1o4o%C2%BF9kFeb4Y3N-BmhIjH6b33"
  get (brig . path ("/teams/invitations/info?code=" <> icode))
    !!! const 400 === statusCode

testInvitationInfoExpired :: Brig -> Opt.Timeout -> Http ()
testInvitationInfoExpired brig timeout = do
  email <- randomEmail
  (uid, tid) <- createUserWithTeam brig
  let invite = stdInvitationRequest email
  inv <- responseJsonError =<< postInvitation brig tid uid invite
  -- Note: This value must be larger than the option passed as `team-invitation-timeout`
  awaitExpiry (round timeout + 5) tid (inInvitation inv)
  getCode tid (inInvitation inv) !!! const 400 === statusCode
  headInvitationByEmail brig email 404
  where
    getCode t i =
      get
        ( brig
            . path "/i/teams/invitation-code"
            . queryItem "team" (toByteString' t)
            . queryItem "invitation_id" (toByteString' i)
        )
    awaitExpiry :: Int -> TeamId -> InvitationId -> Http ()
    awaitExpiry n t i = do
      liftIO $ threadDelay 1000000
      r <- getCode t i
      when (statusCode r == 200 && n > 0) $
        awaitExpiry (n - 1) t i

testSuspendTeam :: Brig -> Http ()
testSuspendTeam brig = do
  inviteeEmail <- randomEmail
  inviteeEmail2 <- randomEmail
  (inviter, tid) <- createUserWithTeam brig
  -- invite and register invitee
  let invite = stdInvitationRequest inviteeEmail
  inv <- responseJsonError =<< postInvitation brig tid inviter invite
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  rsp2 <-
    post
      ( brig
          . path "/register"
          . contentJson
          . body (accept inviteeEmail inviteeCode)
      )
      <!! const 201 === statusCode
  let Just (invitee, Just email) = (userId &&& userEmail) <$> responseJsonMaybe rsp2
  -- invite invitee2 (don't register)
  let invite2 = stdInvitationRequest inviteeEmail2

  inv2 <- responseJsonError =<< postInvitation brig tid inviter invite2
  Just _ <- getInvitationCode brig tid (inInvitation inv2)
  -- suspend team
  suspendTeam brig tid !!! const 200 === statusCode
  -- login fails
  login brig (defEmailLogin email) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "suspended") === fmap Error.label . responseJsonMaybe
  -- check status
  chkStatus brig inviter Suspended
  chkStatus brig invitee Suspended
  assertNoInvitationCode brig tid (inInvitation inv2)
  -- unsuspend
  unsuspendTeam brig tid !!! const 200 === statusCode
  chkStatus brig inviter Active
  chkStatus brig invitee Active
  login brig (defEmailLogin email) PersistentCookie !!! const 200 === statusCode

testDeleteTeamUser :: Brig -> Galley -> Http ()
testDeleteTeamUser brig galley = do
  (creator, tid) <- createUserWithTeam brig
  -- Cannot delete the user since it will make the team orphan
  deleteUser creator (Just defPassword) brig !!! do
    const 403 === statusCode
    const (Just "no-self-delete-for-team-owner") === fmap Error.label . responseJsonMaybe
  -- We need to invite another user to a full permission member
  invitee <- userId <$> inviteAndRegisterUser creator tid brig
  updatePermissions creator tid (invitee, fullPermissions) galley
  -- Still cannot delete, must be demoted first
  deleteUser creator (Just defPassword) brig !!! do
    const 403 === statusCode
    const (Just "no-self-delete-for-team-owner") === fmap Error.label . responseJsonMaybe
  -- Demote creator
  updatePermissions invitee tid (creator, rolePermissions RoleAdmin) galley
  -- Now the creator can delete the account
  deleteUser creator (Just defPassword) brig !!! const 200 === statusCode
  -- The new full permission member cannot
  deleteUser invitee (Just defPassword) brig !!! const 403 === statusCode
  -- We can still invite new users who can delete their account only if they are not an owner
  inviteeFull <- userId <$> inviteAndRegisterUser invitee tid brig
  updatePermissions invitee tid (inviteeFull, fullPermissions) galley
  deleteUser inviteeFull (Just defPassword) brig !!! do
    const 403 === statusCode
    const (Just "no-self-delete-for-team-owner") === fmap Error.label . responseJsonMaybe
  inviteeMember <- userId <$> inviteAndRegisterUser invitee tid brig
  deleteUser inviteeMember (Just defPassword) brig !!! const 200 === statusCode
  -- still cannot delete invitee
  deleteUser invitee (Just defPassword) brig !!! do
    const 403 === statusCode
    const (Just "no-self-delete-for-team-owner") === fmap Error.label . responseJsonMaybe
  -- Ensure internal endpoints are also exercised
  deleteUserInternal invitee brig !!! const 202 === statusCode
  -- Eventually the user will be deleted, leaving the team orphan
  void $ retryWhileN 20 (/= Deleted) (getStatus brig invitee)
  chkStatus brig invitee Deleted

testSSOIsTeamOwner :: Brig -> Galley -> Http ()
testSSOIsTeamOwner brig galley = do
  (creator, tid) <- createUserWithTeam brig
  stranger <- userId <$> randomUser brig
  invitee <- userId <$> inviteAndRegisterUser creator tid brig
  let check expectWhat uid = void $ get (galley . paths opath . expectWhat)
        where
          opath = ["i", "teams", toByteString' tid, "is-team-owner", toByteString' uid]
  check expect2xx creator
  check expect4xx stranger
  check expect4xx invitee
  updatePermissions creator tid (invitee, fullPermissions) galley
  check expect2xx invitee

testConnectionSameTeam :: Brig -> Http ()
testConnectionSameTeam brig = do
  (creatorA, tidA) <- createUserWithTeam brig
  inviteeA <- userId <$> inviteAndRegisterUser creatorA tidA brig
  postConnection brig creatorA inviteeA !!! do
    const 403 === statusCode
    const (Just "same-binding-team-users") === fmap Error.label . responseJsonMaybe
  creatorB <- userId <$> randomUser brig
  -- Can connect across teams
  postConnection brig creatorA creatorB !!! const 201 === statusCode
  external <- userId <$> randomUser brig
  -- Externals are also ok
  postConnection brig creatorA external !!! const 201 === statusCode
  postConnection brig creatorB external !!! const 201 === statusCode

----------------------------------------------------------------------
-- SSO

testCreateUserInternalSSO :: Brig -> Galley -> Http ()
testCreateUserInternalSSO brig galley = do
  teamid <- snd <$> createUserWithTeam brig
  let ssoid = UserSSOId mkSimpleSampleUref
  -- creating users requires both sso_id and team_id
  postUser' True False "dummy" True False (Just ssoid) Nothing brig
    !!! const 400 === statusCode
  postUser' True False "dummy" True False Nothing (Just teamid) brig
    !!! const 400 === statusCode
  -- creating user with sso_id, team_id is ok
  resp <-
    postUser "dummy" True False (Just ssoid) (Just teamid) brig <!! do
      const 201 === statusCode
      const (Just ssoid) === (userSSOId . selfUser <=< responseJsonMaybe)
  -- self profile contains sso id
  let Just uid = userId <$> responseJsonMaybe resp
  profile <- getSelfProfile brig uid
  liftIO $
    assertEqual
      "self profile user identity mismatch"
      (Just ssoid)
      (userSSOId $ selfUser profile)
  -- sso-managed users must have team id.
  let Just teamid' = userTeam $ selfUser profile
  liftIO $ assertEqual "bad team_id" teamid teamid'
  -- does galley know about this?  is user active?
  _ <- getTeamMember uid teamid galley
  isact <- isActivatedUser uid brig
  liftIO $ assertBool "user not activated" isact

-- | See also: 'testDeleteTeamUser'.
testDeleteUserSSO :: Brig -> Galley -> Http ()
testDeleteUserSSO brig galley = do
  (creator, tid) <- createUserWithTeam brig
  let ssoid = UserSSOId mkSimpleSampleUref
      mkuser :: Bool -> Http (Maybe User)
      mkuser withemail =
        responseJsonMaybe
          <$> ( postUser "dummy" withemail False (Just ssoid) (Just tid) brig
                  <!! const 201 === statusCode
              )
  -- create and delete sso user (with email)
  Just (userId -> user1) <- mkuser True
  deleteUser user1 (Just defPassword) brig !!! const 200 === statusCode
  -- create sso user with email
  Just (userId -> creator') <- mkuser True
  updatePermissions creator tid (creator', fullPermissions) galley
  -- demote and delete creator, but cannot do it for second owner yet (as someone needs to demote them)
  updatePermissions creator' tid (creator, rolePermissions RoleMember) galley
  deleteUser creator (Just defPassword) brig !!! const 200 === statusCode
  -- create sso user without email, make an owner
  Just (userId -> user3) <- mkuser False
  updatePermissions creator' tid (user3, fullPermissions) galley
  -- can't delete herself, even without email
  deleteUser user3 (Just defPassword) brig !!! const 403 === statusCode
  -- delete second owner now, we don't enforce existence of emails in the backend
  updatePermissions user3 tid (creator', rolePermissions RoleMember) galley
  deleteUser creator' (Just defPassword) brig !!! const 200 === statusCode

test2FaDisabledForSsoUser :: Brig -> Galley -> Http ()
test2FaDisabledForSsoUser brig galley = do
  teamid <- snd <$> createUserWithTeam brig
  setTeamFeatureLockStatus @Public.SndFactorPasswordChallengeConfig galley teamid Public.LockStatusUnlocked
  setTeamSndFactorPasswordChallenge galley teamid Public.FeatureStatusEnabled
  let ssoid = UserSSOId mkSimpleSampleUref
  createUserResp <-
    postUser "dummy" True False (Just ssoid) (Just teamid) brig <!! do
      const 201 === statusCode
      const (Just ssoid) === (userSSOId . selfUser <=< responseJsonMaybe)
  let Just uid = userId <$> responseJsonMaybe createUserResp
  let verificationCode = Nothing
  addClient brig uid (defNewClientWithVerificationCode verificationCode PermanentClientType [head somePrekeys] (head someLastPrekeys))
    !!! const 201 === statusCode

-- TODO:
-- add sso service.  (we'll need a name for that now.)
-- brig needs to notify the sso service about deletions!
-- if the mock sso service disagrees with the deletion: 403 "sso-not-allowed" or something
-- if user is last remaining owner: 403 "no-other-owner" (as above).
-- otherwise: 2xx.
