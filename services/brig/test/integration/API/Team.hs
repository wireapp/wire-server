module API.Team (tests) where

import API.Search.Util
import API.Team.Util
import Bilge hiding (accept, head, timeout)
import Bilge.Assert
import qualified Brig.AWS as AWS
import qualified Brig.Options as Opt
import Brig.Types
import Brig.Types.Intra
import Brig.Types.Team.Invitation
import Brig.Types.User.Auth
import Control.Arrow ((&&&))
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.ByteString.Conversion
import Data.Handle (fromHandle)
import Data.Id hiding (client)
import Data.Json.Util (toUTCTimeMillis)
import qualified Data.Text.Ascii as Ascii
import Data.Time (addUTCTime, getCurrentTime)
import qualified Data.UUID.V4 as UUID
import qualified Galley.Types.Teams as Team
import qualified Galley.Types.Teams.Intra as Team
import Imports
import Network.HTTP.Client (Manager)
import qualified Network.Wai.Utilities.Error as Error
import Test.Tasty hiding (Timeout)
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import UnliftIO.Async (mapConcurrently_, pooledForConcurrentlyN_, replicateConcurrently)
import Util
import Util.AWS as Util
import Web.Cookie (parseSetCookie, setCookieName)

newtype TeamSizeLimit = TeamSizeLimit Word16

tests :: Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> AWS.Env -> IO TestTree
tests conf m b c g aws = do
  let tl = TeamSizeLimit . Opt.setMaxTeamSize . Opt.optSettings $ conf
  let it = Opt.setTeamInvitationTimeout . Opt.optSettings $ conf
  return $
    testGroup
      "team"
      [ testGroup
          "invitation"
          [ test m "post /teams/:tid/invitations - 201" $ testInvitationEmail b g,
            test m "post /teams/:tid/invitations - 403 no permission" $ testInvitationNoPermission b g,
            test m "post /teams/:tid/invitations - 403 too many pending" $ testInvitationTooManyPending b g tl,
            test m "post /teams/:tid/invitations - roles" $ testInvitationRoles b g,
            test' aws m "post /register - 201 accepted" $ testInvitationEmailAccepted b g,
            test' aws m "post /register user & team - 201 accepted" $ testCreateTeam b g aws,
            test' aws m "post /register user & team - 201 preverified" $ testCreateTeamPreverified b g aws,
            test m "post /register - 400 no passwordless" $ testTeamNoPassword b,
            test m "post /register - 400 code already used" $ testInvitationCodeExists b g,
            test m "post /register - 400 bad code" $ testInvitationInvalidCode b,
            test m "post /register - 400 no wireless" $ testInvitationCodeNoIdentity b,
            test m "post /register - 400 mutually exclusive" $ testInvitationMutuallyExclusive b,
            test m "post /register - 403 too many members" $ testInvitationTooManyMembers b g tl,
            test m "get /teams/:tid/invitations - 200 (paging)" $ testInvitationPaging b g,
            test m "get /teams/:tid/invitations/info - 200" $ testInvitationInfo b g,
            test m "get /teams/:tid/invitations/info - 400" $ testInvitationInfoBadCode b,
            test m "get /teams/:tid/invitations/info - 400 expired" $ testInvitationInfoExpired b g it,
            test m "post /i/teams/:tid/suspend - 200" $ testSuspendTeam b g,
            test m "put /self - 200 update events" $ testUpdateEvents b g c,
            test m "delete /self - 200 (ensure no orphan teams)" $ testDeleteTeamUser b g,
            test m "post /connections - 403 (same binding team)" $ testConnectionSameTeam b g
          ],
        testGroup
          "search"
          [ test m "post /register members are unsearchable" $ testNonSearchableDefault b g,
            test m "team members may only search within team if set" $ testNonSearchableAcrossTeam conf b g
          ],
        testGroup
          "sso"
          [ test m "post /i/users  - 201 internal-SSO" $ testCreateUserInternalSSO b g,
            test m "delete /i/users/:uid - 202 internal-SSO (ensure no orphan teams)" $ testDeleteUserSSO b g,
            test m "get /i/users/:uid/is-team-owner/:tid" $ testSSOIsTeamOwner b g
          ]
      ]

-------------------------------------------------------------------------------
-- Invitation Tests

testUpdateEvents :: Brig -> Galley -> Cannon -> Http ()
testUpdateEvents brig galley cannon = do
  (alice, tid) <- createUserWithTeam brig galley
  inviteeEmail <- randomEmail
  -- invite and register Bob
  let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing Nothing
  inv <- responseJsonError =<< postInvitation brig tid alice invite
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  rsp2 <-
    post
      ( brig . path "/register"
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
      newAssets = Just [ImageAsset "abc" (Just AssetComplete)]
      newName = Just $ Name "Alice in Wonderland"
      newPic = Nothing -- Legacy
      userUpdate = UserUpdate newName newPic newAssets newColId
      update = RequestBodyLBS . encode $ userUpdate
  -- Update profile & receive notification
  WS.bracketRN cannon [alice, bob] $ \[aliceWS, bobWS] -> do
    put (brig . path "/self" . contentJson . zUser alice . zConn "c" . body update)
      !!! const 200 === statusCode
    liftIO $ mapConcurrently_ (\ws -> assertUpdateNotification ws alice userUpdate) [aliceWS, bobWS]

testInvitationEmail :: Brig -> Galley -> Http ()
testInvitationEmail brig galley = do
  (inviter, tid) <- createUserWithTeam brig galley
  invitee <- randomEmail
  let invite = InvitationRequest invitee (Name "Bob") Nothing Nothing
  void $ postInvitation brig tid inviter invite

testInvitationTooManyPending :: Brig -> Galley -> TeamSizeLimit -> Http ()
testInvitationTooManyPending brig galley (TeamSizeLimit limit) = do
  (inviter, tid) <- createUserWithTeam brig galley
  emails <- replicateConcurrently (fromIntegral limit) randomEmail
  let invite e = InvitationRequest e (Name "Bob") Nothing Nothing
  pooledForConcurrentlyN_ 16 emails $ \email ->
    postInvitation brig tid inviter (invite email)
  e <- randomEmail
  -- TODO: If this test takes longer to run than `team-invitation-timeout`, then some of the
  --       invitations have likely expired already and this test will actually _fail_
  postInvitation brig tid inviter (invite e) !!! do
    const 403 === statusCode
    const (Just "too-many-team-invitations") === fmap Error.label . responseJsonMaybe

-- | Admins can invite external partners, but not owners.
testInvitationRoles :: HasCallStack => Brig -> Galley -> Http ()
testInvitationRoles brig galley = do
  (owner, tid) <- createUserWithTeam brig galley
  let registerInvite :: Invitation -> Email -> Http UserId
      registerInvite inv invemail = do
        Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
        rsp <-
          post
            ( brig . path "/register"
                . contentJson
                . body (accept invemail inviteeCode)
            )
            <!! const 201 === statusCode
        let Just invitee = userId <$> responseJsonMaybe rsp
        pure invitee
  -- owner creates a member alice.
  alice :: UserId <- do
    aliceEmail <- randomEmail
    let invite = InvitationRequest aliceEmail (Name "Alice") Nothing (Just Team.RoleAdmin)
    inv :: Invitation <- responseJsonError =<< postInvitation brig tid owner invite
    registerInvite inv aliceEmail
  -- alice creates a external partner bob.  success!  bob only has externalPartner perms.
  do
    bobEmail <- randomEmail
    let invite = InvitationRequest bobEmail (Name "Bob") Nothing (Just Team.RoleExternalPartner)
    inv :: Invitation <-
      responseJsonError
        =<< ( postInvitation brig tid alice invite <!! do
                const 201 === statusCode
            )
    uid <- registerInvite inv bobEmail
    let memreq =
          galley . zUser owner . zConn "c"
            . paths ["teams", toByteString' tid, "members", toByteString' uid]
    mem :: Team.TeamMember <- responseJsonError =<< (get memreq <!! const 200 === statusCode)
    liftIO $ assertEqual "perms" (Team.rolePermissions Team.RoleExternalPartner) (mem ^. Team.permissions)
  -- alice creates an owner charly.  failure!
  do
    charlyEmail <- randomEmail
    let invite = InvitationRequest charlyEmail (Name "Charly") Nothing (Just Team.RoleOwner)
    postInvitation brig tid alice invite !!! do
      const 403 === statusCode
      const (Just "insufficient-permissions") === fmap Error.label . responseJsonMaybe

testInvitationEmailAccepted :: Brig -> Galley -> Http ()
testInvitationEmailAccepted brig galley = do
  (inviter, tid) <- createUserWithTeam brig galley
  inviteeEmail <- randomEmail
  let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing Nothing
  inv <- responseJsonError =<< postInvitation brig tid inviter invite
  let invmeta = Just (inviter, inCreatedAt inv)
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  rsp2 <-
    post
      ( brig . path "/register"
          . contentJson
          . body (accept inviteeEmail inviteeCode)
      )
      <!! const 201 === statusCode
  let Just (invitee, Just email2) = (userId &&& userEmail) <$> responseJsonMaybe rsp2
  let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
  liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)
  -- Verify that the invited user is active
  login brig (defEmailLogin email2) PersistentCookie !!! const 200 === statusCode
  -- Verify that the user is part of the team
  mem <- getTeamMember invitee tid galley
  liftIO $ assertEqual "Member not part of the team" invitee (mem ^. Team.userId)
  liftIO $ assertEqual "Member has no/wrong invitation metadata" invmeta (mem ^. Team.invitation)
  conns <- listConnections invitee brig
  liftIO $ assertBool "User should have no connections" (null (clConnections conns) && not (clHasMore conns))

testCreateTeam :: Brig -> Galley -> AWS.Env -> Http ()
testCreateTeam brig galley aws = do
  email <- randomEmail
  usr <- responseJsonError =<< register email newTeam brig
  let uid = userId usr
  -- Verify that the user is part of exactly one (binding) team
  teams <- view Team.teamListTeams <$> getTeams uid galley
  liftIO $ assertBool "User not part of exactly one team" (length teams == 1)
  let team = fromMaybe (error "No team??") $ listToMaybe teams
  liftIO $ assertBool "Team not binding" (team ^. Team.teamBinding == Team.Binding)
  mem <- getTeamMember uid (team ^. Team.teamId) galley
  liftIO $ assertBool "Member not part of the team" (uid == mem ^. Team.userId)
  -- Verify that the user cannot send invitations before activating their account
  inviteeEmail <- randomEmail
  let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing Nothing
  postInvitation brig (team ^. Team.teamId) uid invite !!! const 403 === statusCode
  -- Verify that the team is still in status "pending"
  team2 <- getTeam galley (team ^. Team.teamId)
  liftIO $ assertEqual "status" Team.PendingActive (Team.tdStatus team2)
  -- Activate account
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "activation key/code not found"
    Just kc -> activate brig kc !!! const 200 === statusCode
  liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled usr)
  -- Verify that Team has status Active now
  team3 <- getTeam galley (team ^. Team.teamId)
  liftIO $ assertEqual "status" Team.Active (Team.tdStatus team3)

testCreateTeamPreverified :: Brig -> Galley -> AWS.Env -> Http ()
testCreateTeamPreverified brig galley aws = do
  email <- randomEmail
  requestActivationCode brig 200 (Left email)
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "activation key/code not found"
    Just (_, c) -> do
      usr <- responseJsonError =<< register' email newTeam c brig <!! const 201 === statusCode
      let uid = userId usr
      liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled usr)
      teams <- view Team.teamListTeams <$> getTeams uid galley
      liftIO $ assertBool "User not part of exactly one team" (length teams == 1)
      let team = fromMaybe (error "No team??") $ listToMaybe teams
      liftIO $ assertBool "Team not binding" (team ^. Team.teamBinding == Team.Binding)
      mem <- getTeamMember uid (team ^. Team.teamId) galley
      liftIO $ assertBool "Member not part of the team" (uid == mem ^. Team.userId)
      team2 <- getTeam galley (team ^. Team.teamId)
      liftIO $ assertEqual "Team should already be active" Team.Active (Team.tdStatus team2)
      -- Verify that the user can already send invitations before activating their account
      inviteeEmail <- randomEmail
      let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing Nothing
      postInvitation brig (team ^. Team.teamId) uid invite !!! const 201 === statusCode

testInvitationNoPermission :: Brig -> Galley -> Http ()
testInvitationNoPermission brig galley = do
  (_, tid) <- createUserWithTeam brig galley
  alice <- userId <$> randomUser brig
  email <- randomEmail
  let invite = InvitationRequest email (Name "Bob") Nothing Nothing
  postInvitation brig tid alice invite !!! do
    const 403 === statusCode
    const (Just "insufficient-permissions") === fmap Error.label . responseJsonMaybe

testTeamNoPassword :: Brig -> Http ()
testTeamNoPassword brig = do
  e <- randomEmail
  -- Team creators must have a password
  post
    ( brig . path "/register" . contentJson
        . body
          ( RequestBodyLBS . encode $
              object
                [ "name" .= ("Bob" :: Text),
                  "email" .= fromEmail e,
                  "team" .= newTeam
                ]
          )
    )
    !!! const 400
    === statusCode
  -- And so do any other binding team members
  code <- liftIO $ InvitationCode . Ascii.encodeBase64Url <$> randomBytes 24
  post
    ( brig . path "/register" . contentJson
        . body
          ( RequestBodyLBS . encode $
              object
                [ "name" .= ("Bob" :: Text),
                  "email" .= fromEmail e,
                  "team_code" .= code
                ]
          )
    )
    !!! const 400
    === statusCode

testInvitationCodeExists :: Brig -> Galley -> Http ()
testInvitationCodeExists brig galley = do
  email <- randomEmail
  (uid, tid) <- createUserWithTeam brig galley
  let invite email_ = InvitationRequest email_ (Name "Bob") Nothing Nothing
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
      Email ->
      Maybe InvitationCode ->
      Maybe Team.BindingNewTeam ->
      Maybe InvitationCode ->
      HttpT IO (Response (Maybe LByteString))
    req e c t i =
      post
        ( brig . path "/register" . contentJson
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
  (creator, tid) <- createUserWithTeam brig galley
  pooledForConcurrentlyN_ 16 [1 .. limit -1] $ \_ ->
    createTeamMember brig galley creator tid Team.fullPermissions
  em <- randomEmail
  let invite = InvitationRequest em (Name "Bob") Nothing Nothing
  inv <- responseJsonError =<< postInvitation brig tid creator invite
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  post
    ( brig . path "/register"
        . contentJson
        . body (accept em inviteeCode)
    )
    !!! do
      const 403 === statusCode
      const (Just "too-many-team-members") === fmap Error.label . responseJsonMaybe

testInvitationPaging :: HasCallStack => Brig -> Galley -> Http ()
testInvitationPaging brig galley = do
  before <- liftIO $ toUTCTimeMillis . addUTCTime (-1) <$> getCurrentTime
  (uid, tid) <- createUserWithTeam brig galley
  let total = 5
      invite email = InvitationRequest email (Name "Bob") Nothing Nothing
  emails <- replicateM total $ do
    email <- randomEmail
    postInvitation brig tid uid (invite email) !!! const 201 === statusCode
    pure email
  after1ms <- liftIO $ toUTCTimeMillis . addUTCTime 1 <$> getCurrentTime
  let next :: HasCallStack => Int -> (Int, Maybe InvitationId) -> Int -> Http (Int, Maybe InvitationId)
      next step (count, start) actualPageLen = do
        let count' = count + step
        let range = queryRange (toByteString' <$> start) (Just step)
        r <-
          get (brig . paths ["teams", toByteString' tid, "invitations"] . zUser uid . range)
            <!! const 200 === statusCode
        let (Just (invs, more)) = (ilInvitations &&& ilHasMore) <$> responseJsonMaybe r
        liftIO $ assertEqual "page size" actualPageLen (length invs)
        liftIO $ assertEqual "has more" (count' < total) more
        liftIO $ validateInv `mapM_` invs
        return (count', fmap inInvitation . listToMaybe . reverse $ invs)
      validateInv :: Invitation -> Assertion
      validateInv inv = do
        assertEqual "tid" tid (inTeam inv)
        assertBool "email" (inIdentity inv `elem` emails)
        -- (the output list is not ordered chronologically and emails are unique, so we just
        -- check whether the email is one of the valid ones.)
        assertBool "timestamp" (inCreatedAt inv > before && inCreatedAt inv < after1ms)
        assertEqual "uid" (Just uid) (inCreatedBy inv)
  -- not checked: @inInvitation inv :: InvitationId@

  foldM_ (next 2) (0, Nothing) [2, 2, 1, 0]
  foldM_ (next total) (0, Nothing) [total, 0]
  foldM_ (next (total + 1)) (0, Nothing) [total, 0]

testInvitationInfo :: Brig -> Galley -> Http ()
testInvitationInfo brig galley = do
  email <- randomEmail
  (uid, tid) <- createUserWithTeam brig galley
  let invite = InvitationRequest email (Name "Bob") Nothing Nothing
  inv <- responseJsonError =<< postInvitation brig tid uid invite
  Just invCode <- getInvitationCode brig tid (inInvitation inv)
  Just invitation <- getInvitation brig invCode
  liftIO $ assertEqual "Invitations differ" inv invitation

testInvitationInfoBadCode :: Brig -> Http ()
testInvitationInfoBadCode brig = do
  -- The code contains non-ASCII characters after url-decoding
  let icode = "8z6JVcO1o4o%C2%BF9kFeb4Y3N-BmhIjH6b33"
  get (brig . path ("/teams/invitations/info?code=" <> icode))
    !!! const 400 === statusCode

testInvitationInfoExpired :: Brig -> Galley -> Opt.Timeout -> Http ()
testInvitationInfoExpired brig galley timeout = do
  email <- randomEmail
  (uid, tid) <- createUserWithTeam brig galley
  let invite = InvitationRequest email (Name "Bob") Nothing Nothing
  inv <- responseJsonError =<< postInvitation brig tid uid invite
  -- Note: This value must be larger than the option passed as `team-invitation-timeout`
  awaitExpiry (round timeout + 5) tid (inInvitation inv)
  getCode tid (inInvitation inv) !!! const 400 === statusCode
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
        awaitExpiry (n -1) t i

testSuspendTeam :: Brig -> Galley -> Http ()
testSuspendTeam brig galley = do
  inviteeEmail <- randomEmail
  inviteeEmail2 <- randomEmail
  (inviter, tid) <- createUserWithTeam brig galley
  -- invite and register invitee
  let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing Nothing
  inv <- responseJsonError =<< postInvitation brig tid inviter invite
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  rsp2 <-
    post
      ( brig . path "/register"
          . contentJson
          . body (accept inviteeEmail inviteeCode)
      )
      <!! const 201 === statusCode
  let Just (invitee, Just email) = (userId &&& userEmail) <$> responseJsonMaybe rsp2
  -- invite invitee2 (don't register)
  let invite2 = InvitationRequest inviteeEmail2 (Name "Bob") Nothing Nothing
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
  (creator, tid) <- createUserWithTeam brig galley
  -- Cannot delete the user since it will make the team orphan
  deleteUser creator (Just defPassword) brig !!! do
    const 403 === statusCode
    const (Just "no-other-owner") === fmap Error.label . responseJsonMaybe
  -- We need to invite another user to a full permission member
  invitee <- userId <$> inviteAndRegisterUser creator tid brig
  -- Still cannot delete, need to make this a full permission member
  deleteUser creator (Just defPassword) brig !!! do
    const 403 === statusCode
    const (Just "no-other-owner") === fmap Error.label . responseJsonMaybe
  -- Let's promote the other user
  updatePermissions creator tid (invitee, Team.fullPermissions) galley
  -- Now the creator can delete the account
  deleteUser creator (Just defPassword) brig !!! const 200 === statusCode
  -- The new full permission member cannot
  deleteUser invitee (Just defPassword) brig !!! const 403 === statusCode
  -- We can still invite new users who can delete their account, regardless of status
  inviteeFull <- userId <$> inviteAndRegisterUser invitee tid brig
  updatePermissions invitee tid (inviteeFull, Team.fullPermissions) galley
  deleteUser inviteeFull (Just defPassword) brig !!! const 200 === statusCode
  inviteeMember <- userId <$> inviteAndRegisterUser invitee tid brig
  deleteUser inviteeMember (Just defPassword) brig !!! const 200 === statusCode
  deleteUser invitee (Just defPassword) brig !!! do
    const 403 === statusCode
    const (Just "no-other-owner") === fmap Error.label . responseJsonMaybe
  -- Ensure internal endpoints are also exercised
  deleteUserInternal invitee brig !!! const 202 === statusCode
  -- Eventually the user will be deleted, leaving the team orphan
  void $ retryWhileN 20 (/= Deleted) (getStatus brig invitee)
  chkStatus brig invitee Deleted

testSSOIsTeamOwner :: Brig -> Galley -> Http ()
testSSOIsTeamOwner brig galley = do
  (creator, tid) <- createUserWithTeam brig galley
  stranger <- userId <$> randomUser brig
  invitee <- userId <$> inviteAndRegisterUser creator tid brig
  let check expectWhat uid = void $ get (brig . paths opath . expectWhat)
        where
          opath = ["i", "users", toByteString' uid, "is-team-owner", toByteString' tid]
  check expect2xx creator
  check expect4xx stranger
  check expect4xx invitee
  updatePermissions creator tid (invitee, Team.fullPermissions) galley
  check expect2xx invitee

testConnectionSameTeam :: Brig -> Galley -> Http ()
testConnectionSameTeam brig galley = do
  (creatorA, tidA) <- createUserWithTeam brig galley
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

-------------------------------------------------------------------------------
-- Search

testNonSearchableDefault :: Brig -> Galley -> Http ()
testNonSearchableDefault brig galley = do
  nonMember <- randomUserWithHandle brig
  email <- randomEmail
  rsp <- register email newTeam brig
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "activation key/code not found"
    Just kc -> activate brig kc !!! const 200 === statusCode
  let Just creator = responseJsonMaybe rsp
  [team] <- view Team.teamListTeams <$> getTeams (userId creator) galley
  let tid = view Team.teamId team
  invitee <- inviteAndRegisterUser (userId creator) tid brig
  creatorWithHandle <- setRandomHandle brig creator
  inviteeWithHandle <- setRandomHandle brig invitee
  refreshIndex brig
  let uid1 = userId nonMember
      uid2 = userId creatorWithHandle
      uid3 = userId inviteeWithHandle
      Just uHandle = fromHandle <$> userHandle nonMember
      Just cHandle = fromHandle <$> userHandle creatorWithHandle
      Just iHandle = fromHandle <$> userHandle inviteeWithHandle
  -- users are searchable by default
  assertSearchable "user is searchable" brig uid1 True
  assertCanFind brig uid2 uid1 uHandle
  -- team owners are not searchable by default
  assertSearchable "owner isn't searchable" brig uid2 False
  assertCan'tFind brig uid1 uid2 cHandle
  -- team members are not searchable by default
  assertSearchable "member isn't searchable" brig uid3 False
  assertCan'tFind brig uid1 uid3 iHandle

testNonSearchableAcrossTeam :: Opt.Opts -> Brig -> Galley -> Http ()
testNonSearchableAcrossTeam opts brig galley = do
  nonMember <- randomUserWithHandle brig
  email <- randomEmail
  rsp <- register email newTeam brig
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "activation key/code not found"
    Just kc -> activate brig kc !!! const 200 === statusCode
  let Just creator = responseJsonMaybe rsp
  [team] <- view Team.teamListTeams <$> getTeams (userId creator) galley
  let tid = view Team.teamId team
  invitee <- inviteAndRegisterUser (userId creator) tid brig
  creatorWithHandle <- setRandomHandle brig creator
  inviteeWithHandle <- setRandomHandle brig invitee
  -- Separate team creator
  emailOther <- randomEmail
  rspOther <- register emailOther newTeam brig
  actOther <- getActivationCode brig (Left emailOther)
  case actOther of
    Nothing -> liftIO $ assertFailure "activation key/code not found"
    Just kc -> activate brig kc !!! const 200 === statusCode
  let Just creatorOther = responseJsonMaybe rspOther
  creatorOtherWithHandle <- setRandomHandle brig creatorOther
  refreshIndex brig
  let uid1 = userId nonMember
      uid2 = userId creatorWithHandle
      uid3 = userId inviteeWithHandle
      uid4 = userId creatorOtherWithHandle
      Just uHandle = fromHandle <$> userHandle nonMember
      uName = fromName $ userName nonMember
      Just cHandle = fromHandle <$> userHandle creatorWithHandle
      cName = fromName $ userName creatorWithHandle
      Just iHandle = fromHandle <$> userHandle inviteeWithHandle
      Just oHandle = fromHandle <$> userHandle creatorOtherWithHandle
      oName = fromName $ userName creatorOtherWithHandle
  -- users are searchable by default
  assertSearchable "user is searchable" brig uid1 True
  assertCanFind brig uid2 uid1 uHandle
  -- users are also searcheable by name
  assertCanFind brig uid3 uid1 uName
  -- team users can also be made searchable
  updateSearchableStatus brig uid4 optIn
  assertSearchable "user is searchable" brig uid4 True
  refreshIndex brig
  assertCanFind brig uid2 uid4 oHandle
  assertCanFind brig uid3 uid4 oName
  -- team owners are not searchable by default
  assertSearchable "owner isn't searchable" brig uid2 False
  assertCan'tFind brig uid1 uid2 cHandle
  -- team members are not searchable by default
  assertSearchable "member isn't searchable" brig uid3 False
  assertCan'tFind brig uid1 uid3 iHandle
  -- make the creator searchable
  updateSearchableStatus brig uid2 optIn
  refreshIndex brig
  -- uid2 is searchable for uid1, uid3 and uid4 now
  assertCanFind brig uid1 uid2 cHandle
  assertCanFind brig uid1 uid2 cName
  assertCanFind brig uid3 uid2 cHandle
  assertCanFind brig uid3 uid2 cName
  assertCanFind brig uid4 uid2 cHandle
  assertCanFind brig uid4 uid2 cName
  let newOpts = opts & Opt.optionSettings . Opt.searchSameTeamOnly .~ Just True
  withSettingsOverrides newOpts $ do
    -- team users cannot search by name nor handle if not a team user
    assertCan'tFind brig uid2 uid1 uHandle
    assertCan'tFind brig uid2 uid1 uName
    -- and also not across
    assertCan'tFind brig uid3 uid4 oHandle
    assertCan'tFind brig uid3 uid4 oName
    -- uid3 can find uid2 because uid2 is visible and they are on the same team
    assertCanFind brig uid3 uid2 cHandle
    assertCanFind brig uid3 uid2 cName

----------------------------------------------------------------------
-- SSO

testCreateUserInternalSSO :: Brig -> Galley -> Http ()
testCreateUserInternalSSO brig galley = do
  teamid <- snd <$> createUserWithTeam brig galley
  let ssoid = UserSSOId "nil" "nil"
  -- creating users requires both sso_id and team_id
  postUser' True False "dummy" True False (Just ssoid) Nothing brig
    !!! const 400 === statusCode
  postUser' True False "dummy" True False Nothing (Just teamid) brig
    !!! const 400 === statusCode
  -- creating user with sso_id, team_id is ok
  resp <- postUser "dummy" True False (Just ssoid) (Just teamid) brig <!! do
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
  (creator, tid) <- createUserWithTeam brig galley
  let ssoid = UserSSOId "nil" "nil"
      mkuser :: Bool -> Http (Maybe User)
      mkuser withemail =
        responseJsonMaybe
          <$> ( postUser "dummy" withemail False (Just ssoid) (Just tid) brig
                  <!! const 201 === statusCode
              )
  -- create and delete sso user (with email)
  Just (userId -> user1) <- mkuser True
  deleteUser user1 (Just defPassword) brig !!! const 200 === statusCode
  -- create sso user with email, delete owner, delete user
  Just (userId -> creator') <- mkuser True
  updatePermissions creator tid (creator', Team.fullPermissions) galley
  deleteUser creator (Just defPassword) brig !!! const 200 === statusCode
  deleteUser creator' (Just defPassword) brig !!! const 403 === statusCode
  -- create sso user without email, delete owner
  Just (userId -> user3) <- mkuser False
  updatePermissions creator' tid (user3, Team.fullPermissions) galley
  deleteUser creator' (Just defPassword) brig !!! const 403 === statusCode
-- TODO:
-- add sso service.  (we'll need a name for that now.)
-- brig needs to notify the sso service about deletions!
-- if the mock sso service disagrees with the deletion: 403 "sso-not-allowed" or something
-- if user is last remaining owner: 403 "no-other-owner" (as above).
-- otherwise: 2xx.
