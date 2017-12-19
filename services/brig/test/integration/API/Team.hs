{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module API.Team (tests) where

import API.Search.Util
import API.Team.Util
import Bilge hiding (accept, timeout, head)
import Bilge.Assert
import Brig.Types hiding (Invitation (..), InvitationRequest (..), InvitationList (..))
import Brig.Types.Team.Invitation
import Brig.Types.User.Auth
import Brig.Types.Intra
import Control.Arrow ((&&&))
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently_, replicateConcurrently)
import Control.Lens ((^.), (^?), view)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Id hiding (client)
import Data.List.Extra (chunksOf)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Client             (Manager)
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Web.Cookie (parseSetCookie, setCookieName)
import Util

import qualified Data.Text.Ascii             as Ascii
import qualified Data.Text.Encoding          as T
import qualified Data.UUID.V4                as UUID
import qualified Network.Wai.Utilities.Error as Error
import qualified Galley.Types.Teams          as Team
import qualified Galley.Types.Teams.Intra    as Team
import qualified Test.Tasty.Cannon           as WS

tests :: Manager -> Brig -> Cannon -> Galley -> IO TestTree
tests m b c g =
    return $ testGroup "team"
        [ testGroup "invitation"
            [ test m "post /teams/:tid/invitations - 201"                  $ testInvitationEmail b g
            , test m "post /teams/:tid/invitations - 403 no permission"    $ testInvitationNoPermission b
            , test m "post /teams/:tid/invitations - 403 too many pending" $ testInvitationTooManyPending b g
            , test m "post /register - 201 accepted"                       $ testInvitationEmailAccepted b g
            , test m "post /register user & team - 201 accepted"           $ testCreateTeam b g
            , test m "post /register user & team - 201 preverified"        $ testCreateTeamPreverified b g
            , test m "post /register - 400 no passwordless"                $ testTeamNoPassword b
            , test m "post /register - 400 code already used"              $ testInvitationCodeExists b g
            , test m "post /register - 400 bad code"                       $ testInvitationInvalidCode b
            , test m "post /register - 400 no wireless"                    $ testInvitationCodeNoIdentity b
            , test m "post /register - 400 mutually exclusive"             $ testInvitationMutuallyExclusive b
            , test m "post /register - 403 too many members"               $ testInvitationTooManyMembers b g
            , test m "get /teams/:tid/invitations - 200 (paging)"          $ testInvitationPaging b g
            , test m "get /teams/:tid/invitations/info - 200"              $ testInvitationInfo b g
            , test m "get /teams/:tid/invitations/info - 400"              $ testInvitationInfoBadCode b
            , test m "post /i/teams/:tid/suspend - 200"                    $ testSuspendTeam b g
            , test m "put /self - 200 update events"                       $ testUpdateEvents b g c
            , test m "delete /self - 200 (ensure no orphan teams)"         $ testDeleteTeamUser b g
            , test m "post /connections - 403 (same binding team)"         $ testConnectionSameTeam b g
            ]
        , testGroup "search"
            [ test m "post /register members are unsearchable"           $ testNonSearchableDefault b g
            ]
        ]

-------------------------------------------------------------------------------
-- Invitation Tests

testUpdateEvents :: Brig -> Galley -> Cannon -> Http ()
testUpdateEvents brig galley cannon = do
    inviteeEmail  <- randomEmail
    alice         <- userId <$> randomUser brig
    tid           <- createTeam alice galley

    -- invite and register Bob
    let invite  = InvitationRequest inviteeEmail (Name "Bob") Nothing
    Just inv <- decodeBody <$> postInvitation brig tid alice invite
    Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
    rsp2 <- post (brig . path "/register"
                       . contentJson
                       . body (accept inviteeEmail inviteeCode))
                  <!! const 201 === statusCode
    let Just bob   = userId <$> decodeBody rsp2

    -- ensure Alice and Bob are not connected
    void $ getConnection brig bob alice <!! const 404 === statusCode
    void $ getConnection brig alice bob <!! const 404 === statusCode

    -- Alice updates her profile
    let newColId   = Just 5
        newAssets  = Just [ImageAsset "abc" (Just AssetComplete)]
        newName    = Just $ Name "Alice in Wonderland"
        newPic     = Nothing -- Legacy
        userUpdate = UserUpdate newName newPic newAssets newColId
        update     = RequestBodyLBS . encode $ userUpdate

    -- Update profile & receive notification
    WS.bracketRN cannon [alice, bob] $ \[aliceWS, bobWS] -> do
        put (brig . path "/self" . contentJson . zUser alice . zConn "c" . body update) !!!
            const 200 === statusCode
        liftIO $ mapConcurrently_ (\ws -> assertUpdateNotification ws alice userUpdate) [aliceWS, bobWS]

testInvitationEmail :: Brig -> Galley -> Http ()
testInvitationEmail brig galley = do
    invitee <- randomEmail
    inviter <- userId <$> randomUser brig
    tid     <- createTeam inviter galley
    let invite = InvitationRequest invitee (Name "Bob") Nothing
    void $ postInvitation brig tid inviter invite

-- TODO: Use max team size from options
testInvitationTooManyPending :: Brig -> Galley -> Http ()
testInvitationTooManyPending brig galley = do
    inviter <- userId <$> randomUser brig
    tid     <- createTeam inviter galley
    emails  <- replicateConcurrently 128 randomEmail
    let invite e = InvitationRequest e (Name "Bob") Nothing
    mapM_ (mapConcurrently_ $ postInvitation brig tid inviter . invite) (chunksOf 16 emails)
    e <- randomEmail
    postInvitation brig tid inviter (invite e) !!! do
        const 403 === statusCode
        const (Just "too-many-team-invitations") === fmap Error.label . decodeBody

testInvitationEmailAccepted :: Brig -> Galley -> Http ()
testInvitationEmailAccepted brig galley = do
    inviteeEmail <- randomEmail
    inviter <- userId <$> randomUser brig
    tid     <- createTeam inviter galley
    let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing
    Just inv <- decodeBody <$> postInvitation brig tid inviter invite
    Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
    rsp2 <- post (brig . path "/register"
                       . contentJson
                       . body (accept inviteeEmail inviteeCode))
                  <!! const 201 === statusCode
    let Just (invitee, Just email2) = (userId &&& userEmail) <$> decodeBody rsp2
    let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
    liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)
    -- Verify that the invited user is active
    login brig (defEmailLogin email2) PersistentCookie !!! const 200 === statusCode
    -- Verify that the user is part of the team
    mem <- getTeamMember invitee tid galley
    liftIO $ assertBool "Member not part of the team" (invitee == mem ^. Team.userId)
    conns <- listConnections invitee brig
    liftIO $ assertBool "User should have no connections" (null (clConnections conns) && not (clHasMore conns))

testCreateTeam :: Brig -> Galley -> Http ()
testCreateTeam brig galley = do
    email <- randomEmail
    rsp <- register email newTeam brig
    let Just uid = userId <$> decodeBody rsp
    -- Verify that the user is part of exactly one (binding) team
    teams <- view Team.teamListTeams <$> getTeams uid galley
    liftIO $ assertBool "User not part of exactly one team" (length teams == 1)
    let team = fromMaybe (error "No team??") $ listToMaybe teams
    liftIO $ assertBool "Team not binding" (team^.Team.teamBinding == Team.Binding)
    mem <- getTeamMember uid (team^.Team.teamId) galley
    liftIO $ assertBool "Member not part of the team" (uid == mem ^. Team.userId)
    -- Verify that the user cannot send invitations before activating their account
    inviteeEmail <- randomEmail
    let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing
    postInvitation brig (team^.Team.teamId) uid invite !!! const 403 === statusCode
    -- Verify that the team is still in status "pending"
    team2 <- getTeam galley (team^.Team.teamId)
    liftIO $ assertEqual "status" Team.PendingActive (Team.tdStatus team2)
    -- Activate account
    act <- getActivationCode brig (Left email)
    case act of
        Nothing -> liftIO $ assertFailure "activation key/code not found"
        Just kc -> activate brig kc !!! const 200 === statusCode
    -- Verify that Team has status Active now
    team3 <- getTeam galley (team^.Team.teamId)
    liftIO $ assertEqual "status" Team.Active (Team.tdStatus team3)

testCreateTeamPreverified :: Brig -> Galley -> Http ()
testCreateTeamPreverified brig galley = do
    email <- randomEmail
    requestActivationCode brig (Left email)
    act <- getActivationCode brig (Left email)
    case act of
        Nothing     -> liftIO $ assertFailure "activation key/code not found"
        Just (_, c) -> do
            rsp <- register' email newTeam c brig <!! const 201 === statusCode
            let Just uid = userId <$> decodeBody rsp
            teams <- view Team.teamListTeams <$> getTeams uid galley
            liftIO $ assertBool "User not part of exactly one team" (length teams == 1)
            let team = fromMaybe (error "No team??") $ listToMaybe teams
            liftIO $ assertBool "Team not binding" (team^.Team.teamBinding == Team.Binding)
            mem <- getTeamMember uid (team^.Team.teamId) galley
            liftIO $ assertBool "Member not part of the team" (uid == mem ^. Team.userId)
            team2 <- getTeam galley (team^.Team.teamId)
            liftIO $ assertEqual "Team should already be active" Team.Active (Team.tdStatus team2)
            -- Verify that the user can already send invitations before activating their account
            inviteeEmail <- randomEmail
            let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing
            postInvitation brig (team^.Team.teamId) uid invite !!! const 201 === statusCode

testInvitationNoPermission :: Brig -> Http ()
testInvitationNoPermission brig = do
    alice <- userId <$> randomUser brig
    tid   <- randomTeamId
    email <- randomEmail
    let invite = InvitationRequest email (Name "Bob") Nothing
    postInvitation brig tid alice invite !!! do
        const 403 === statusCode
        const (Just "insufficient-permissions") === fmap Error.label . decodeBody

testTeamNoPassword :: Brig -> Http ()
testTeamNoPassword brig = do
    e <- randomEmail
    -- Team creators must have a password
    post (brig . path "/register" . contentJson . body (
        RequestBodyLBS . encode  $ object
            [ "name"  .= ("Bob" :: Text)
            , "email" .= fromEmail e
            , "team"  .= newTeam
            ]
        )) !!! const 400 === statusCode
    -- And so do any other binding team members
    code <- liftIO $ InvitationCode . Ascii.encodeBase64Url <$> randomBytes 24
    post (brig . path "/register" . contentJson . body (
        RequestBodyLBS . encode  $ object
            [ "name"      .= ("Bob" :: Text)
            , "email"     .= fromEmail e
            , "team_code" .= code
            ]
        )) !!! const 400 === statusCode

testInvitationCodeExists :: Brig -> Galley -> Http ()
testInvitationCodeExists brig galley = do
    email <- randomEmail
    uid   <- userId <$> randomUser brig
    tid   <- createTeam uid galley
    rsp   <- postInvitation brig tid uid (invite email) <!! const 201 === statusCode

    let Just invId = inInvitation <$> decodeBody rsp
    Just invCode <- getInvitationCode brig tid invId

    post (brig . path "/register" . contentJson . body (accept email invCode)) !!!
        const 201 === statusCode

    post (brig . path "/register" . contentJson . body (accept email invCode)) !!! do
        const 409                 === statusCode
        const (Just "key-exists") === fmap Error.label . decodeBody

    email2 <- randomEmail
    post (brig . path "/register" . contentJson . body (accept email2 invCode)) !!! do
        const 400 === statusCode
        const (Just "invalid-invitation-code") === fmap Error.label . decodeBody
 where
    invite email = InvitationRequest email (Name "Bob") Nothing

testInvitationInvalidCode :: Brig -> Http ()
testInvitationInvalidCode brig = do
    email <- randomEmail
    -- Syntactically invalid
    let code1 = InvitationCode (Ascii.unsafeFromText "8z6JVcO1o4o¿9kFeb4Y3N-BmhIjH6b33")
    post (brig . path "/register" . contentJson . body (accept email code1)) !!! do
        const 400 === statusCode
        const (Just "bad-request") === fmap Error.label . decodeBody
    -- Syntactically valid but semantically invalid
    code2 <- liftIO $ InvitationCode . Ascii.encodeBase64Url <$> randomBytes 24
    post (brig . path "/register" . contentJson . body (accept email code2)) !!! do
        const 400 === statusCode
        const (Just "invalid-invitation-code") === fmap Error.label . decodeBody

testInvitationCodeNoIdentity :: Brig -> Http ()
testInvitationCodeNoIdentity brig = do
    uid <- liftIO $ Id <$> UUID.nextRandom
    post (brig . path "/register" . contentJson . body (payload uid)) !!! do
        const 403                       === statusCode
        const (Just "missing-identity") === fmap Error.label . decodeBody
  where
    payload u = RequestBodyLBS . encode $ object
        [ "name"      .= ("Bob" :: Text)
        , "team_code" .= u
        , "password"  .= defPassword
        ]

testInvitationMutuallyExclusive :: Brig -> Http ()
testInvitationMutuallyExclusive brig = do
    email <- randomEmail
    code <- liftIO $ InvitationCode . Ascii.encodeBase64Url <$> randomBytes 24
    req email (Just code) (Just newTeam) Nothing      !!! const 400 === statusCode
    req email (Just code) Nothing        (Just code)  !!! const 400 === statusCode
    req email Nothing     (Just newTeam) (Just code)  !!! const 400 === statusCode
    req email (Just code) (Just newTeam) (Just code)  !!! const 400 === statusCode
  where
    req :: Email -> Maybe InvitationCode -> Maybe Team.BindingNewTeam -> Maybe InvitationCode
        -> HttpT IO (Response (Maybe ByteString))
    req e c t i = post (brig . path "/register" . contentJson . body (
        RequestBodyLBS . encode  $ object
            [ "name"            .= ("Bob" :: Text)
            , "email"           .= fromEmail e
            , "password"        .= defPassword
            , "team_code"       .= c
            , "team"            .= t
            , "invitation_code" .= i
            ]
        ))

testInvitationTooManyMembers :: Brig -> Galley -> Http ()
testInvitationTooManyMembers brig galley = do
    creator <- userId <$> randomUser brig
    tid     <- createTeam creator galley
    uids    <- fmap toNewMember <$> replicateConcurrently 127 randomId
    mapM_ (mapConcurrently_ (addTeamMember galley tid)) $ chunksOf 16 uids

    em <- randomEmail
    let invite  = InvitationRequest em (Name "Bob") Nothing
    Just inv <- decodeBody <$> postInvitation brig tid creator invite
    Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
    post (brig . path "/register"
               . contentJson
               . body (accept em inviteeCode)) !!! do
        const 403 === statusCode
        const (Just "too-many-team-members") === fmap Error.label . decodeBody
  where
    toNewMember u = Team.newNewTeamMember $ Team.newTeamMember u Team.fullPermissions

testInvitationPaging :: Brig -> Galley -> Http ()
testInvitationPaging b g = do
    u     <- userId <$> randomUser b
    tid   <- createTeam u g
    replicateM_ total $ do
        email <- randomEmail
        postInvitation b tid u (invite email) !!! const 201 === statusCode
    foldM_ (next u tid 2) (0, Nothing) [2,2,1,0]
    foldM_ (next u tid total) (0, Nothing) [total,0]
  where
    total = 5

    next :: UserId -> TeamId -> Int -> (Int, Maybe InvitationId) -> Int -> Http (Int, Maybe InvitationId)
    next u t step (count, start) n = do
        let count' = count + step
        let range = queryRange (toByteString' <$> start) (Just step)
        r <- get (b . paths ["teams", toByteString' t, "invitations"] . zUser u . range) <!!
            const 200 === statusCode
        let (invs, more) = (fmap ilInvitations &&& fmap ilHasMore) $ decodeBody r
        liftIO $ assertEqual "page size" (Just n) (length <$> invs)
        liftIO $ assertEqual "has more" (Just (count' < total)) more
        return . (count',) $ invs >>= fmap inInvitation . listToMaybe . reverse

    invite email = InvitationRequest email (Name "Bob") Nothing

testInvitationInfo :: Brig -> Galley -> Http ()
testInvitationInfo brig galley = do
    email    <- randomEmail
    uid      <- userId <$> randomUser brig
    tid      <- createTeam uid galley
    let invite = InvitationRequest email (Name "Bob") Nothing
    Just inv <- decodeBody <$> postInvitation brig tid uid invite

    Just invCode    <- getInvitationCode brig tid (inInvitation inv)
    Just invitation <- getInvitation brig invCode

    liftIO $ assertEqual "Invitations differ" inv invitation

testInvitationInfoBadCode :: Brig -> Http ()
testInvitationInfoBadCode brig = do
    -- The code contains non-ASCII characters after url-decoding
    let icode = "8z6JVcO1o4o%C2%BF9kFeb4Y3N-BmhIjH6b33"
    get (brig . path ("/teams/invitations/info?code=" <> icode)) !!!
        const 400 === statusCode

testSuspendTeam :: Brig -> Galley -> Http ()
testSuspendTeam brig galley = do
    inviteeEmail  <- randomEmail
    inviteeEmail2 <- randomEmail
    inviter       <- userId <$> randomUser brig
    tid           <- createTeam inviter galley

    -- invite and register invitee
    let invite  = InvitationRequest inviteeEmail (Name "Bob") Nothing
    Just inv <- decodeBody <$> postInvitation brig tid inviter invite
    Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
    rsp2 <- post (brig . path "/register"
                       . contentJson
                       . body (accept inviteeEmail inviteeCode))
                  <!! const 201 === statusCode
    let Just (invitee, Just email) = (userId &&& userEmail) <$> decodeBody rsp2

    -- invite invitee2 (don't register)
    let invite2 = InvitationRequest inviteeEmail2 (Name "Bob") Nothing
    Just inv2 <- decodeBody <$> postInvitation brig tid inviter invite2
    Just _ <- getInvitationCode brig tid (inInvitation inv2)

    -- suspend team
    suspendTeam brig tid !!! const 200 === statusCode
    -- login fails
    login brig (defEmailLogin email) PersistentCookie !!! do
       const 403 === statusCode
       const (Just "suspended") === fmap Error.label . decodeBody
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
    creator <- userId <$> randomUser brig
    tid     <- createTeam creator galley
    -- Cannot delete the user since it will make the team orphan
    deleteUser creator (Just defPassword) brig !!! do
        const 403 === statusCode
        const (Just "no-other-owner") === fmap Error.label . decodeBody
    -- We need to invite another user to a full permission member
    invitee <- userId <$> inviteAndRegisterUser creator tid brig
    -- Still cannot delete, need to make this a full permission member
    deleteUser creator (Just defPassword) brig !!! do
        const 403 === statusCode
        const (Just "no-other-owner") === fmap Error.label . decodeBody
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
        const (Just "no-other-owner") === fmap Error.label . decodeBody

testConnectionSameTeam :: Brig -> Galley -> Http ()
testConnectionSameTeam brig galley = do
    creatorA <- userId <$> randomUser brig
    tidA     <- createTeam creatorA galley
    inviteeA <- userId <$> inviteAndRegisterUser creatorA tidA brig

    postConnection brig creatorA inviteeA !!! do
        const 403 === statusCode
        const (Just "same-binding-team-users") === fmap Error.label . decodeBody

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
    email     <- randomEmail
    rsp       <- register email newTeam brig

    act <- getActivationCode brig (Left email)
    case act of
      Nothing -> liftIO $ assertFailure "activation key/code not found"
      Just kc -> activate brig kc !!! const 200 === statusCode

    let Just creator = decodeBody rsp
    [team]  <- view Team.teamListTeams <$> getTeams (userId creator) galley
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

-------------------------------------------------------------------------------
-- Utilities

listConnections :: UserId -> Brig -> Http UserConnectionList
listConnections u brig = do
    r <- get $ brig
             . path "connections"
             . zUser u
    return $ fromMaybe (error "listConnections: failed to parse response") (decodeBody r)

getInvitation :: Brig -> InvitationCode -> Http (Maybe Invitation)
getInvitation brig c = do
    r <- get $ brig
             . path "/teams/invitations/info"
             . queryItem "code" (toByteString' c)
    return . decode . fromMaybe "" $ responseBody r

postInvitation :: Brig -> TeamId -> UserId -> InvitationRequest -> Http ResponseLBS
postInvitation brig t u i = post $ brig
    . paths ["teams", toByteString' t, "invitations"]
    . contentJson
    . body (RequestBodyLBS $ encode i)
    . zAuthAccess u "conn"

suspendTeam :: Brig -> TeamId -> HttpT IO (Response (Maybe ByteString))
suspendTeam brig t = post $ brig
    . paths ["i", "teams", toByteString' t, "suspend"]
    . contentJson

unsuspendTeam :: Brig -> TeamId -> Http ResponseLBS
unsuspendTeam brig t = post $ brig
    . paths ["i", "teams", toByteString' t, "unsuspend"]
    . contentJson

getTeam :: Galley -> TeamId -> Http Team.TeamData
getTeam galley t = do
    r <- get $ galley . paths ["i", "teams", toByteString' t]
    return $ fromMaybe (error "getTeam: failed to parse response") (decodeBody r)

getInvitationCode :: Brig -> TeamId -> InvitationId -> Http (Maybe InvitationCode)
getInvitationCode brig t ref = do
    r <- get ( brig
             . path "/i/teams/invitation-code"
             . queryItem "team" (toByteString' t)
             . queryItem "invitation_id" (toByteString' ref)
             )
    let lbs   = fromMaybe "" $ responseBody r
    return $ fromByteString . fromMaybe (error "No code?") $ T.encodeUtf8 <$> (lbs ^? key "code"  . _String)

assertNoInvitationCode :: Brig -> TeamId -> InvitationId -> Http ()
assertNoInvitationCode brig t i =
    get ( brig
        . path "/i/teams/invitation-code"
        . queryItem "team" (toByteString' t)
        . queryItem "invitation_id" (toByteString' i)
        ) !!! do
          const 400 === statusCode
          const (Just "invalid-invitation-code") === fmap Error.label . decodeBody

randomTeamId :: MonadIO m => m TeamId
randomTeamId = Id <$> liftIO UUID.nextRandom

getTeamMember :: UserId -> TeamId -> Galley -> Http Team.TeamMember
getTeamMember u tid galley = do
    r <- get ( galley
             . paths ["i", "teams", toByteString' tid, "members", toByteString' u]
             . zUser u
             . expect2xx
             )
    return $ fromMaybe (error "getTeamMember: failed to parse response") (decodeBody r)

getTeams :: UserId -> Galley -> Http Team.TeamList
getTeams u galley = do
    r <- get ( galley
             . paths ["teams"]
             . zAuthAccess u "conn"
             . expect2xx
             )
    return $ fromMaybe (error "getTeams: failed to parse response") (decodeBody r)

accept :: Email -> InvitationCode -> RequestBody
accept email code = RequestBodyLBS . encode $ object
    [ "name"      .= ("Bob" :: Text)
    , "email"     .= fromEmail email
    , "password"  .= defPassword
    , "team_code" .= code
    ]

register :: Email -> Team.BindingNewTeam -> Brig -> HttpT IO (Response (Maybe ByteString))
register e t brig = post (brig . path "/register" . contentJson . body (
    RequestBodyLBS . encode  $ object
        [ "name"            .= ("Bob" :: Text)
        , "email"           .= fromEmail e
        , "password"        .= defPassword
        , "team"            .= t
        ]
    ))

register' :: Email -> Team.BindingNewTeam -> ActivationCode -> Brig -> HttpT IO (Response (Maybe ByteString))
register' e t c brig = post (brig . path "/register" . contentJson . body (
    RequestBodyLBS . encode  $ object
        [ "name"            .= ("Bob" :: Text)
        , "email"           .= fromEmail e
        , "email_code"      .= c
        , "password"        .= defPassword
        , "team"            .= t
        ]
    ))

inviteAndRegisterUser :: UserId -> TeamId -> Brig -> HttpT IO User
inviteAndRegisterUser u tid brig = do
    inviteeEmail <- randomEmail
    let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing
    Just inv <- decodeBody <$> postInvitation brig tid u invite
    Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
    rspInvitee <- post (brig . path "/register"
                             . contentJson
                             . body (accept inviteeEmail inviteeCode)) <!! const 201 === statusCode

    let Just invitee = decodeBody rspInvitee
    return invitee

updatePermissions :: UserId -> TeamId -> (UserId, Team.Permissions) -> Galley -> HttpT IO ()
updatePermissions from tid (to, perm) galley =
    put ( galley
        . paths ["teams", toByteString' tid, "members"]
        . zUser from
        . zConn "conn"
        . Bilge.json changeMember
        ) !!! const 200 === statusCode
  where
    changeMember = Team.newNewTeamMember $ Team.newTeamMember to perm
