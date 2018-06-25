{-# LANGUAGE OverloadedStrings #-}

module API.Teams (tests) where

import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens hiding ((#), (.=))
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Foldable (forM_, for_)
import Data.Id
import Data.List1
import Data.Misc (PlainTextPassword (..))
import Data.Monoid
import Data.Range
import Galley.Types hiding (EventType (..), EventData (..), MemberUpdate (..))
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Gundeck.Types.Notification
import Test.Tasty
import Test.Tasty.Cannon (Cannon, TimeoutUnit (..), (#))
import Test.Tasty.HUnit
import API.SQS

import qualified API.Util as Util
import qualified Control.Concurrent.Async.Lifted.Safe as AsyncSafe
import qualified Data.Currency as Currency
import qualified Data.List1 as List1
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Galley.Types as Conv
import qualified Network.Wai.Utilities.Error as Error
import qualified Test.Tasty.Cannon as WS
import qualified Galley.Aws as Aws

type TestSignature a = Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http a

test :: IO TestSetup -> TestName -> TestSignature a -> TestTree
test s n h = testCase n runTest
  where
    runTest = do
        setup <- s
        void $ runHttpT (manager setup) (h (galley setup) (brig setup) (cannon setup) (awsEnv setup))

tests :: IO TestSetup -> TestTree
tests s = testGroup "Teams API"
    [ test s "create team" testCreateTeam
    , test s "create multiple binding teams fail" testCreateMulitpleBindingTeams
    , test s "create binding team with currency" testCreateBindingTeamWithCurrency
    , test s "create team with members" testCreateTeamWithMembers
    , test s "create 1-1 conversation between binding team members (fail)" testCreateOne2OneFailNonBindingTeamMembers
    , test s "create 1-1 conversation between binding team members" testCreateOne2OneWithMembers
    , test s "add new team member" testAddTeamMember
    , test s "add new team member binding teams" testAddTeamMemberCheckBound
    , test s "add new team member internal" testAddTeamMemberInternal
    , test s "remove team member" testRemoveTeamMember
    , test s "remove team member (binding)" testRemoveBindingTeamMember
    , test s "add team conversation" testAddTeamConv
    , test s "add managed conversation through public endpoint (fail)" testAddManagedConv
    , test s "add managed team conversation ignores given users" testAddTeamConvWithUsers
    , test s "add team member to conversation without connection" testAddTeamMemberToConv
    , test s "delete non-binding team" testDeleteTeam
    , test s "delete binding team" testDeleteBindingTeam
    , test s "delete team conversation" testDeleteTeamConv
    , test s "update team data" testUpdateTeam
    , test s "update team member" testUpdateTeamMember
    , test s "update team status" testUpdateTeamStatus
    , test s "post crypto broadcast message json" postCryptoBroadcastMessageJson
    , test s "post crypto broadcast message protobuf" postCryptoBroadcastMessageProto
    , test s "post crypto broadcast message redundant/missing" postCryptoBroadcastMessageJson2
    , test s "post crypto broadcast message no-team" postCryptoBroadcastMessageNoTeam
    , test s "post crypto broadcast message 100 (or max conns)" postCryptoBroadcastMessage100OrMaxConns
    ]

timeout :: WS.Timeout
timeout = 3 # Second

testCreateTeam :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testCreateTeam g b c a = do
    owner <- Util.randomUser b
    WS.bracketR c owner $ \wsOwner -> do
        tid   <- Util.createTeam g "foo" owner []
        team  <- Util.getTeam g owner tid
        assertQueueEmpty a
        liftIO $ do
            assertEqual "owner" owner (team^.teamCreator)
            eventChecks <- WS.awaitMatch timeout wsOwner $ \notif -> do
                ntfTransient notif @?= False
                let e = List1.head (WS.unpackPayload notif)
                e^.eventType @?= TeamCreate
                e^.eventTeam @?= tid
                e^.eventData @?= Just (EdTeamCreate team)
            void $ WS.assertSuccess eventChecks

testCreateMulitpleBindingTeams :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testCreateMulitpleBindingTeams g b _ a = do
    owner <- Util.randomUser b
    _     <- Util.createTeamInternal g "foo" owner
    assertQueue "create team" a tActivate
    -- Cannot create more teams if bound (used internal API)
    let nt = NonBindingNewTeam $ newNewTeam (unsafeRange "owner") (unsafeRange "icon")
    post (g . path "/teams" . zUser owner . zConn "conn" . json nt) !!!
        const 403 === statusCode

    -- If never used the internal API, can create multiple teams
    owner' <- Util.randomUser b
    void $ Util.createTeam g "foo" owner' []
    void $ Util.createTeam g "foo" owner' []

testCreateBindingTeamWithCurrency :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testCreateBindingTeamWithCurrency g b _ a = do
    _owner <- Util.randomUser b
    _      <- Util.createTeamInternal g "foo" _owner
    -- Backwards compatible
    assertQueue "create team" a (tActivateWithCurrency Nothing)

    -- Ensure currency is properly journaled
    _owner <- Util.randomUser b
    _      <- Util.createTeamInternalWithCurrency g "foo" _owner Currency.USD
    assertQueue "create team" a (tActivateWithCurrency $ Just Currency.USD)

testCreateTeamWithMembers :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testCreateTeamWithMembers g b c _ = do
    owner <- Util.randomUser b
    user1 <- Util.randomUser b
    user2 <- Util.randomUser b
    let pp = Util.symmPermissions [CreateConversation, AddConversationMember]
    let m1 = newTeamMember user1 pp
    let m2 = newTeamMember user2 pp
    Util.connectUsers b owner (list1 user1 [user2])
    WS.bracketR3 c owner user1 user2 $ \(wsOwner, wsUser1, wsUser2) -> do
        tid  <- Util.createTeam g "foo" owner [m1, m2]
        team <- Util.getTeam g owner tid
        mem  <- Util.getTeamMembers g owner tid
        liftIO $ do
            assertEqual "members"
                (Set.fromList [newTeamMember owner fullPermissions, m1, m2])
                (Set.fromList (mem^.teamMembers))
            void $ mapConcurrently (checkCreateEvent team) [wsOwner, wsUser1, wsUser2]
  where
    checkCreateEvent team w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= TeamCreate
        e^.eventTeam @?= (team^.teamId)
        e^.eventData @?= Just (EdTeamCreate team)

testCreateOne2OneFailNonBindingTeamMembers :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testCreateOne2OneFailNonBindingTeamMembers g b _ a = do
    owner <- Util.randomUser b
    let p1 = Util.symmPermissions [CreateConversation, AddConversationMember]
    let p2 = Util.symmPermissions [CreateConversation, AddConversationMember, AddTeamMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    mem2 <- flip newTeamMember p2 <$> Util.randomUser b
    Util.connectUsers b owner (list1 (mem1^.userId) [mem2^.userId])
    tid <- Util.createTeam g "foo" owner [mem1, mem2]
    -- Cannot create a 1-1 conversation, not connected and in the same team but not binding
    Util.createOne2OneTeamConv g (mem1^.userId) (mem2^.userId) Nothing tid !!! do
        const 404 === statusCode
        const "non-binding-team" === (Error.label . Util.decodeBody' "error label")
    -- Both have a binding team but not the same team
    owner1 <- Util.randomUser b
    tid1 <- Util.createTeamInternal g "foo" owner1
    assertQueue "create team" a tActivate
    owner2 <- Util.randomUser b
    void $ Util.createTeamInternal g "foo" owner2
    assertQueue "create another team" a tActivate
    Util.createOne2OneTeamConv g owner1 owner2 Nothing tid1 !!! do
        const 403 === statusCode
        const "non-binding-team-members" === (Error.label . Util.decodeBody' "error label")

testCreateOne2OneWithMembers :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testCreateOne2OneWithMembers g b c a = do
    owner <- Util.randomUser b
    tid   <- Util.createTeamInternal g "foo" owner
    assertQueue "create team" a tActivate
    let p1 = Util.symmPermissions [CreateConversation]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b

    WS.bracketR c (mem1^.userId) $ \wsMem1 -> do
        Util.addTeamMemberInternal g tid mem1
        checkTeamMemberJoin tid (mem1^.userId) wsMem1
        assertQueue "team member join" a $ tUpdate 2 [owner]
        WS.assertNoEvent timeout [wsMem1]

    void $ retryWhileN 10 repeatIf (Util.createOne2OneTeamConv g owner (mem1^.userId) Nothing tid)

    -- Recreating a One2One is a no-op, returns a 200
    Util.createOne2OneTeamConv g owner (mem1^.userId) Nothing tid !!! const 200 === statusCode
  where
    repeatIf :: Util.ResponseLBS -> Bool
    repeatIf r = statusCode r /= 201

testAddTeamMember :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testAddTeamMember g b c _ = do
    owner <- Util.randomUser b
    let p1 = Util.symmPermissions [CreateConversation, AddConversationMember]
    let p2 = Util.symmPermissions [CreateConversation, AddConversationMember, AddTeamMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    mem2 <- flip newTeamMember p2 <$> Util.randomUser b
    Util.connectUsers b owner (list1 (mem1^.userId) [mem2^.userId])
    Util.connectUsers b (mem1^.userId) (list1 (mem2^.userId) [])
    tid <- Util.createTeam g "foo" owner [mem1, mem2]

    mem3 <- flip newTeamMember p1 <$> Util.randomUser b
    let payload = json (newNewTeamMember mem3)
    Util.connectUsers b (mem1^.userId) (list1 (mem3^.userId) [])
    Util.connectUsers b (mem2^.userId) (list1 (mem3^.userId) [])

    -- `mem1` lacks permission to add new team members
    post (g . paths ["teams", toByteString' tid, "members"] . zUser (mem1^.userId) . payload) !!!
        const 403 === statusCode

    WS.bracketRN c [owner, (mem1^.userId), (mem2^.userId), (mem3^.userId)] $ \[wsOwner, wsMem1, wsMem2, wsMem3] -> do
        -- `mem2` has `AddTeamMember` permission
        Util.addTeamMember g (mem2^.userId) tid mem3
        AsyncSafe.mapConcurrently_ (checkTeamMemberJoin tid (mem3^.userId)) [wsOwner, wsMem1, wsMem2, wsMem3]

testAddTeamMemberCheckBound :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testAddTeamMemberCheckBound g b _ a = do
    ownerBound <- Util.randomUser b
    tidBound   <- Util.createTeamInternal g "foo" ownerBound
    assertQueue "create team" a tActivate

    rndMem <- flip newTeamMember (Util.symmPermissions []) <$> Util.randomUser b
    -- Cannot add any users to bound teams
    post (g . paths ["teams", toByteString' tidBound, "members"] . zUser ownerBound . zConn "conn" . json (newNewTeamMember rndMem)) !!!
        const 403 === statusCode

    owner <- Util.randomUser b
    tid   <- Util.createTeam g "foo" owner []
    -- Cannot add bound users to any teams
    let boundMem = newTeamMember ownerBound (Util.symmPermissions [])
    post (g . paths ["teams", toByteString' tid, "members"] . zUser owner . zConn "conn" . json (newNewTeamMember boundMem)) !!!
        const 403 === statusCode

testAddTeamMemberInternal :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testAddTeamMemberInternal g b c a = do
    owner <- Util.randomUser b
    tid <- Util.createTeam g "foo" owner []
    let p1 = Util.symmPermissions [GetBilling] -- permissions are irrelevant on internal endpoint
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b

    WS.bracketRN c [owner, mem1^.userId] $ \[wsOwner, wsMem1] -> do
        Util.addTeamMemberInternal g tid mem1
        liftIO . void $ mapConcurrently (checkJoinEvent tid (mem1^.userId)) [wsOwner, wsMem1]
        assertQueue "tem member join" a $ tUpdate 2 [owner]
    void $ Util.getTeamMemberInternal g tid (mem1^.userId)
  where
    checkJoinEvent tid usr w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= MemberJoin
        e^.eventTeam @?= tid
        e^.eventData @?= Just (EdMemberJoin usr)

testRemoveTeamMember :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testRemoveTeamMember g b c _ = do
    owner <- Util.randomUser b
    let p1 = Util.symmPermissions [AddConversationMember]
    let p2 = Util.symmPermissions [AddConversationMember, RemoveTeamMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    mem2 <- flip newTeamMember p2 <$> Util.randomUser b
    mext1 <- Util.randomUser b
    mext2 <- Util.randomUser b
    mext3 <- Util.randomUser b
    Util.connectUsers b owner (list1 (mem1^.userId) [mem2^.userId, mext1, mext2, mext3])
    tid <- Util.createTeam g "foo" owner [mem1, mem2]

    -- Managed conversation:
    void $ Util.createManagedConv g owner (ConvTeamInfo tid True) [] (Just "gossip") Nothing Nothing
    -- Regular conversation:
    cid2 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [mem1^.userId, mem2^.userId, mext1] (Just "blaa") Nothing Nothing
    -- Member external 2 is a guest and not a part of any conversation that mem1 is a part of
    void $ Util.createTeamConv g owner (ConvTeamInfo tid False) [mem2^.userId, mext2] (Just "blaa") Nothing Nothing
    -- Member external 3 is a guest and part of a conversation that mem1 is a part of
    cid3 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [mem1^.userId, mext3] (Just "blaa") Nothing Nothing

    WS.bracketRN c [owner, mem1^.userId, mem2^.userId, mext1, mext2, mext3] $ \ws@[wsOwner, wsMem1, wsMem2, wsMext1, _wsMext2, wsMext3] -> do
        -- `mem1` lacks permission to remove team members
        delete ( g
               . paths ["teams", toByteString' tid, "members", toByteString' (mem2^.userId)]
               . zUser (mem1^.userId)
               . zConn "conn"
               ) !!! const 403 === statusCode

        -- `mem2` has `RemoveTeamMember` permission
        delete ( g
               . paths ["teams", toByteString' tid, "members", toByteString' (mem1^.userId)]
               . zUser (mem2^.userId)
               . zConn "conn"
               ) !!! const 200 === statusCode

        -- Ensure that `mem1` is still a user (tid is not a binding team)
        Util.ensureDeletedState b False owner (mem1^.userId)

        AsyncSafe.mapConcurrently_ (checkTeamMemberLeave tid (mem1^.userId)) [wsOwner, wsMem1, wsMem2]
        checkConvMemberLeaveEvent cid2 (mem1^.userId) wsMext1
        checkConvMemberLeaveEvent cid3 (mem1^.userId) wsMext3
        WS.assertNoEvent timeout ws

testRemoveBindingTeamMember :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testRemoveBindingTeamMember g b c a = do
    owner <- Util.randomUser b
    tid   <- Util.createTeamInternal g "foo" owner
    assertQueue "create team" a tActivate
    mext  <- Util.randomUser b
    let p1 = Util.symmPermissions [AddConversationMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    Util.addTeamMemberInternal g tid mem1
    assertQueue "team member join" a $ tUpdate 2 [owner]
    Util.connectUsers b owner (singleton mext)
    cid1 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [(mem1^.userId), mext] (Just "blaa") Nothing Nothing

    -- Deleting from a binding team without a password is a bad request
    delete ( g
           . paths ["teams", toByteString' tid, "members", toByteString' (mem1^.userId)]
           . zUser owner
           . zConn "conn"
           ) !!! const 400 === statusCode

    delete ( g
           . paths ["teams", toByteString' tid, "members", toByteString' (mem1^.userId)]
           . zUser owner
           . zConn "conn"
           . json (newTeamMemberDeleteData (PlainTextPassword "wrong passwd"))
           ) !!! do
        const 403 === statusCode
        const "access-denied" === (Error.label . Util.decodeBody' "error label")

    -- Mem1 is still part of Wire
    Util.ensureDeletedState b False owner (mem1^.userId)

    WS.bracketR2 c owner mext $ \(wsOwner, wsMext) -> do
        delete ( g
               . paths ["teams", toByteString' tid, "members", toByteString' (mem1^.userId)]
               . zUser owner
               . zConn "conn"
               . json (newTeamMemberDeleteData (PlainTextPassword Util.defPassword))
               ) !!! const 202 === statusCode

        checkTeamMemberLeave tid (mem1^.userId) wsOwner
        checkConvMemberLeaveEvent cid1 (mem1^.userId) wsMext

        assertQueue "team member leave" a $ tUpdate 1 [owner]
        WS.assertNoEvent timeout [wsMext]
        -- Mem1 is now gone from Wire
        Util.ensureDeletedState b True owner (mem1^.userId)

testAddTeamConv :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testAddTeamConv g b c _ = do
    owner  <- Util.randomUser b
    extern <- Util.randomUser b

    let p = Util.symmPermissions [CreateConversation, AddConversationMember]
    mem1 <- flip newTeamMember p <$> Util.randomUser b
    mem2 <- flip newTeamMember p <$> Util.randomUser b

    Util.connectUsers b owner (list1 (mem1^.userId) [extern, mem2^.userId])
    tid <- Util.createTeam g "foo" owner [mem2]

    WS.bracketRN c [owner, extern, mem1^.userId, mem2^.userId]  $ \ws@[wsOwner, wsExtern, wsMem1, wsMem2] -> do
        -- Managed conversation:
        cid1 <- Util.createManagedConv g owner (ConvTeamInfo tid True) [] (Just "gossip") Nothing Nothing
        checkConvCreateEvent cid1 wsOwner
        checkConvCreateEvent cid1 wsMem2

        -- Regular conversation:
        cid2 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [extern] (Just "blaa") Nothing Nothing
        checkConvCreateEvent cid2 wsOwner
        checkConvCreateEvent cid2 wsExtern
        -- mem2 is not a conversation member but still receives an event that
        -- a new team conversation has been created:
        checkTeamConvCreateEvent tid cid2 wsMem2

        Util.addTeamMember g owner tid mem1

        checkTeamMemberJoin tid (mem1^.userId) wsOwner
        checkTeamMemberJoin tid (mem1^.userId) wsMem1
        checkTeamMemberJoin tid (mem1^.userId) wsMem2

        -- New team members are added automatically to managed conversations ...
        Util.assertConvMember g (mem1^.userId) cid1
        -- ... but not to regular ones.
        Util.assertNotConvMember g (mem1^.userId) cid2

        -- Managed team conversations get all team members added implicitly.
        cid3 <- Util.createManagedConv g owner (ConvTeamInfo tid True) [] (Just "blup") Nothing Nothing
        for_ [owner, mem1^.userId, mem2^.userId] $ \u ->
            Util.assertConvMember g u cid3

        checkConvCreateEvent cid3 wsOwner
        checkConvCreateEvent cid3 wsMem1
        checkConvCreateEvent cid3 wsMem2

        -- Non team members are never added implicitly.
        for_ [cid1, cid3] $
            Util.assertNotConvMember g extern

        WS.assertNoEvent timeout ws

testAddManagedConv :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testAddManagedConv g b _c _ = do
    owner <- Util.randomUser b
    -- let p = Util.symmPermissions [CreateConversation]
    -- member <- flip newTeamMember p <$> Util.randomUser b
    -- Util.connectUsers b owner (singleton (member^.userId))
    tid <- Util.createTeam g "foo" owner []
    Util.createTeamConvAccessRaw g owner (ConvTeamInfo tid True) [] (Just "gossip") Nothing Nothing Nothing !!! do
        const 400 === statusCode
        const "no-managed-team-conv" === (Error.label . Util.decodeBody' "error label")

testAddTeamConvWithUsers :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testAddTeamConvWithUsers g b _ _ = do
    owner  <- Util.randomUser b
    extern <- Util.randomUser b
    Util.connectUsers b owner (list1 extern [])
    tid <- Util.createTeam g "foo" owner []
    -- Create managed team conversation and erroneously specify external users.
    cid <- Util.createManagedConv g owner (ConvTeamInfo tid True) [extern] (Just "gossip") Nothing Nothing
    -- External users have been ignored.
    Util.assertNotConvMember g extern cid
    -- Team members are present.
    Util.assertConvMember g owner cid

testAddTeamMemberToConv :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testAddTeamMemberToConv g b _ _ = do
    owner <- Util.randomUser b
    let p = Util.symmPermissions [AddConversationMember]
    mem1 <- flip newTeamMember p <$> Util.randomUser b
    mem2 <- flip newTeamMember p <$> Util.randomUser b
    mem3 <- flip newTeamMember (Util.symmPermissions []) <$> Util.randomUser b

    Util.connectUsers b owner (list1 (mem1^.userId) [mem2^.userId, mem3^.userId])
    tid <- Util.createTeam g "foo" owner [mem1, mem2, mem3]

    -- Team owner creates new regular team conversation:
    cid <- Util.createTeamConv g owner (ConvTeamInfo tid False) [] (Just "blaa") Nothing Nothing

    -- Team member 1 (who is *not* a member of the new conversation)
    -- can add other team members without requiring a user connection
    -- thanks to both being team members and member 1 having the permission
    -- `AddConversationMember`.
    Util.assertNotConvMember g (mem1^.userId) cid
    Util.postMembers g (mem1^.userId) (list1 (mem2^.userId) []) cid !!! const 200 === statusCode
    Util.assertConvMember g (mem2^.userId) cid

    -- OTOH, team member 3 can not add another team member since it
    -- lacks the required permission
    Util.assertNotConvMember g (mem3^.userId) cid
    Util.postMembers g (mem3^.userId) (list1 (mem1^.userId) []) cid !!! do
        const 403                === statusCode
        const "operation-denied" === (Error.label . Util.decodeBody' "error label")

testDeleteTeam :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testDeleteTeam g b c a = do
    owner <- Util.randomUser b
    let p = Util.symmPermissions [AddConversationMember]
    member <- flip newTeamMember p <$> Util.randomUser b
    extern <- Util.randomUser b
    Util.connectUsers b owner (list1 (member^.userId) [extern])

    tid  <- Util.createTeam g "foo" owner [member]
    cid1 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [] (Just "blaa") Nothing Nothing
    cid2 <- Util.createManagedConv g owner (ConvTeamInfo tid True) [] (Just "blup") Nothing Nothing

    Util.assertConvMember g owner cid2
    Util.assertConvMember g (member^.userId) cid2
    Util.assertNotConvMember g extern cid2

    Util.postMembers g owner (list1 extern []) cid1 !!! const 200 === statusCode
    Util.assertConvMember g owner cid1
    Util.assertConvMember g extern cid1
    Util.assertNotConvMember g (member^.userId) cid1

    void $ WS.bracketR3 c owner extern (member^.userId) $ \(wsOwner, wsExtern, wsMember) -> do
        delete (g . paths ["teams", toByteString' tid] . zUser owner . zConn "conn") !!!
            const 202 === statusCode
        checkTeamDeleteEvent tid wsOwner
        checkTeamDeleteEvent tid wsMember
        checkConvDeleteEvent cid1 wsExtern
        WS.assertNoEvent timeout [wsOwner, wsExtern, wsMember]

    get (g . paths ["teams", toByteString' tid] . zUser owner) !!!
        const 404 === statusCode

    get (g . paths ["teams", toByteString' tid, "members"] . zUser owner) !!!
        const 403 === statusCode

    get (g . paths ["teams", toByteString' tid, "conversations"] . zUser owner) !!!
        const 403 === statusCode

    for_ [owner, extern, member^.userId] $ \u -> do
        -- Ensure no user got deleted
        Util.ensureDeletedState b False owner u
        for_ [cid1, cid2] $ \x -> do
            Util.getConv g u x !!! const 404 === statusCode
            Util.getSelfMember g u x !!! do
                const 200         === statusCode
                const (Just Null) === Util.decodeBody
    assertQueueEmpty a

testDeleteBindingTeam :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testDeleteBindingTeam g b c a = do
    owner  <- Util.randomUser b
    tid    <- Util.createTeamInternal g "foo" owner
    assertQueue "create team" a tActivate
    let p1 = Util.symmPermissions [AddConversationMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    let p2 = Util.symmPermissions [AddConversationMember]
    mem2 <- flip newTeamMember p2 <$> Util.randomUser b
    let p3 = Util.symmPermissions [AddConversationMember]
    mem3 <- flip newTeamMember p3 <$> Util.randomUser b
    Util.addTeamMemberInternal g tid mem1
    assertQueue "team member join 2" a $ tUpdate 2 [owner]
    Util.addTeamMemberInternal g tid mem2
    assertQueue "team member join 3" a $ tUpdate 3 [owner]
    Util.addTeamMemberInternal g tid mem3
    assertQueue "team member join 4" a $ tUpdate 4 [owner]
    extern <- Util.randomUser b

    delete ( g
           . paths ["teams", toByteString' tid]
           . zUser owner
           . zConn "conn"
           . json (newTeamDeleteData (PlainTextPassword "wrong passwd"))
           ) !!! do
        const 403 === statusCode
        const "access-denied" === (Error.label . Util.decodeBody' "error label")

    delete ( g
           . paths ["teams", toByteString' tid, "members", toByteString' (mem3^.userId)]
           . zUser owner
           . zConn "conn"
           . json (newTeamMemberDeleteData (PlainTextPassword Util.defPassword))
           ) !!! const 202 === statusCode
    assertQueue "team member leave 1" a $ tUpdate 3 [owner]

    void $ WS.bracketRN c [owner, (mem1^.userId), (mem2^.userId), extern] $ \[wsOwner, wsMember1, wsMember2, wsExtern] -> do
        delete ( g
               . paths ["teams", toByteString' tid]
               . zUser owner
               . zConn "conn"
               . json (newTeamDeleteData (PlainTextPassword Util.defPassword))
               ) !!! const 202 === statusCode

        checkUserDeleteEvent owner wsOwner
        checkUserDeleteEvent (mem1^.userId) wsMember1
        checkUserDeleteEvent (mem2^.userId) wsMember2

        checkTeamDeleteEvent tid wsOwner
        checkTeamDeleteEvent tid wsMember1
        checkTeamDeleteEvent tid wsMember2

        WS.assertNoEvent (1 # Second) [wsExtern]
        -- Note that given the async nature of team deletion, we may
        -- have other events in the queue (such as TEAM_UPDATE)
        tryAssertQueue 10 "team delete, should be there" a tDelete

    forM_ [owner, (mem1^.userId), (mem2^.userId)] $
        -- Ensure users are marked as deleted; since we already
        -- received the event, should _really_ be deleted
        Util.ensureDeletedState b True extern

    -- Let's clean it up, just in case
    ensureQueueEmpty a

testDeleteTeamConv :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testDeleteTeamConv g b c _ = do
    owner <- Util.randomUser b
    let p = Util.symmPermissions [DeleteConversation]
    member <- flip newTeamMember p <$> Util.randomUser b
    extern <- Util.randomUser b
    Util.connectUsers b owner (list1 (member^.userId) [extern])

    tid  <- Util.createTeam g "foo" owner [member]
    cid1 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [] (Just "blaa") Nothing Nothing
    let access = ConversationAccessUpdate [InviteAccess, CodeAccess] ActivatedAccessRole
    putAccessUpdate g owner cid1 access !!! const 200 === statusCode
    code <- decodeConvCodeEvent <$> (postConvCode g owner cid1 <!! const 201 === statusCode)
    cid2 <- Util.createManagedConv g owner (ConvTeamInfo tid True) [] (Just "blup") Nothing Nothing

    Util.postMembers g owner (list1 extern [member^.userId]) cid1 !!! const 200 === statusCode

    for_ [owner, member^.userId, extern] $ \u -> Util.assertConvMember g u cid1
    for_ [owner, member^.userId]         $ \u -> Util.assertConvMember g u cid2

    WS.bracketR3 c owner extern (member^.userId) $ \(wsOwner, wsExtern, wsMember) -> do
        delete ( g
               . paths ["teams", toByteString' tid, "conversations", toByteString' cid2]
               . zUser (member^.userId)
               . zConn "conn"
               ) !!!  const 200 === statusCode

        checkTeamConvDeleteEvent tid cid2 wsOwner
        checkTeamConvDeleteEvent tid cid2 wsMember

        delete ( g
               . paths ["teams", toByteString' tid, "conversations", toByteString' cid1]
               . zUser (member^.userId)
               . zConn "conn"
               ) !!!  const 200 === statusCode

        checkTeamConvDeleteEvent tid cid1 wsOwner
        checkTeamConvDeleteEvent tid cid1 wsMember
        checkConvDeletevent cid1 wsExtern

        WS.assertNoEvent timeout [wsOwner, wsExtern, wsMember]

    for_ [cid1, cid2] $ \x ->
        for_ [owner, member^.userId, extern] $ \u -> do
            Util.getConv g u x !!! const 404 === statusCode
            Util.assertNotConvMember g u x

    postConvCodeCheck g code !!! const 404 === statusCode
  where
    checkTeamConvDeleteEvent tid cid w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= ConvDelete
        e^.eventTeam @?= tid
        e^.eventData @?= Just (EdConvDelete cid)

    checkConvDeletevent cid w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        evtType e @?= Conv.ConvDelete
        evtConv e @?= cid
        evtData e @?= Nothing

testUpdateTeam :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testUpdateTeam g b c _ = do
    owner <- Util.randomUser b
    let p = Util.symmPermissions [DeleteConversation]
    member <- flip newTeamMember p <$> Util.randomUser b
    Util.connectUsers b owner (list1 (member^.userId) [])
    tid <- Util.createTeam g "foo" owner [member]
    let bad = object ["name" .= T.replicate 100 "too large"]
    put ( g
        . paths ["teams", toByteString' tid]
        . zUser owner
        . zConn "conn"
        . json bad
        ) !!! const 400 === statusCode
    let u = newTeamUpdateData
          & nameUpdate .~ (Just $ unsafeRange "bar")
          & iconUpdate .~ (Just $ unsafeRange "xxx")
          & iconKeyUpdate .~ (Just $ unsafeRange "yyy")
    WS.bracketR2 c owner (member^.userId) $ \(wsOwner, wsMember) -> do
        put ( g
            . paths ["teams", toByteString' tid]
            . zUser owner
            . zConn "conn"
            . json u
            ) !!! const 200 === statusCode

        checkTeamUpdateEvent tid u wsOwner
        checkTeamUpdateEvent tid u wsMember
        WS.assertNoEvent timeout [wsOwner, wsMember]
  where
    checkTeamUpdateEvent tid upd w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= TeamUpdate
        e^.eventTeam @?= tid
        e^.eventData @?= Just (EdTeamUpdate upd)

testUpdateTeamMember :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testUpdateTeamMember g b c a = do
    owner <- Util.randomUser b
    let p = Util.symmPermissions [SetMemberPermissions]
    member <- flip newTeamMember p <$> Util.randomUser b
    Util.connectUsers b owner (list1 (member^.userId) [])
    tid <- Util.createTeam g "foo" owner [member]
    -- Must have at least 1 member with full permissions
    let changeOwner = newNewTeamMember (newTeamMember owner p)
    put ( g
        . paths ["teams", toByteString' tid, "members"]
        . zUser (member^.userId)
        . zConn "conn"
        . json changeOwner
        ) !!! do
        const 403 === statusCode
        const "no-other-owner" === (Error.label . Util.decodeBody' "error label")
    let changeMember = newNewTeamMember (member & permissions .~ fullPermissions)
    WS.bracketR2 c owner (member^.userId) $ \(wsOwner, wsMember) -> do
        put ( g
            . paths ["teams", toByteString' tid, "members"]
            . zUser owner
            . zConn "conn"
            . json changeMember
            ) !!! const 200 === statusCode
        member' <- Util.getTeamMember g owner tid (member^.userId)
        liftIO $ assertEqual "permissions" (member'^.permissions) (changeMember^.ntmNewTeamMember.permissions)
        checkTeamMemberUpdateEvent tid (member^.userId) wsOwner (pure fullPermissions)
        checkTeamMemberUpdateEvent tid (member^.userId) wsMember (pure fullPermissions)
        WS.assertNoEvent timeout [wsOwner, wsMember]
    -- Now that the other member has full permissions, it can demote the owner
    WS.bracketR2 c (member^.userId) owner $ \(wsMember, wsOwner) -> do
        put ( g
            . paths ["teams", toByteString' tid, "members"]
            . zUser (member^.userId)
            . zConn "conn"
            . json changeOwner
            ) !!! const 200 === statusCode
        owner' <- Util.getTeamMember g (member^.userId) tid owner
        liftIO $ assertEqual "permissions" (owner'^.permissions) (changeOwner^.ntmNewTeamMember.permissions)
        -- owner no longer has GetPermissions so can't see actual update
        checkTeamMemberUpdateEvent tid owner wsOwner Nothing
        checkTeamMemberUpdateEvent tid owner wsMember (pure p)
        WS.assertNoEvent timeout [wsOwner, wsMember]
    assertQueueEmpty a
  where
    checkTeamMemberUpdateEvent tid uid w mPerm = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= MemberUpdate
        e^.eventTeam @?= tid
        e^.eventData @?= Just (EdMemberUpdate uid mPerm)

testUpdateTeamStatus :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testUpdateTeamStatus g b _ a = do
    owner <- Util.randomUser b
    tid   <- Util.createTeamInternal g "foo" owner
    assertQueue "create team" a tActivate
    -- Check for idempotency
    Util.changeTeamStatus g tid Active
    assertQueueEmpty a
    Util.changeTeamStatus g tid Suspended
    assertQueue "suspend first time" a tSuspend
    Util.changeTeamStatus g tid Suspended
    assertQueueEmpty a
    Util.changeTeamStatus g tid Suspended
    assertQueueEmpty a
    Util.changeTeamStatus g tid Active
    assertQueue "activate again" a tActivate

    void $ put ( g
               . paths ["i", "teams", toByteString' tid, "status"]
               . json (TeamStatusUpdate Deleted Nothing)
               ) !!! do
        const 403 === statusCode
        const "invalid-team-status-update" === (Error.label . Util.decodeBody' "error label")

checkUserDeleteEvent :: UserId -> WS.WebSocket -> Http ()
checkUserDeleteEvent uid w = WS.assertMatch_ timeout w $ \notif -> do
    let j = Object $ List1.head (ntfPayload notif)
    let etype = j ^? key "type" . _String
    let euser = j ^? key "id" . _String
    etype @?= Just "user.delete"
    euser @?= Just (UUID.toText (toUUID uid))

checkTeamMemberJoin :: TeamId -> UserId -> WS.WebSocket -> Http ()
checkTeamMemberJoin tid uid w = WS.assertMatch_ timeout w $ \notif -> do
    ntfTransient notif @?= False
    let e = List1.head (WS.unpackPayload notif)
    e^.eventType @?= MemberJoin
    e^.eventTeam @?= tid
    e^.eventData @?= Just (EdMemberJoin uid)

checkTeamMemberLeave :: TeamId -> UserId -> WS.WebSocket -> Http ()
checkTeamMemberLeave tid usr w = WS.assertMatch_ timeout w $ \notif -> do
    ntfTransient notif @?= False
    let e = List1.head (WS.unpackPayload notif)
    e^.eventType @?= MemberLeave
    e^.eventTeam @?= tid
    e^.eventData @?= Just (EdMemberLeave usr)

checkTeamConvCreateEvent :: TeamId -> ConvId -> WS.WebSocket -> Http ()
checkTeamConvCreateEvent tid cid w = WS.assertMatch_ timeout w $ \notif -> do
    ntfTransient notif @?= False
    let e = List1.head (WS.unpackPayload notif)
    e^.eventType @?= ConvCreate
    e^.eventTeam @?= tid
    e^.eventData @?= Just (EdConvCreate cid)

checkConvCreateEvent :: ConvId -> WS.WebSocket -> Http ()
checkConvCreateEvent cid w = WS.assertMatch_ timeout w $ \notif -> do
    ntfTransient notif @?= False
    let e = List1.head (WS.unpackPayload notif)
    evtType e @?= Conv.ConvCreate
    case evtData e of
        Just (Conv.EdConversation x) -> cnvId x @?= cid
        other                        -> assertFailure $ "Unexpected event data: " <> show other

checkTeamDeleteEvent :: TeamId -> WS.WebSocket -> Http ()
checkTeamDeleteEvent tid w = WS.assertMatch_ timeout w $ \notif -> do
    ntfTransient notif @?= False
    let e = List1.head (WS.unpackPayload notif)
    e^.eventType @?= TeamDelete
    e^.eventTeam @?= tid
    e^.eventData @?= Nothing

checkConvDeleteEvent :: ConvId -> WS.WebSocket -> Http ()
checkConvDeleteEvent cid w = WS.assertMatch_ timeout w $ \notif -> do
    ntfTransient notif @?= False
    let e = List1.head (WS.unpackPayload notif)
    evtType e @?= Conv.ConvDelete
    evtConv e @?= cid
    evtData e @?= Nothing

checkConvMemberLeaveEvent :: ConvId -> UserId -> WS.WebSocket -> Http ()
checkConvMemberLeaveEvent cid usr w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        evtConv e @?= cid
        evtType e @?= Conv.MemberLeave
        case evtData e of
            Just (Conv.EdMembers mm) -> mm @?= Conv.Members [usr]
            other                    -> assertFailure $ "Unexpected event data: " <> show other

postCryptoBroadcastMessageJson :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
postCryptoBroadcastMessageJson g b c a = do
    -- Team1: Alice, Bob. Team2: Charlie. Regular user: Dan. Connect Alice,Charlie,Dan
    (alice,  ac) <- randomUserWithClient b (someLastPrekeys !! 0)
    (bob,    bc) <- randomUserWithClient b (someLastPrekeys !! 1)
    (charlie,cc) <- randomUserWithClient b (someLastPrekeys !! 2)
    (dan,    dc) <- randomUserWithClient b (someLastPrekeys !! 3)
    connectUsers b alice (list1 charlie [dan])
    tid1 <- createTeamInternal g "foo" alice
    assertQueue "" a tActivate
    addTeamMemberInternal g tid1 $ newTeamMember bob (symmPermissions [])
    assertQueue "" a $ tUpdate 2 [alice]
    _ <- createTeamInternal g "foo" charlie
    assertQueue "" a tActivate
    -- A second client for Alice
    ac2 <- randomClient b alice (someLastPrekeys !! 4)
    -- Complete: Alice broadcasts a message to Bob,Charlie,Dan and herself
    let t = 1 # Second -- WS receive timeout
    let msg = [(alice, ac2, "ciphertext0"), (bob, bc, "ciphertext1"), (charlie, cc, "ciphertext2"), (dan, dc, "ciphertext3")]
    WS.bracketRN c [bob, charlie, dan] $ \[wsB, wsC, wsD] ->
        -- Alice's clients 1 and 2 listen to their own messages only
        WS.bracketR (c . queryItem "client" (toByteString' ac2)) alice $ \wsA2 ->
            WS.bracketR (c . queryItem "client" (toByteString' ac)) alice $ \wsA1 -> do
                Util.postOtrBroadcastMessage id g alice ac msg !!! do
                    const 201 === statusCode
                    assertTrue_ (eqMismatch [] [] [] . decodeBody)
                -- Bob should get the broadcast (team member of alice)
                void . liftIO $ WS.assertMatch t wsB (wsAssertOtr (selfConv bob) alice ac bc "ciphertext1")
                -- Charlie should get the broadcast (contact of alice and user of teams feature)
                void . liftIO $ WS.assertMatch t wsC (wsAssertOtr (selfConv charlie) alice ac cc "ciphertext2")
                -- Dan should get the broadcast (contact of alice and not user of teams feature)
                void . liftIO $ WS.assertMatch t wsD (wsAssertOtr (selfConv dan) alice ac dc "ciphertext3")
                -- Alice's first client should not get the broadcast
                assertNoMsg wsA1 (wsAssertOtr (selfConv alice) alice ac ac "ciphertext0")
                -- Alice's second client should get the broadcast
                void . liftIO $ WS.assertMatch t wsA2 (wsAssertOtr (selfConv alice) alice ac ac2 "ciphertext0")

postCryptoBroadcastMessageJson2 :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
postCryptoBroadcastMessageJson2 g b c a = do
    -- Team1: Alice, Bob. Team2: Charlie. Connect Alice,Charlie
    (alice,  ac) <- randomUserWithClient b (someLastPrekeys !! 0)
    (bob,    bc) <- randomUserWithClient b (someLastPrekeys !! 1)
    (charlie,cc) <- randomUserWithClient b (someLastPrekeys !! 2)
    connectUsers b alice (list1 charlie [])
    tid1 <- createTeamInternal g "foo" alice
    assertQueue "" a tActivate
    addTeamMemberInternal g tid1 $ newTeamMember bob (symmPermissions [])
    assertQueue "" a $ tUpdate 2 [alice]

    let t = 3 # Second -- WS receive timeout
    -- Missing charlie
    let m1 = [(bob, bc, "ciphertext1")]
    Util.postOtrBroadcastMessage id g alice ac m1 !!! do
        const 412 === statusCode
        assertTrue "1: Only Charlie and his device" (eqMismatch [(charlie, Set.singleton cc)] [] [] . decodeBody)

    -- Complete
    WS.bracketR2 c bob charlie $ \(wsB, wsE) -> do
        let m2 = [(bob, bc, "ciphertext2"), (charlie, cc, "ciphertext2")]
        Util.postOtrBroadcastMessage id g alice ac m2 !!! do
            const 201 === statusCode
            assertTrue "No devices expected" (eqMismatch [] [] [] . decodeBody)
        void . liftIO $ WS.assertMatch t wsB (wsAssertOtr (selfConv bob) alice ac bc "ciphertext2")
        void . liftIO $ WS.assertMatch t wsE (wsAssertOtr (selfConv charlie) alice ac cc "ciphertext2")

    -- Redundant self
    WS.bracketR3 c alice bob charlie $ \(wsA, wsB, wsE) -> do
        let m3 = [(alice, ac, "ciphertext3"), (bob, bc, "ciphertext3"), (charlie, cc, "ciphertext3")]
        Util.postOtrBroadcastMessage id g alice ac m3 !!! do
            const 201 === statusCode
            assertTrue "2: Only Alice and her device" (eqMismatch [] [(alice, Set.singleton ac)] [] . decodeBody)
        void . liftIO $ WS.assertMatch t wsB (wsAssertOtr (selfConv bob) alice ac bc "ciphertext3")
        void . liftIO $ WS.assertMatch t wsE (wsAssertOtr (selfConv charlie) alice ac cc "ciphertext3")
        -- Alice should not get it
        assertNoMsg wsA (wsAssertOtr (selfConv alice) alice ac ac "ciphertext3")

    -- Deleted charlie
    WS.bracketR2 c bob charlie $ \(wsB, wsE) -> do
        deleteClient b charlie cc (Just $ PlainTextPassword defPassword) !!! const 200 === statusCode
        let m4 = [(bob, bc, "ciphertext4"), (charlie, cc, "ciphertext4")]
        Util.postOtrBroadcastMessage id g alice ac m4 !!! do
            const 201 === statusCode
            assertTrue "3: Only Charlie and his device" (eqMismatch [] [] [(charlie, Set.singleton cc)] . decodeBody)
        void . liftIO $ WS.assertMatch t wsB (wsAssertOtr (selfConv bob) alice ac bc "ciphertext4")
        -- charlie should not get it
        assertNoMsg wsE (wsAssertOtr (selfConv charlie) alice ac cc "ciphertext4")

postCryptoBroadcastMessageProto :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
postCryptoBroadcastMessageProto g b c a = do
    -- similar to postCryptoBroadcastMessageJson except uses protobuf

    -- Team1: Alice, Bob. Team2: Charlie. Regular user: Dan. Connect Alice,Charlie,Dan
    (alice,  ac) <- randomUserWithClient b (someLastPrekeys !! 0)
    (bob,    bc) <- randomUserWithClient b (someLastPrekeys !! 1)
    (charlie,cc) <- randomUserWithClient b (someLastPrekeys !! 2)
    (dan,    dc) <- randomUserWithClient b (someLastPrekeys !! 3)
    connectUsers b alice (list1 charlie [dan])
    tid1 <- createTeamInternal g "foo" alice
    assertQueue "" a tActivate
    addTeamMemberInternal g tid1 $ newTeamMember bob (symmPermissions [])
    assertQueue "" a $ tUpdate 2 [alice]
    _ <- createTeamInternal g "foo" charlie
    assertQueue "" a tActivate
    -- Complete: Alice broadcasts a message to Bob,Charlie,Dan
    let t = 1 # Second -- WS receive timeout
    let ciphertext = encodeCiphertext "hello bob"
    WS.bracketRN c [alice, bob, charlie, dan] $ \ws@[_, wsB, wsC, wsD] -> do
        let msg = otrRecipients [(bob, [(bc, ciphertext)]), (charlie, [(cc, ciphertext)]), (dan, [(dc, ciphertext)])]
        Util.postProtoOtrBroadcast g alice ac msg !!! do
            const 201 === statusCode
            assertTrue_ (eqMismatch [] [] [] . decodeBody)
        -- Bob should get the broadcast (team member of alice)
        void . liftIO $ WS.assertMatch t wsB (wsAssertOtr' (encodeCiphertext "data") (selfConv bob) alice ac bc ciphertext)
        -- Charlie should get the broadcast (contact of alice and user of teams feature)
        void . liftIO $ WS.assertMatch t wsC (wsAssertOtr' (encodeCiphertext "data") (selfConv charlie) alice ac cc ciphertext)
        -- Dan should get the broadcast (contact of alice and not user of teams feature)
        void . liftIO $ WS.assertMatch t wsD (wsAssertOtr' (encodeCiphertext "data") (selfConv dan) alice ac dc ciphertext)
        -- Alice should not get her own broadcast
        WS.assertNoEvent timeout ws

postCryptoBroadcastMessageNoTeam :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
postCryptoBroadcastMessageNoTeam g b _ _ = do
    (alice, ac) <- randomUserWithClient b (someLastPrekeys !! 0)
    (bob,   bc) <- randomUserWithClient b (someLastPrekeys !! 1)
    connectUsers b alice (list1 bob [])
    let msg = [(bob, bc, "ciphertext1")]
    Util.postOtrBroadcastMessage id g alice ac msg !!! const 404 === statusCode

postCryptoBroadcastMessage100OrMaxConns :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
postCryptoBroadcastMessage100OrMaxConns g b c a = do
    (alice, ac) <- randomUserWithClient b (someLastPrekeys !! 0)
    _ <- createTeamInternal g "foo" alice
    assertQueue "" a tActivate
    ((bob, bc), others) <- createAndConnectUserWhileLimitNotReached alice (100 :: Int) [] (someLastPrekeys !! 1)
    connectUsers b alice (list1 bob (fst <$> others))
    let t = 3 # Second -- WS receive timeout
    WS.bracketRN c (bob : (fst <$> others)) $ \ws -> do
        let f (u, clt) = (u, clt, "ciphertext")
        let msg = (bob, bc, "ciphertext") : (f <$> others)
        Util.postOtrBroadcastMessage id g alice ac msg !!! do
            const 201 === statusCode
            assertTrue_ (eqMismatch [] [] [] . decodeBody)
        void . liftIO $ WS.assertMatch t (Prelude.head ws) (wsAssertOtr (selfConv bob) alice ac bc "ciphertext")
        for_ (zip (tail ws) others) $ \(wsU, (u, clt)) ->
            liftIO $ WS.assertMatch t wsU (wsAssertOtr (selfConv u) alice ac clt "ciphertext")
  where
    createAndConnectUserWhileLimitNotReached alice remaining acc pk = do
        (uid, cid) <- randomUserWithClient b pk
        (r1, r2)   <- List1.head <$> connectUsersUnchecked b alice (singleton uid)
        case (statusCode r1, statusCode r2, remaining, acc) of
            (201, 200, 0, []    ) -> error "Need to connect with at least 1 user"
            (201, 200, 0, (x:xs)) -> return (x, xs)
            (201, 200, _, _     ) -> createAndConnectUserWhileLimitNotReached alice (remaining-1) ((uid ,cid):acc) pk
            (403, 403, _, []    ) -> error "Need to connect with at least 1 user"
            (403, 403, _, (x:xs)) -> return (x, xs)
            (xxx, yyy, _, _     ) -> error ("Unexpected while connecting users: " ++ show xxx ++ " and " ++ show yyy)
