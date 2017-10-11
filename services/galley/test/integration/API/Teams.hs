{-# LANGUAGE OverloadedStrings #-}

module API.Teams (tests) where

import API.Util (Galley, Brig, test, zUser, zConn)
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens hiding ((#), (.=))
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Retry
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Foldable (for_)
import Data.Id
import Data.List1
import Data.Misc (PlainTextPassword (..))
import Data.Monoid
import Data.Range
import Galley.Types hiding (EventType (..), EventData (..), MemberUpdate (..))
import Galley.Types.Teams
import Gundeck.Types.Notification
import Test.Tasty
import Test.Tasty.Cannon (Cannon, TimeoutUnit (..), (#))
import Test.Tasty.HUnit
import API.SQS

import qualified API.Util as Util
import qualified Data.List1 as List1
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Galley.Types as Conv
import qualified Network.Wai.Utilities.Error as Error
import qualified Test.Tasty.Cannon as WS
import qualified Galley.Aws as Aws

tests :: Galley -> Brig -> Cannon -> Manager -> Maybe Aws.Env -> TestTree
tests g b c m a = testGroup "Teams API"
    [ test m "create team" (testCreateTeam g b c a)
    , test m "create multiple binding teams fail" (testCreateMulitpleBindingTeams g b a)
    , test m "create team with members" (testCreateTeamWithMembers g b c)
    , test m "create 1-1 conversation between binding team members (fail)" (testCreateOne2OneFailNonBindingTeamMembers g b a)
    , test m "create 1-1 conversation between binding team members" (testCreateOne2OneWithMembers g b c a)
    , test m "add new team member" (testAddTeamMember g b c)
    , test m "add new team member binding teams" (testAddTeamMemberCheckBound g b a)
    , test m "add new team member internal" (testAddTeamMemberInternal g b c a)
    , test m "remove team member" (testRemoveTeamMember g b c)
    , test m "remove team member (binding)" (testRemoveBindingTeamMember g b c a)
    , test m "add team conversation" (testAddTeamConv g b c)
    , test m "add managed team conversation ignores given users" (testAddTeamConvWithUsers g b)
    , test m "add team member to conversation without connection" (testAddTeamMemberToConv g b)
    , test m "delete non-binding team" (testDeleteTeam g b c a)
    , test m "delete binding team" (testDeleteBindingTeam g b c a)
    , test m "delete team conversation" (testDeleteTeamConv g b c)
    , test m "update team data" (testUpdateTeam g b c)
    , test m "update team member" (testUpdateTeamMember g b c a)
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

testCreateMulitpleBindingTeams :: Galley -> Brig -> Maybe Aws.Env -> Http ()
testCreateMulitpleBindingTeams g b a = do
    owner <- Util.randomUser b
    _     <- Util.createTeamInternal g "foo" owner
    assertQueue a tCreate
    -- Cannot create more teams if bound (used internal API)
    let nt = NonBindingNewTeam $ newNewTeam (unsafeRange "owner") (unsafeRange "icon")
    post (g . path "/teams" . zUser owner . zConn "conn" . json nt) !!!
        const 403 === statusCode

    -- If never used the internal API, can create multiple teams
    owner' <- Util.randomUser b
    void $ Util.createTeam g "foo" owner' []
    void $ Util.createTeam g "foo" owner' []

testCreateTeamWithMembers :: Galley -> Brig -> Cannon -> Http ()
testCreateTeamWithMembers g b c = do
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

testCreateOne2OneFailNonBindingTeamMembers :: Galley -> Brig -> Maybe Aws.Env -> Http ()
testCreateOne2OneFailNonBindingTeamMembers g b a = do
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
    assertQueue a tCreate
    owner2 <- Util.randomUser b
    void $ Util.createTeamInternal g "foo" owner2
    assertQueue a tCreate
    Util.createOne2OneTeamConv g owner1 owner2 Nothing tid1 !!! do
        const 403 === statusCode
        const "non-binding-team-members" === (Error.label . Util.decodeBody' "error label")

testCreateOne2OneWithMembers :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testCreateOne2OneWithMembers g b c a = do
    owner <- Util.randomUser b
    tid   <- Util.createTeamInternal g "foo" owner
    assertQueue a tCreate
    let p1 = Util.symmPermissions [CreateConversation]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b

    WS.bracketR c (mem1^.userId) $ \wsMem1 -> do
        Util.addTeamMemberInternal g tid mem1
        checkTeamMemberJoin tid (mem1^.userId) wsMem1
        assertQueue a $ tUpdate 2 [owner]
        WS.assertNoEvent timeout [wsMem1]

    void $ retryWhileN 10 repeatIf (Util.createOne2OneTeamConv g owner (mem1^.userId) Nothing tid)

    -- Recreating a One2One is a no-op, returns a 200
    Util.createOne2OneTeamConv g owner (mem1^.userId) Nothing tid !!! const 200 === statusCode
  where
    repeatIf :: Util.ResponseLBS -> Bool
    repeatIf r = statusCode r /= 201

    retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
    retryWhileN n f m = retrying (constantDelay 1000000 <> limitRetries n)
                                 (const (return . f))
                                 (const m)

testAddTeamMember :: Galley -> Brig -> Cannon -> Http ()
testAddTeamMember g b c = do
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
        liftIO . void $ mapConcurrently (checkJoinEvent tid (mem3^.userId)) [wsOwner, wsMem1, wsMem2, wsMem3]
  where
    checkJoinEvent tid usr w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= MemberJoin
        e^.eventTeam @?= tid
        e^.eventData @?= Just (EdMemberJoin usr)

testAddTeamMemberCheckBound :: Galley -> Brig -> Maybe Aws.Env -> Http ()
testAddTeamMemberCheckBound g b a = do
    ownerBound <- Util.randomUser b
    tidBound   <- Util.createTeamInternal g "foo" ownerBound
    assertQueue a tCreate

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
        assertQueue a $ tUpdate 2 [owner]
    void $ Util.getTeamMemberInternal g tid (mem1^.userId)
  where
    checkJoinEvent tid usr w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= MemberJoin
        e^.eventTeam @?= tid
        e^.eventData @?= Just (EdMemberJoin usr)

testRemoveTeamMember :: Galley -> Brig -> Cannon -> Http ()
testRemoveTeamMember g b c = do
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
    void $ Util.createTeamConv g owner (ConvTeamInfo tid True) [] (Just "gossip")
    -- Regular conversation:
    cid2 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [mem1^.userId, mem2^.userId, mext1] (Just "blaa")
    -- Member external 2 is a guest and not a part of any conversation that mem1 is a part of
    void $ Util.createTeamConv g owner (ConvTeamInfo tid False) [mem2^.userId, mext2] (Just "blaa")
    -- Member external 3 is a guest and part of a conversation that mem1 is a part of
    cid3 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [mem1^.userId, mext3] (Just "blaa")

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

        liftIO . void $ mapConcurrently (checkLeaveEvent tid (mem1^.userId)) [wsOwner, wsMem1, wsMem2]
        checkConvMemberLeaveEvent cid2 (mem1^.userId) wsMext1
        checkConvMemberLeaveEvent cid3 (mem1^.userId) wsMext3
        WS.assertNoEvent timeout ws
  where
    checkLeaveEvent tid usr w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= MemberLeave
        e^.eventTeam @?= tid
        e^.eventData @?= Just (EdMemberLeave usr)

testRemoveBindingTeamMember :: Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http ()
testRemoveBindingTeamMember g b c a = do
    owner <- Util.randomUser b
    tid   <- Util.createTeamInternal g "foo" owner
    assertQueue a tCreate
    mext  <- Util.randomUser b
    let p1 = Util.symmPermissions [AddConversationMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    Util.addTeamMemberInternal g tid mem1
    assertQueue a $ tUpdate 2 [owner]
    Util.connectUsers b owner (singleton mext)
    cid1 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [(mem1^.userId), mext] (Just "blaa")

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

    WS.bracketR c mext $ \wsMext -> do
        delete ( g
               . paths ["teams", toByteString' tid, "members", toByteString' (mem1^.userId)]
               . zUser owner
               . zConn "conn"
               . json (newTeamMemberDeleteData (PlainTextPassword Util.defPassword))
               ) !!! const 202 === statusCode

        checkConvMemberLeaveEvent cid1 (mem1^.userId) wsMext
        assertQueue a $ tUpdate 1 [owner]
        WS.assertNoEvent timeout [wsMext]
        -- Mem1 is now gone from Wire
        Util.ensureDeletedState b True owner (mem1^.userId)

testAddTeamConv :: Galley -> Brig -> Cannon -> Http ()
testAddTeamConv g b c = do
    owner  <- Util.randomUser b
    extern <- Util.randomUser b

    let p = Util.symmPermissions [CreateConversation, AddConversationMember]
    mem1 <- flip newTeamMember p <$> Util.randomUser b
    mem2 <- flip newTeamMember p <$> Util.randomUser b

    Util.connectUsers b owner (list1 (mem1^.userId) [extern, mem2^.userId])
    tid <- Util.createTeam g "foo" owner [mem2]

    WS.bracketRN c [owner, extern, mem1^.userId, mem2^.userId]  $ \ws@[wsOwner, wsExtern, wsMem1, wsMem2] -> do
        -- Managed conversation:
        cid1 <- Util.createTeamConv g owner (ConvTeamInfo tid True) [] (Just "gossip")
        checkConvCreateEvent cid1 wsOwner
        checkConvCreateEvent cid1 wsMem2

        -- Regular conversation:
        cid2 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [extern] (Just "blaa")
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
        cid3 <- Util.createTeamConv g owner (ConvTeamInfo tid True) [] (Just "blup")
        for_ [owner, mem1^.userId, mem2^.userId] $ \u ->
            Util.assertConvMember g u cid3

        checkConvCreateEvent cid3 wsOwner
        checkConvCreateEvent cid3 wsMem1
        checkConvCreateEvent cid3 wsMem2

        -- Non team members are never added implicitly.
        for_ [cid1, cid3] $
            Util.assertNotConvMember g extern

        WS.assertNoEvent timeout ws
  where
    checkTeamConvCreateEvent tid cid w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= ConvCreate
        e^.eventTeam @?= tid
        e^.eventData @?= Just (EdConvCreate cid)

    checkConvCreateEvent cid w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        evtType e @?= Conv.ConvCreate
        case evtData e of
            Just (Conv.EdConversation x) -> cnvId x @?= cid
            other                        -> assertFailure $ "Unexpected event data: " <> show other

testAddTeamConvWithUsers :: Galley -> Brig -> Http ()
testAddTeamConvWithUsers g b = do
    owner  <- Util.randomUser b
    extern <- Util.randomUser b
    Util.connectUsers b owner (list1 extern [])
    tid <- Util.createTeam g "foo" owner []
    -- Create managed team conversation and erroneously specify external users.
    cid <- Util.createTeamConv g owner (ConvTeamInfo tid True) [extern] (Just "gossip")
    -- External users have been ignored.
    Util.assertNotConvMember g extern cid
    -- Team members are present.
    Util.assertConvMember g owner cid

testAddTeamMemberToConv :: Galley -> Brig -> Http ()
testAddTeamMemberToConv g b = do
    owner <- Util.randomUser b
    let p = Util.symmPermissions [AddConversationMember]
    mem1 <- flip newTeamMember p <$> Util.randomUser b
    mem2 <- flip newTeamMember p <$> Util.randomUser b
    mem3 <- flip newTeamMember (Util.symmPermissions []) <$> Util.randomUser b

    Util.connectUsers b owner (list1 (mem1^.userId) [mem2^.userId, mem3^.userId])
    tid <- Util.createTeam g "foo" owner [mem1, mem2, mem3]

    -- Team owner creates new regular team conversation:
    cid <- Util.createTeamConv g owner (ConvTeamInfo tid False) [] (Just "blaa")

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
    cid1 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [] (Just "blaa")
    cid2 <- Util.createTeamConv g owner (ConvTeamInfo tid True) [] (Just "blup")

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
    assertQueue a tCreate
    let p1 = Util.symmPermissions [AddConversationMember]
    mem1 <- flip newTeamMember p1 <$> Util.randomUser b
    Util.addTeamMemberInternal g tid mem1
    assertQueue a $ tUpdate 2 [owner]
    extern <- Util.randomUser b

    delete ( g
           . paths ["teams", toByteString' tid]
           . zUser owner
           . zConn "conn"
           . json (newTeamDeleteData (PlainTextPassword "wrong passwd"))
           ) !!! do
        const 403 === statusCode
        const "access-denied" === (Error.label . Util.decodeBody' "error label")

    void $ WS.bracketR3 c owner (mem1^.userId) extern $ \(wsOwner, wsMember, wsExtern) -> do
        delete ( g
               . paths ["teams", toByteString' tid]
               . zUser owner
               . zConn "conn"
               . json (newTeamDeleteData (PlainTextPassword Util.defPassword))
               ) !!! const 202 === statusCode
        checkTeamDeleteEvent tid wsOwner
        checkTeamDeleteEvent tid wsMember
        -- TODO: Due to the async nature of the deletion, we should actually check for
        --       the user deletion event to avoid race conditions at this point
        WS.assertNoEvent timeout [wsExtern]
        assertQueue a tDelete

    mapM_ (Util.ensureDeletedState b True extern) [owner, (mem1^.userId)]

testDeleteTeamConv :: Galley -> Brig -> Cannon -> Http ()
testDeleteTeamConv g b c = do
    owner <- Util.randomUser b
    let p = Util.symmPermissions [DeleteConversation]
    member <- flip newTeamMember p <$> Util.randomUser b
    extern <- Util.randomUser b
    Util.connectUsers b owner (list1 (member^.userId) [extern])

    tid  <- Util.createTeam g "foo" owner [member]
    cid1 <- Util.createTeamConv g owner (ConvTeamInfo tid False) [] (Just "blaa")
    cid2 <- Util.createTeamConv g owner (ConvTeamInfo tid True) [] (Just "blup")

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

testUpdateTeam :: Galley -> Brig -> Cannon -> Http ()
testUpdateTeam g b c = do
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
        checkTeamMemberUpdateEvent tid (member^.userId) wsOwner
        checkTeamMemberUpdateEvent tid (member^.userId) wsMember
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
        checkTeamMemberUpdateEvent tid owner wsOwner
        checkTeamMemberUpdateEvent tid owner wsMember
        WS.assertNoEvent timeout [wsOwner, wsMember]
    assertQueueEmpty a
  where
    checkTeamMemberUpdateEvent tid uid w = WS.assertMatch_ timeout w $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e^.eventType @?= MemberUpdate
        e^.eventTeam @?= tid
        e^.eventData @?= Just (EdMemberUpdate uid)

checkTeamMemberJoin :: TeamId -> UserId -> WS.WebSocket -> Http ()
checkTeamMemberJoin tid uid w = WS.assertMatch_ timeout w $ \notif -> do
    ntfTransient notif @?= False
    let e = List1.head (WS.unpackPayload notif)
    e^.eventType @?= MemberJoin
    e^.eventTeam @?= tid
    e^.eventData @?= Just (EdMemberJoin uid)

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

